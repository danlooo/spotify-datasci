#!/usr/bin/env R

#
# R targets pipeline
#

library(tidyverse)
library(ggnewscale)
library(ggrepel)
library(ggbeeswarm)
library(ggpubr)

library(broom)
library(targets)
library(spotifyr)
library(reticulate)
library(keras)
library(tensorflow)

keras::use_condaenv("datasci", conda = "/home/daniel/miniconda3/bin/conda")
options(clustermq.scheduler = "multicore")
safely(get_spotify_access_token)()
theme_set(theme_minimal())

list(
  tar_target(selected_audio_features, c(
    "danceability", "energy", "key", "mode", "valence", "loudness",
    "speechiness", "acousticness", "instrumentalness", "liveness"
  )),
  tar_target(terms, c("techno", "rock", "jazz", "classical")),
  tar_target(track_searches_offsets, seq(0, 150, by = 50)),
  tar_target(
    name = track_searches,
    command = {
      tibble(term = terms) |>
        expand_grid(offset = track_searches_offsets) |>
        mutate(data = map2(term, offset, ~ search_spotify(.x, type = "track", limit = 50, offset = .y))) |>
        unnest(data)
    },
    pattern = cross(terms, track_searches_offsets)
  ),
  tar_target(track_ids, track_searches$id),
  tar_target(track_train_test_split, {
    train_ids <-
      track_searches |>
      group_by(term) |>
      sample_frac(0.8) |>
      pull(id)

    tibble(id = track_searches$id) |>
      mutate(is_train = id %in% train_ids)
  }),
  tar_target(track_train_test_split_file, {
    track_train_test_split |> write_csv("tmp/track_train_test_split.csv")
  }),
  tar_target(
    name = track_audio_analyses,
    command = tibble(id = track_ids, audio_analysis = track_ids |> possibly(get_track_audio_analysis, NA)() |> list()),
    pattern = map(track_ids) # only one track per target
  ),
  tar_target(track_audio_features, get_track_audio_features(track_searches$id), pattern = map(track_searches)), # can be done in batches
  tar_target(track_features_file, {
    track_audio_features |>
      left_join(track_searches, by = "id") |>
      write_csv("tmp/track_features.csv")
  }),
  tar_target(track_pitches,
    {
      # crop or loop to this many segments
      n_segments <- 800

      track_audio_analyses |>
        transmute(
          id,
          pitches = audio_analysis |> map(possibly(~ {
            df <-
              .x$segments$pitches |>
              as.data.frame() |>
              t() |>
              as_tibble()

            # reshape to fixed number of samples: first n, loop data if needed
            rep(list(df), ceiling(n_segments / nrow(df))) |>
              bind_rows() |>
              head(n_segments)
          }, NA))
        )
    },
    pattern = map(track_audio_analyses)
  ),
  tar_target(valid_tracks, {
    ambigious_tracks <-
      track_searches |>
      count(id) |>
      filter(n > 1)
    
    # tracks having all features available
    track_audio_analyses |>
      filter(!is.na(audio_analysis)) |>
      inner_join(track_audio_features) |>
      anti_join(ambigious_tracks) |>
      pull(id) |>
      unique()
  }),
  tar_target(model_data, {
    train_samples <-
      track_train_test_split |>
      filter(id %in% valid_tracks) |>
      filter(is_train) |>
      pull(id) |>
      unique()

    x_long <-
      track_pitches |>
      filter(id %in% train_samples) |>
      unnest(pitches) |>
      group_by(id) |>
      mutate(step = row_number()) |>
      pivot_longer(-c(id, step)) |>
      filter(step <= 800)

    x_array <-
      x_long |>
      pull(value) |>
      array(dim = c(
        x_long$id |> unique() |> length(),
        x_long$step |> unique() |> length(),
        x_long$name |> unique() |> length()
      ))

    y <-
      tibble(id = x_long$id |> unique()) |>
      filter(id %in% train_samples) |>
      left_join(track_searches) |>
      select(id, term) |>
      mutate(yes = 1) |>
      complete(id, term, fill = list(yes = 0)) |>
      
      # normalization due to multiple mentioning
      group_by(id) |>
      mutate(yes = yes / sum(yes)) |>
      ungroup() |>
      
      pivot_wider(names_from = term, values_from = yes) |>
      column_to_rownames("id") |>
      as.matrix()

    list(train_x = x_array, train_y = y)
  }),
  tar_target(model_archs, {
    list(
      "base" = function() {
        keras_model_sequential(input_shape = dim(model_data$train_x)[2:3]) |>
          layer_flatten() |>
          layer_dense(units = 3, activation = "relu") |>
          layer_dense(units = ncol(model_data$train_y), activation = "softmax")
      },
      "lstm1" = function() {
        keras_model_sequential(input_shape = dim(model_data$train_x)[2:3]) |>
          layer_lstm(units = 64) |>
          layer_dense(units = ncol(model_data$train_y), activation = "softmax")
      },
      "cnn1" = function() {
        keras_model_sequential(input_shape = dim(model_data$train_x)[2:3]) |>
          layer_conv_1d(filters = 64, kernel_size = 5) |>
          layer_activation_relu() |>
          layer_batch_normalization() |>
          layer_global_average_pooling_1d() |>
          layer_dense(units = 32) |>
          layer_dense(units = ncol(model_data$train_y), activation = "softmax")
      },
      "cnn2" = function() {
        keras_model_sequential(input_shape = dim(model_data$train_x)[2:3]) |>
          layer_conv_1d(filters = 512, kernel_size = 5) |>
          layer_activation_relu() |>
          layer_batch_normalization() |>
          layer_conv_1d(filters = 64, kernel_size = 5) |>
          layer_activation_relu() |>
          layer_batch_normalization() |>
          layer_global_average_pooling_1d() |>
          layer_dense(units = 32) |>
          layer_dense(units = ncol(model_data$train_y), activation = "softmax")
      },
      "cnn3" = function() {
        keras_model_sequential(input_shape = dim(model_data$train_x)[2:3]) |>
          layer_conv_1d(filters = 16, kernel_size = 3) |>
          layer_activation_relu() |>
          layer_batch_normalization() |>
          layer_global_average_pooling_1d() |>
          layer_dense(units = ncol(model_data$train_y), activation = "softmax")
      },
      "cnn4" = function() {
        keras_model_sequential(input_shape = dim(model_data$train_x)[2:3]) |>
          layer_conv_1d(filters = 16, kernel_size = 3) |>
          layer_activation_relu() |>
          layer_batch_normalization() |>
          layer_conv_1d(filters = 16, kernel_size = 3) |>
          layer_activation_relu() |>
          layer_batch_normalization() |>
          layer_global_average_pooling_1d() |>
          layer_dense(units = ncol(model_data$train_y), activation = "softmax")
      },
      "cnn5" = function() {
        keras_model_sequential(input_shape = dim(model_data$train_x)[2:3]) |>
          layer_conv_1d(filters = 16, kernel_size = 65) |>
          layer_activation_relu() |>
          layer_batch_normalization() |>
          layer_conv_1d(filters = 16, kernel_size = 65) |>
          layer_activation_relu() |>
          layer_batch_normalization() |>
          layer_global_average_pooling_1d() |>
          layer_dense(units = ncol(model_data$train_y), activation = "softmax")
      },
      "cnn6" = function() {
        keras_model_sequential(input_shape = dim(model_data$train_x)[2:3]) |>
          layer_conv_1d(filters = 32, kernel_size = 3) |>
          layer_activation_relu() |>
          layer_batch_normalization() |>
          layer_conv_1d(filters = 32, kernel_size = 3) |>
          layer_activation_relu() |>
          layer_batch_normalization() |>
          layer_global_average_pooling_1d() |>
          layer_dense(units = ncol(model_data$train_y), activation = "softmax")
      },
      "cnn7" = function() {
        keras_model_sequential(input_shape = dim(model_data$train_x)[2:3]) |>
          layer_conv_1d(filters = 8, kernel_size = 3) |>
          layer_activation_relu() |>
          layer_batch_normalization() |>
          layer_conv_1d(filters = 8, kernel_size = 3) |>
          layer_activation_relu() |>
          layer_batch_normalization() |>
          layer_global_average_pooling_1d() |>
          layer_dense(units = ncol(model_data$train_y), activation = "softmax")
      },
      "cnn8" = function() {
        keras_model_sequential(input_shape = dim(model_data$train_x)[2:3]) |>
          layer_conv_1d(filters = 16, kernel_size = 3) |>
          layer_activation_relu() |>
          layer_batch_normalization() |>
          layer_conv_1d(filters = 16, kernel_size = 3) |>
          layer_activation_relu() |>
          layer_batch_normalization() |>
          layer_global_average_pooling_1d() |>
          layer_dense(units = 32, activation = "relu") |>
          layer_dense(units = 32, activation = "relu") |>
          layer_dense(units = ncol(model_data$train_y), activation = "softmax")
      },
      "cnn9" = function() {
        keras_model_sequential(input_shape = dim(model_data$train_x)[2:3]) |>
          layer_conv_1d(filters = 16, kernel_size = 3) |>
          layer_activation_relu() |>
          layer_batch_normalization() |>
          layer_global_average_pooling_1d() |>
          layer_dense(units = ncol(model_data$train_y), activation = "softmax")
      }
    )
  }),
  tar_target(model_fits,
    {
      file <- str_glue("tmp/best_model/{tar_name()}.h5")

      callbacks <- list(
        callback_early_stopping(patience = 50, min_delta = 0.0001, monitor = "val_accuracy"),
        callback_tensorboard(log_dir = str_glue("tmp/tensorboard/{tar_name()}")),
        callback_model_checkpoint(save_best_only = TRUE, filepath = file, monitor = "val_accuracy"),
        callback_csv_logger(str_glue("tmp/train_history/{tar_name()}.csv"), separator = ",")
      )

      model <- model_archs[[1]]()

      model |>
        compile(loss = "categorical_crossentropy", metrics = "accuracy") |>
        fit(
          x = model_data$train_x,
          y = model_data$train_y,
          callbacks = callbacks,
          epochs = 250,
          verbose = 0,
          validation_split = 0.2
        )

      file
    },
    pattern = map(model_archs),
    format = "file"
  ),
  tar_target(evaluations, {
    tibble(
      name = names(model_archs),
      model = names(model_fits),
    ) |>
      mutate(
        data = model |> map(~ str_glue("tmp/train_history/{.x}.csv") |> read_csv())
      ) |>
      unnest() |>
      group_by(name) |>
      filter(val_accuracy == max(val_accuracy)) |>
      filter(row_number() == 1) |>
      ungroup() |>
      arrange(-val_accuracy)
  })
)