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

dotenv::load_dot_env()
safely(get_spotify_access_token)()
theme_set(theme_minimal())

list(
  tar_target(selected_audio_features, c(
    "danceability", "energy", "key", "mode", "valence", "loudness",
    "speechiness", "acousticness", "instrumentalness", "liveness"
  )),
  tar_target(terms, c("techno", "rock", "jazz", "classical")),
  tar_target(track_searches_offsets, seq(0, 850, by = 50)),
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
    pitch_error_tracks <-
      track_pitches |>
      unnest(pitches) |>
      count(id) |>
      filter(n != 800)

    ambigious_tracks <-
      track_searches |>
      count(id) |>
      filter(n > 1)

    # tracks having all features available
    track_audio_analyses |>
      filter(!is.na(audio_analysis)) |>
      inner_join(track_audio_features) |>
      anti_join(ambigious_tracks) |>
      anti_join(pitch_error_tracks) |>
      pull(id) |>
      unique()
  }),
  tar_target(model_data, {
    valid_train_samples <-
      track_train_test_split |>
      filter(is_train) |>
      pull(id) |>
      intersect(valid_tracks)

    valid_test_samples <-
      track_train_test_split |>
      filter(!is_train) |>
      pull(id) |>
      intersect(valid_tracks)

    x_long <-
      track_pitches |>
      filter(id %in% valid_tracks) |>
      unnest(pitches) |>
      group_by(id) |>
      mutate(segment = row_number()) |>
      pivot_longer(-c(id, segment), names_to = "pitch_name") |>
      arrange(pitch_name, segment, id) # array fills column-wise

    x_train_long <- x_long |> filter(id %in% valid_train_samples)
    x_test_long <- x_long |> filter(id %in% valid_test_samples)

    x_train_array <-
      x_train_long |>
      pull(value) |>
      array(
        dim = c(
          x_train_long$id |> unique() |> length(),
          x_train_long$segment |> unique() |> length(),
          x_train_long$pitch_name |> unique() |> length()
        ),
        dimnames = list(
          "id" = x_train_long$id |> unique(),
          "segment" = x_train_long$segment |> unique(),
          "pitch_name" = x_train_long$pitch_name |> unique()
        )
      )

    x_test_array <-
      x_test_long |>
      pull(value) |>
      array(
        dim = c(
          x_test_long$id |> unique() |> length(),
          x_test_long$segment |> unique() |> length(),
          x_test_long$pitch_name |> unique() |> length()
        ),
        dimnames = list(
          "id" = x_test_long$id |> unique(),
          "segment" = x_test_long$segment |> unique(),
          "pitch_name" = x_test_long$pitch_name |> unique()
        )
      )

    y_train <-
      tibble(id = x_train_long$id |> unique()) |>
      filter(id %in% valid_train_samples) |>
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

    y_test <-
      tibble(id = x_test_long$id |> unique()) |>
      filter(id %in% valid_test_samples) |>
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

    list(train_x = x_train_array, train_y = y_train, test_x = x_test_array, test_y = y_test)
  }),
  tar_target(model_archs, {
    list(
      "base" = function() {
        # base MLP ignoring spatial aspects
        keras_model_sequential(input_shape = dim(model_data$train_x)[2:3], name = "base") |>
          layer_global_average_pooling_1d() |>
          layer_flatten() |>
          layer_dense(units = 4, activation = "relu") |>
          layer_activation_selu() |>
          layer_dense(units = ncol(model_data$train_y), activation = "softmax")
      },
      "lstm" = function() {
        # LSTM to model unidirectional of time series
        keras_model_sequential(input_shape = dim(model_data$train_x)[2:3], name = "lstm") |>
          layer_conv_1d(filters = 1, kernel_size = 5) |>
          layer_lstm(units = 32) |>
          layer_dense(units = 32) |>
          layer_activation_selu() |>
          layer_dense(units = ncol(model_data$train_y), activation = "softmax")
      },
      "cnn1" = function() {
        # sequential CNN
        keras_model_sequential(input_shape = dim(model_data$train_x)[2:3], name = "cnn1") |>
          layer_conv_1d(filters = 12, kernel_size = 3) |>
          layer_activation_relu() |>
          layer_batch_normalization() |>
          layer_conv_1d(filters = 12, kernel_size = 3) |>
          layer_activation_relu() |>
          layer_batch_normalization() |>
          layer_global_average_pooling_1d() |>
          layer_dense(units = ncol(model_data$train_y), activation = "softmax")
      },
      "cnn2" = function() {
        # CNN that can pick patterns of various sizes
        input <- layer_input(shape = dim(model_data$train_x)[2:3])
        c1 <- layer_conv_1d(filters = 6, kernel_size = 11, name = "small_patterns")(input)
        c2 <- layer_conv_1d(filters = 6, kernel_size = 51, name = "medium_patterns")(input)
        c3 <- layer_conv_1d(filters = 6, kernel_size = 101, name = "big_patterns")(input)
        agg1 <- layer_global_max_pooling_1d(c1, name = "small_pool")
        agg2 <- layer_global_max_pooling_1d(c2, name = "medium_pool")
        agg3 <- layer_global_max_pooling_1d(c3, name = "big_pool")
        agg <- layer_concatenate(list(agg1, agg2, agg3))
        cl1 <- layer_dense(units = 12)(agg)
        cl2 <- layer_activation_selu(cl1)
        output <- layer_dense(units = ncol(model_data$train_y), activation = "softmax")(cl2)
        keras_model(input, output, name = "cnn2")
      }
    )
  }),
  tar_target(model_fits,
    {
      file <- str_glue("tmp/best_model/{tar_name()}.h5")

      callbacks <- list(
        callback_early_stopping(patience = 20, min_delta = 0.001, monitor = "val_loss", verbose = 1),
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
          verbose = 1,
          validation_data = list(model_data$test_x, model_data$test_y)
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
