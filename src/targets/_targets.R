#!/usr/bin/env R

#
# R targets pipeline
#

source("src/targets/init.R")

list(
  tar_target(track_selected_audio_features, c(
    "danceability", "energy", "key", "mode", "valence", "loudness",
    "speechiness", "acousticness", "instrumentalness", "liveness"
  )),
  tar_target(terms_file, "raw/terms.txt", format = "file"),
  tar_target(terms, read_lines(terms_file)),
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
    command = tibble(id = track_ids,audio_analysis = track_ids |> possibly(get_track_audio_analysis, NA)() |> list()),
    pattern = map(track_ids) # only one track per target
  ),
  tar_target(track_audio_features, get_track_audio_features(track_searches$id), pattern = map(track_searches)), # can be done in batches
  tar_target(track_features_file, {
    track_audio_features |>
      left_join(track_searches, by = "id") |>
      write_csv("tmp/track_features.csv")
    }),
  tar_target(track_pitches, {
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
        }, NA)))
  },
  pattern = map(track_audio_analyses)
  ),
  tar_target(track_valid, {
    # tracks having all features available
    track_audio_analyses |>
      filter(! is.na(audio_analysis)) |>
      inner_join(track_audio_features) |>
      pull(id)
  })
)
