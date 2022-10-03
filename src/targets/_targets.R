#!/usr/bin/env R

#
# R targets pipeline
#

source("src/targets/init.R")

list(
  tar_target(terms_file, "raw/terms.txt", format = "file"),
  tar_target(terms, read_lines(terms_file)),
  tar_target(
    name = track_searches,
    command = {
      tibble(term = terms) |>
        mutate(data = term |> map( ~ search_spotify(.x, type = "track", limit = 50))) |>
        unnest(data)
      },
    pattern = map(terms)
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
    command = tibble(id = track_ids,audio_analysis = track_ids |> get_track_audio_analysis() |> list()),
    pattern = map(track_ids) # only one track per target
  ),
  tar_target(track_audio_features, get_track_audio_features(track_searches$id), pattern = map(track_searches)), # can be done in batches
  tar_target(track_features_file, {
    track_audio_features |>
      left_join(track_searches, by = "id") |>
      write_csv("tmp/track_features.csv")
    }),
  tar_target(track_pca_plt, plot_tracks_pca(track_audio_features, track_searches)),
  tar_target(track_pca_plt_file, ggsave("tmp/track_pca_plt.png", track_pca_plt), format = "file")
)
