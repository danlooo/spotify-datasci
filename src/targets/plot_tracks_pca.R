plot_tracks_pca <- function(track_audio_features, track_searches) {
  features <- c("danceability", "energy", "key", "mode", "valence", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness")
  
  pca <-
    track_audio_features |>
    filter(! is.na(id)) |>
    column_to_rownames("id") |>
    select(features) |>
    mutate(across(everything(), scale)) |>
    filter(across(everything(), ~ ! is.na(.x))) |>
    prcomp()
  
  tracks <-
    pca$x |>
    as_tibble(rownames = "id") |>
    left_join(track_audio_features, by = "id") |>
    left_join(track_searches, by = "id")
  
  pca_clusters <-
    tracks |>
    group_by(term) |>
    summarise(PC1 = median(PC1), PC2 = median(PC2))
  
  tibble() %>%
    ggplot(aes(x = PC1, y = PC2)) +
    geom_point(
      data = tracks,
      mapping = aes(color = term)
    ) +
    ggrepel::geom_label_repel(
      data = pca_clusters,
      mapping = aes(label = term, color = term)
    ) +
    guides(color = FALSE) +
    ggnewscale::new_scale_color() +
    geom_segment(
      data = pca$rotation %>% as_tibble(rownames = "feature"),
      mapping = aes(x = 0, y = 0, xend = max(abs(pca$x[,1])) * PC1, yend = max(abs(pca$x[,2])) * PC2),
      arrow = arrow()
    ) +
    ggrepel::geom_text_repel(
      data = pca$rotation %>% as_tibble(rownames = "feature"),
      mapping = aes(label = feature, x = max(abs(pca$x[,1])) * PC1, y = max(abs(pca$x[,2])) * PC2)
    ) +
    labs(title = "The Space of Music") +
    theme_minimal()
}