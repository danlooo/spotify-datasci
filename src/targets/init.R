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

access_token <- get_spotify_access_token()

list.files("src/targets", full.names = TRUE) |>
  discard(~ str_ends(.x, "_targets.R")) |>
  discard(~ str_ends(.x, "init.R")) |>
  walk(source)


theme_set(theme_minimal())