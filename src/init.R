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
safely(get_spotify_access_token)()

theme_set(theme_minimal())