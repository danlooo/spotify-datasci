#!/usr/bin/env Rscript

options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/all/__linux__/focal/2022-08-01+Y3JhbiwyOjQ1MjYyMTU7RTY0MEEyRTM"))

cran_packages <- c("targets", "reticulate", "spotifyr", "styler", "ggnewscale", "ggrepel", "ggpubr", "ggbeeswarm", "broom", "Hmisc")

install.packages(cran_packages)

reticulate::py_install(c("pandas", "numpy", "tensorflow", "keras", "sklearn"))
