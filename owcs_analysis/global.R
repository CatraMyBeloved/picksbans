# global.R
library(shiny)
library(bslib)
library(tidyverse)
library(DT)

# Helper function to load all R files in a directory
load_module_files <- function(dir) {
  files <- list.files(dir, pattern = "\\.R$", full.names = TRUE)
  lapply(files, source)
}
