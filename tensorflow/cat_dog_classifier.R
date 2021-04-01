# classify images of dogs and cats

library(keras)

# data inputs
library(tidyverse)
library(fs)
files_table <- as_tibble(path_rel(choose.files()))





