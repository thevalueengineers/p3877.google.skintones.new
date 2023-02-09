# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(qualtRics)
library(tveDataLoader)
library(readxl)

# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c('dplyr',
               'stringr',
               'tidyr',
               'ggplot2',
               'effects',
               'readr',
               'glmnet',
               'car',
               'broom',
               'tidyverse',
               'caret',
               'purrr',
               'weights',
               'multcompView',
               'agricolae'
  ), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

# Setup


setup <- list(
  tar_target(
    data_folder,
    paste0(
      "Z:/Shared/TVE Data/1. Client Projects/Google/3877 - Skin Tone Scales - New Scales Iteration/",
      "4. Work in Progress/1. Data/"
    )
  ),
  tar_target(
    data,
    drop_first_two_rows(
      paste0(
        data_folder,
        "Feb 2nd 2023 Data Run.xls"
      )
    )
  )
)

# targets workflow ----
list(
  setup
)
