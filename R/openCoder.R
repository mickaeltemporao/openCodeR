#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     openCoder.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2017-09-14 13:34:07
# Modified:     2017-09-15 04:37:01
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2017 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
promptUser <- function(raw_dict,
                       programs_raw = "programs_raw",
                       groups = "categories.json") {
  suppressMessages(
                   require(tidyverse)
                   )

  programs <- jsonlite::fromJSON(groups)

  input_coding <- menu(names(programs),
                      title = "Choose a coding group")
  clean_codes <- programs[[input_coding]]


  # readCharacter <- function() {
  #   n <- readline(prompt="Input value: ")
  #   return(as.character(n))
  # }

  # message("Enter the name of the coded variable: (e.g. v1):")
  # output_var <- readCharacter()
  output_var <- paste0("codes_", names(programs)[input_coding])

  #TODO: add match structure test
  programs_raw   <- grep(programs_raw, names(raw_dict), value = TRUE)
  programs_coded <- grep("program_v", names(raw_dict), value = TRUE)

  input <- menu(names(raw_dict),
                title = "Choose the the variable you want to clean: ")

  to_code <- names(raw_dict)[input]



  raw_dict[[output_var]] <- NA_character_

  test <- sum(is.na(raw_dict[[output_var]]))

  cnt <- 0

  while (test > 0) {
    # Get current progress
    coded <- !is.na(raw_dict[[output_var]])
    progress <- sum(coded)/nrow(raw_dict)
    message(sprintf("\n%.2f%% coding complete.\n", progress*100))

    # Subset of remaining codings
    remaining <- raw_dict[is.na(raw_dict[[output_var]]),]

    # Get highest frequency program to recode
    value <- remaining[[programs_raw]][1]

    # Prompt user
    input <- menu(clean_codes,
                  title = sprintf("Choose a program for:\n\n %s", value))

    # Record input
    index <- which(raw_dict[[programs_raw]] == value)
    raw_dict[[output_var]][index] <- clean_codes[input]

    cnt <- cnt + 1

    if (cnt >=10) {
      readr::write_csv(raw_dict, dict_path)
      cnt <- 0
    }

    if (input == 0) {
      readr::write_csv(raw_dict, dict_path)
      break
    }
  }
  readr::write_csv(raw_dict, dict_path)
}
