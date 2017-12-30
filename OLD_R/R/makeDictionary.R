#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     makeDictionary.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2017-08-16 10:17:50
# Modified:     2017-09-14 13:43:42
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2017 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
# TODO: Add option to enable subset recoding

# File parameters ==============================================================
# Dropbox path
db_path      <- "~/Dropbox (Vox Pop Labs)"
# Dropbox data file name
db_data_name <- "%5BPilot%5D+PSE+Pilot+Canada_August+16%2C+2017_15.29.csv"
# Local file name
input_file   <- "tmp_data.csv"
# Dictionary file name
dict_file <- "pse_can.dict"
# Panel Path
can_pan_file <- "pse_can_ses.csv"



  # Dictionary Maker =============================================================
check_dict <- drop_dir(db_pil_path) %>% filter(grepl(".dict$", path))

if(nrow(check_dict) == 0) {

  input <- menu(c("Yes", "No"),
                title = "No dictionary file found on drop box, do you want to create it?")

  if (input == 1) {

    message("Enter the name of the program variable (e.g. programs_raw):")
    prog_var <- read_char() %>% match(names(data))
    if (is.na(prog_var)) stop("Program variable provided does not exist!")

    # MAJORS Q58:Q66_3 | Q58:Q65
    raw_dict <- data %>%
      select(prog_var) %>%
      distinct()
    names(raw_dict) <- "programs_raw"

    raw_dict <- raw_dict %>%
      rowwise() %>%
      mutate(programs_raw = char_fix(programs_raw)) %>%
      arrange(programs_raw) %>%
      group_by(programs_raw) %>%
      mutate(program_vpl = NA_character_) %>%
      arrange(programs_raw)

    sprintf('Creating the dictionary file: %s', dict_file)
    write.csv(raw_dict,
              file.path(db_path, db_pil_path, "pse_can.dict"),
              row.names = FALSE)
  } else {
    stop("Provide an existing dictionary file or create one.")
  }

}


# User prompter ================================================================
promptUser <- function(raw_dict,
                       programs_raw = "programs_raw",
                       clean_programs) {
  suppressMessages(
                   require(tidyverse)
                   )

  #TODO: add match structure test
  programs_raw   <- grep(programs_raw, names(raw_dict), value = TRUE)
  programs_coded <- grep("program_v", names(raw_dict), value = TRUE)

  input <- menu(names(raw_dict),
                title = "Enter the name of the variable you want to clean: ")
  to_code <- names(raw_dict)[input]

  test <- sum(is.na(raw_dict[[to_code]]))

  cnt <- 0
  while(test>0) {
    # Get current progress
    coded <- !is.na(raw_dict[[to_code]])
    progress <- sum(coded)/nrow(raw_dict)
    message(sprintf("\n%.2f%% coding complete.\n", progress*100))

    # Subset of remaining codings
    remaining <- raw_dict[is.na(raw_dict[[to_code]]),]

    # Get highest frequency program to recode
    value <- remaining[[programs_raw]][1]

    # Prompt user
    input <- menu(clean_programs,
                  title = sprintf("Choose a program for:\n\n %s", value))

    # Record input
    index <- which(raw_dict[[programs_raw]] == value)
    raw_dict[[to_code]][index] <- clean_programs[input]

    cnt <- cnt + 1

    if (cnt >=10) {
      write.csv(raw_dict,
                file.path(db_path, db_pil_path, "pse_can.dict"),
                row.names = FALSE)
      cnt <- 0
    }

    if (input == 0) {
      write.csv(raw_dict,
                file.path(db_path, db_pil_path, "pse_can.dict"),
                row.names = FALSE)
      break
    }
  }
  write.csv(raw_dict,
            file.path(db_path, db_pil_path, "pse_can.dict"),
            row.names = FALSE)
}

promptUser(raw_dict = raw_dict, programs_raw = "programs_raw", clean_programs)
