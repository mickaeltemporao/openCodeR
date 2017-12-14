#' Open Coder
#' param raw_input character vector containing the column name of the variable
#' to recode
#' importFrom jsonlite fromJSON
open_code <- function(input_csv,
                      raw_dict,

                      groups    = "groups.json") {

  test_csv <- try(read_csv(input_csv, n_max = 5), silent = TRUE)

  if(class(test_csv) == "try-error") {
    stop(sprintf("%s needs to be a csv file!", input_csv))
  }

  # chose variable
  raw_input = "programs_raw",

  groups <- fromJSON(groups)

  input_coding <- menu(names(groups),
                       title = "Choose a coding group:")
  clean_codes <- groups[[input_coding]]

  output_var <- paste0("codes_", names(programs)[input_coding])

  #TODO: add match structure test
  raw_input   <- grep(raw_input, names(raw_dict), value = TRUE)
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

    # Get highest frequency inputs to recode
    value <- remaining[[raw_input]][1]

    # Prompt user
    input <- menu(clean_codes,
                  title = sprintf("Choose a program for:\n\n %s", value))

    # Record input
    index <- which(raw_dict[[raw_input]] == value)
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
