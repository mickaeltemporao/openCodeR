#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     test.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2017-08-18 08:41:34
# Modified:     2017-08-31 14:21:59
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2017 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------

# See makeDictionary for missing files/paths

tmp_file <- drop_search("pse_can_dict_v001.csv")
v001 <- read_csv(file.path(db_path, tmp_file$path)) %>% distinct()

tmp_file <- drop_search("pse_can_dict_nsse.csv")
nsse <- read_csv(file.path(db_path, tmp_file$path)) %>% distinct()

tmp_file <- drop_search("pse_pilot_can_20170817.csv")
data <- read_csv(file.path(db_path, tmp_file$path)) %>%
  select(-contains("prog_v"))

tmp <- data %>% left_join(v001, by = "prog_raw") %>%
  left_join(nsse) %>%
  select(contains("prog_"), everything())

tmp_file <- drop_search("pse_pilot_can_20170817.csv")
write.csv(tmp, file.path(db_path, tmp_file$path), row.names = FALSE)

tmp %>% select(contains("prog_")) %>%
  filter(is.na(prog_v001_vpl))
