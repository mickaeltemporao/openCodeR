#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     init.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2017-09-15 04:33:15
# Modified:     2017-09-15 04:33:59
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2017 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
source("config")
source("R/openCoder.R")

raw_dict <- readr::read_csv(dict_path)
promptUser(raw_dict = raw_dict, programs_raw = "programs_raw")
