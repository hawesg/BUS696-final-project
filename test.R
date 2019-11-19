#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  FCT_LUMPS <-
    list(
      variety_color = 5,
      taster_name = 1,
      taster_twitter = 5,
      designation = 10,
      country = 10,
      variety = 10
    )
} else {
  FCT_LUMPS <- dget(args[1])
}



