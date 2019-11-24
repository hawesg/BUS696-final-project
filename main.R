################################################################################
#                                                                              #
# Purpose:       Jumping off point for the project                             #
#                                                                              #
# Author:        Garrett H, Bart C, Ricky L                                    #
# Contact:       hawes102@mail.chapman.edu                                     #
# Client:        Jonathan Hersh                                                #
#                                                                              #
# Code created:  2019-11-23                                                    #
# Last updated:  2019-11-23                                                    #
# Source:        /Users/garretthawes/wine-project                              #
#                                                                              #
# Comment:       I am planning on inserting some command line options so that  #
#                I can fine tune things from an SSH shell stay tuned.          #
#                                                                              #
################################################################################



################################### Clean up ###################################

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()

################################## Libraries ###################################

source("code/libraries.R")

################################### Contants ###################################

################################## PARAMETERS ##################################
#                                                                              #
# Can set the individual factor lump numbers here, by_count will override all  #
# others if it is not 0 by lumping only factors that have n observations       #
#                                                                              #
################################################################################

FCT_LUMPS <-
  list(
    by_count = 0,
    taster_name = 5,
    taster_twitter = 5,
    designation = 15,
    country = 15,
    variety = 15,
    variety.red = 5,
    variety.white = 5,
    variety.other = 5,
    province = 15,
    winery = 15
  )
# Ex: test4<-fct_lump(test, n=FCT_LUMPS["taster_name"]) or FCT_LUMPS$taster_name

# ---- begin ----
# Step One: Load Data:
source("code/load.R")

# ---- clean ----
# Step Two: Clean Data:
source("code/clean.R")

# ---- feature generation ----
# Step Three: Feature Engeneering:
source("code/features.R")

################################### Clean up ###################################

# TODO Look up how to just erase all the variables other than wine_data_clean
save(wine_data_clean, file = here::here("data","output","clean_wine.RData"))
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
load(here::here("data","output","clean_wine.RData"))

# ---- analysis ----
# Step Three: Analyzing data for report:
source("code/analysis.R")


# ---- plots ----
# Step Three: Analyzing data for report:
#source("code/plots/plots.R")

# ---- render ----
# Step : Knitting Report
#rmarkdown::render("rmd/README.Rmd","github_document", "../README.md")
#rmarkdown::render("rmd/README.Rmd","pdf_document", "../analysis.pdf")


# 
# #!/usr/bin/env Rscript
# args = commandArgs(trailingOnly = TRUE)
# 
# if (length(args) == 0) {
#   FCT_LUMPS <-
#     list(
#       variety_color = 5,
#       taster_name = 1,
#       taster_twitter = 5,
#       designation = 10,
#       country = 10,
#       variety = 10
#     )
# } else {
#   FCT_LUMPS <- dget(args[1])
# }
# 
# 
# 



# header_comment(
#   '', # Purpose
#   ''# Comment
# )
# line_comment("Setup testing and training set")
