###############################################################################-
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
###############################################################################-



################################### Clean up ###################################

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

################################## Libraries ###################################

source("code/1.libraries.R")

################################### Contants ###################################

################################## PARAMETERS ##################################
#                                                                              #
# Can set the individual factor lump numbers here, by_count will override all  #
# others if it is not 0 by lumping only factors that have n observations       #
#                                                                              #
###############################################################################-

FCT_LUMPS <-
  list(
    by_count = 0,
    taster_name = 10,
    taster_twitter = 10,
    designation = 15,
    country = 15,
    variety = 15,
    variety.red = 10,
    variety.white = 10,
    province = 15,
    winery = 15
  )
# Ex: test4<-fct_lump(test, n=FCT_LUMPS["taster_name"]) or FCT_LUMPS$taster_name

# ---- begin ----
# Step One: Load Data:
source("code/2.load.R")

# ---- clean ----
# Step Two: Clean Data:
source("code/3.clean.R")

# ---- feature generation ----
# Step Three: Feature Engeneering:
source("code/4.features.R")

################################### Clean up ###################################

# dput(ls())

rm("colors", "designation_rp", "other", "patterns", 
   "red", "replacements", "temp", "twitter_stats", "wd_a_tl", 
   "wd_temp", "wdx", "white", "wine_data", "wine_data_bind", 
   "wine_data_end_of_clean", "wine_data_original", "wine_data_with_twitter_data")

save(wine_data_clean, file = here::here("data","output","clean_wine.RData"))

# ---- analysis ----
# Step Three: Analyzing data for report:
source("code/5.analysis.R")

# ---- plots ----
# Step Three: Analyzing data for report:
#source("code/plots/plots.R")

# ---- render ----
# Step : Knitting Report
rmarkdown::render("rmd/README.Rmd","github_document", "README.md")
#rmarkdown::render("rmd/README.Rmd","pdf_document", "../analysis.pdf")