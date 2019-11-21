# Load most commonly used libraries

# list.of.packages <- c("tidyverse", "plotly", "here", "ggthemes", "stringr", "plyr", "stringi", "readxl", "PerformanceAnalytics", "ggExtra")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()
#install.packages("devtools")
#install.packages("conflicted")

library("conflicted")
library("ggmap")
library("tidyverse")
library("here")
library("ggthemes")
library("stringr")
library("stringi")
library("readxl")
library("ggExtra")
library("PerformanceAnalytics")
library("GGally")
library("qwraps2")
library("plotROC")
library('sentimentr')





# ---- constants ----


# Set fct_lump size for the various times that fct_lump is used.
FCT_LUMPS <-
  list(
    variety_color = 5,
    taster_name = 1,
    taster_twitter = 5,
    designation = 10,
    country = 10,
    variety = 10,
    province = 10
  )


# test4<-fct_lump(test, n=FCT_LUMPS["taster_name"]) or FCT_LUMPS$taster_name


# VARIETY_PER_COLOR_LUMP <- 5
# TASTER_NAME_LUMP <- 5
# TASTER_TWITTER_LUMP <- 5
# DESIGNATION_LUMP <- 10
# COUNTRY_LUMP <- 10
# VARIETY_LUMP <- 10

# ---- begin ----
# Step One: Load Data:
source("code/load.R")

# ---- clean ----
# Step Two: Clean Data:
source("code/clean.R")

# ---- feature generation ----
# Step Three: Feature Engeneering:
source("code/features.R")

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
