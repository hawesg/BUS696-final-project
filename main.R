# Load most commonly used libraries

list.of.packages <- c("tidyverse", "plotly", "here", "ggthemes", "stringr", "plyr", "stringi")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("plyr")
library("tidyverse")
library("here")
library("ggthemes")
library("stringr")
library("stringi")

# ---- begin ----
# Step One: Load Data:
source("code/load.R")

# ---- clean ----
# Step Two: Clean Data:
source("code/clean.R")

# ---- feature generation ----
# Step Three: Feature Engeneering:
source("code/features.R")

# ---- analysis ----
# Step Three: Analyzing data for report:
source("code/analysis.R")

# ---- plots ----
# Step Three: Analyzing data for report:
source("code/plots/plots.R")

# ---- render ----
# Step Three: Knitting Report
#rmarkdown::render("rmd/README.Rmd","github_document", "../README.md")
#rmarkdown::render("rmd/README.Rmd","pdf_document", "../analysis.pdf")
