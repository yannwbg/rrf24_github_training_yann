### Reproducible Research Fundamentals -  Main R Script

#To do on first iteration
#renv::restore()

# Load libraries ---- 

# Load necessary libraries
library(haven)  # for reading .dta files
library(dplyr)  # for data manipulation
library(tidyr)  # for reshaping data
library(stringr) # work with strings
library(labelled) # use labels
library(gtsummary) # tables
library(gt) # tables
library(ggplot2) #graphs
library(tidyverse) # working with tidy data
library(modelsummary) # creating summary tables
library(stargazer) # writing nice tables
library(RColorBrewer) # color palettes

# Set data path ----

# this is the second root of the project, the first root is the code whose directory 
# is already being handled by the rstudio project.

data_path <- "C:/Users/wb566769/OneDrive - WBG/Documents/GitHub/DataWork/Data"

# Run the R scripts ----

source("Code/01-processing-data.R")
source("Code/02-constructing-data.R")
source("Code/03-analyzing-data.R")

