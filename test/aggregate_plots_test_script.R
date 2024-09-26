## Description =================================================================
# Title:        Aggregate Plots Test Script
# Authors:      Leila Orzag, Jake Peters
# Date:         2024-09-26
# Objective:    We'll use this script to improve the visualization of the
#               Aggregate Recruitment Data. The goal will be to inform decision
#               makers about site-by-site recruitment challenges/successes.
# Tasks:        
#               - [x] Make test script.
#               - [ ] Swap bar charts for line graphs for trends over time
#               - [ ] Extension for HP sites
#               - [ ] Consolidate "verified" and "response_ratio" plots to 2
#                     plotting functions where "site" and "filter" are functions
#               - [ ] Improve overall readability
#               - [ ] Print all figures to PDF to share with Mia
# GH-Issue:     


## Script parameters ===========================================================
project <- "nih-nci-dceg-connect-bq2-prod"
dataset <- "StakeHolderMetrics_RS"
table   <- "aggregate_recruitment"

## Dependencies ================================================================
library(dplyr)
library(DBI)
library(bigrquery)
library(glue)

## Fetch data ==================================================================
source("./app/get_data.R", local=TRUE)      # Rebecca already wrote these for us
source("./app/clean_data.R", local=TRUE)
data <- get_data(project = project, dataset = dataset, table = table)
data <- clean_data(data, type = "aggregate")

## Visualize Data ==============================================================
source("./app/color_palette.R") # Rebecca's code to prescribe colors
source("./app/aggregate_plots/HealthPartners/verified_by_sex.R")
plot <- verified_by_sex_hp(data)
print(plot)

# Leila, I think the next step is to open this plotting function and modify it 
# so that it produces a line plot rather than a bar plot. 

