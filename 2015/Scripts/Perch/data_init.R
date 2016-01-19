## ===========================================================
## Clear the environment first
## ===========================================================
rm(list = ls(all.names=TRUE))

## ===========================================================
## Load Packages -- used here and in other scripts
## ===========================================================
library(dplyr)     # manipulating data
library(magrittr)  # for %<>%
library(readxl)    # reading data
library(ggplot2)   # for visualizations

## ===========================================================
## Load Data
## ===========================================================
diet_spring <- read_excel("2015/Data/WB_Spring_DietAnalysis_2015.xlsx",sheet="Values Only Data")
diet_fall <- read_excel("2015/Data/WB_Fall_DietAnalysis_2015.xlsx",sheet="Values Only Data")
hist_perc <- read_excel("Additional Data/WB_Historical_Percent_Occurrence.xlsx",sheet="data")
wb_shore <- read_excel("Additional Data/lake_erie_western_basin_shoreline.xlsx",sheet="LatLong")
spring_lw <- read_excel("2015/Data/WB_Spring_DietAnalysis_2015.xlsx",sheet="LW")
fall_lw <- read_excel("2015/Data/WB_Fall_DietAnalysis_2015.xlsx",sheet="LW")
prey_type <- read_excel("2015/Data/WB_Fall_DietAnalysis_2015.xlsx",sheet="Prey Type") %>% 
  select(food_item=item,type)
effort <- read_excel("Additional Data/WB_Effort.xlsx",sheet="Effort")

## ===========================================================
## Change Chironomid Larvae and Pupae to Chironomidae
## ===========================================================
diet_spring$food_item <- gsub("Chironomid Larvae","Chironomidae",diet_spring$food_item)
diet_spring$food_item <- gsub("Chironomid Pupae","Chironomidae",diet_spring$food_item)
diet_fall$food_item <- gsub("Chironomid Larvae","Chironomidae",diet_fall$food_item)
diet_fall$food_item <- gsub("Chironomid Pupae","Chironomidae",diet_fall$food_item)

## ===========================================================
## Change Chironomid Daphnia spp. and Daphnia retrocurva to Daphnidae
## ===========================================================
diet_spring$food_item <- gsub("Daphnia spp.","Daphnidae",diet_spring$food_item)
diet_spring$food_item <- gsub("Daphnia retrocurva","Daphnidae",diet_spring$food_item)
diet_fall$food_item <- gsub("Daphnia spp.","Daphnidae",diet_fall$food_item)
diet_fall$food_item <- gsub("Daphnia retrocurva","Daphnidae",diet_fall$food_item)
