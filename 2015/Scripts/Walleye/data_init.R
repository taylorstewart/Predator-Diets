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
walleye_diets <- read_excel("2015/Data/WB_YOY_Walleye_DietAnalysis_2015.xlsx",sheet="Diet Summary")
prey_type <- read_excel("2015/Data/WB_YOY_Walleye_DietAnalysis_2015.xlsx",sheet="Prey Type") %>% 
  select(food_item=item,type)
effort <- read_excel("Additional Data/WB_Effort.xlsx",sheet="Effort")

## ===========================================================
## Change Chironomid Larvae and Pupae to Chironomidae
## ===========================================================
walleye_diets$food_item <- gsub("Chironomid Pupae","Chironomidae",walleye_diets$food_item)

