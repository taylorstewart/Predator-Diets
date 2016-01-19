## Load Data
source("2015/Scripts/Walleye/data_init.R")

## -----------------------------------------------------------
## Remove stomachs with no diet contents
## -----------------------------------------------------------
walleye_diets %<>% filter(food_item != "Empty")

## -----------------------------------------------------------
## Creat lists of prey types found for both species and season
## -----------------------------------------------------------
diet_list <- unique(walleye_diets$food_item)

## -----------------------------------------------------------
### Percent Occurence
## -----------------------------------------------------------
## Calculate the number of fish found with each prey taxa
walleye_diet_n <- as.data.frame(do.call(rbind,lapply(diet_list,function(i) {
  walleye_diets %<>% filter(food_item == i)
  nrow(walleye_diets)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
walleye_freq <- as.data.frame(do.call(rbind,lapply(1:nrow(walleye_diet_n),function(j) {
  round(as.numeric(((walleye_diet_n[j,1])/length(unique(walleye_diets$fid)))*100),1)
})))
## Add prey names to data frame
walleye_freq %<>% transmute(prey_type = diet_list,
                              percent_occur = V1)

## -----------------------------------------------------------
## Save table data into Excel spreadsheet
## -----------------------------------------------------------
library(XLConnect)
wb <- loadWorkbook("2015/Data/walleye_freq_occurence.xlsx",create=T)
createSheet(wb,name="Table")
writeWorksheet(wb,data=as.data.frame(walleye_freq),sheet="Table",startRow=1,startCol=1,header=TRUE)
saveWorkbook(wb)
