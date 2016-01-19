## Load Data
source("2015/Scripts/Perch/data_init.R")

## -----------------------------------------------------------
## Remove stomachs with no diet contents
## -----------------------------------------------------------
diet_spring %<>% filter(food_item != "Empty")
diet_fall %<>% filter(food_item != "Empty")

## -----------------------------------------------------------
## Filter into each species
## -----------------------------------------------------------
yp_spring <- filter(diet_spring,species=="Yellow Perch")
wp_spring <- filter(diet_spring,species=="White Perch")
yp_fall <- filter(diet_fall,species=="Yellow Perch")
wp_fall <- filter(diet_fall,species=="White Perch")

## -----------------------------------------------------------
## Creat lists of prey types found for both species and season
## -----------------------------------------------------------
yp_spring_list <- unique(yp_spring$food_item)
wp_spring_list <- unique(wp_spring$food_item)
yp_fall_list <- unique(yp_fall$food_item)
wp_fall_list <- unique(wp_fall$food_item)

## -----------------------------------------------------------
### SPRING (Yellow Perch and White Perch)
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the number of fish found with each prey taxa
yp_spring_n <- as.data.frame(do.call(rbind,lapply(yp_spring_list,function(i) {
  yp_spring %<>% filter(food_item == i)
  nrow(yp_spring)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
yp_spring_freq <- as.data.frame(do.call(rbind,lapply(1:nrow(yp_spring_n),function(j) {
  round(as.numeric(((yp_spring_n[j,1])/length(unique(yp_spring$fid)))*100),1)
})))
## Add prey names to data frame
yp_spring_freq %<>% transmute(prey_type = yp_spring_list,
                              percent_occur = V1,
                              species = "Yellow Perch",
                              season = "Spring")

##### WHITE PERCH #####
## Calculate the number of fish found with each prey taxa
wp_spring_n <- as.data.frame(do.call(rbind,lapply(wp_spring_list,function(i) {
  wp_spring %<>% filter(food_item == i)
  nrow(wp_spring)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
wp_spring_freq <- as.data.frame(do.call(rbind,lapply(1:nrow(wp_spring_n),function(j) {
  round(as.numeric(((wp_spring_n[j,1])/length(unique(wp_spring$fid)))*100),1)
})))

## Add prey names, fish species, and season to data frame
wp_spring_freq %<>% transmute(prey_type = wp_spring_list,
                              percent_occur = V1,
                              species = "White Perch",
                              season = "Spring")

## -----------------------------------------------------------
### FALL
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the number of fish found with each prey taxa
yp_fall_n <- as.data.frame(do.call(rbind,lapply(yp_fall_list,function(i) {
  yp_fall %<>% filter(food_item == i)
  nrow(yp_fall)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
yp_fall_freq <- as.data.frame(do.call(rbind,lapply(1:nrow(yp_fall_n),function(j) {
  round(as.numeric(((yp_fall_n[j,1])/length(unique(yp_fall$fid)))*100),1)
})))

## Add prey names, fish species, and season to data frame
yp_fall_freq %<>%  transmute(prey_type = yp_fall_list,
                             percent_occur = V1,
                             species = "Yellow Perch",
                             season = "Fall")

##### WHITE PERCH #####
## Calculate the number of fish found with each prey taxa
wp_fall_n <- as.data.frame(do.call(rbind,lapply(wp_fall_list,function(i) {
  wp_fall %<>% filter(food_item == i)
  nrow(wp_fall)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
wp_fall_freq <- as.data.frame(do.call(rbind,lapply(1:nrow(wp_fall_n),function(j) {
  round(as.numeric(((wp_fall_n[j,1])/length(unique(wp_fall$fid)))*100),1)
})))
## Add prey names, fish species, and season to data frame
wp_fall_freq %<>% transmute(prey_type = wp_fall_list,
                            percent_occur = V1,
                            species = "White Perch",
                            season = "Fall")

## -----------------------------------------------------------
## Combine into a final data frame
## -----------------------------------------------------------
freq_comb <- bind_rows(yp_spring_freq,wp_spring_freq,yp_fall_freq,wp_fall_freq)

## -----------------------------------------------------------
## Save table data into Excel spreadsheet
## -----------------------------------------------------------
library(XLConnect)
wb <- loadWorkbook("2015/Data/WB_Percent_Occurrence_Table.xlsx",create=T)
createSheet(wb,name="Table")
writeWorksheet(wb,data=as.data.frame(freq_comb),sheet="Table",startRow=1,startCol=1,header=TRUE)
saveWorkbook(wb)
