## Load Data
source("2014/Scripts/data_init.R")

diet_spring <- readWorksheet(db1,sheet="Calculated Data")
diet_fall <- readWorksheet(db2,sheet="Calculated Data")
diet_spring %<>% filter(!is.na(food_item))
diet_fall %<>% filter(!is.na(food_item))

## -----------------------------------------------------------
## Filter into each species
## -----------------------------------------------------------
yp_spring <- filter(diet_spring,species=="yellow perch")
wp_spring <- filter(diet_spring,species=="white perch")
yp_fall <- filter(diet_fall,species=="yellow perch")
wp_fall <- filter(diet_fall,species=="white perch")

## -----------------------------------------------------------
## Creat lists of prey types found for both species and season
## -----------------------------------------------------------
yp_spring_list <- unique(yp_spring$food_item)
wp_spring_list <- unique(wp_spring$food_item)
yp_fall_list <- unique(yp_fall$food_item)
wp_fall_list <- unique(wp_fall$food_item)

## -----------------------------------------------------------
### SPRING
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the number of fish found with each prey taxa
yp_spring_n <- as.data.frame(do.call(rbind,lapply(yp_spring_list,function(i) {
  yp_spring %<>% filter(food_item == i)
  nrow(yp_spring)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
yp_spring_freq <- as.data.frame(do.call(rbind,lapply(1:nrow(yp_spring_n),function(j) {
  round(as.numeric(((yp_spring_n[j,1])/sum(yp_spring_n))*100),1)
})))
## Add prey names to data frame
yp_spring_freq %<>% mutate(percent = V1,
                           prey_type = yp_spring_list,
                           species = "yellow perch",
                           season = "spring") %>%
  select(prey_type,percent,species,season)

##### WHITE PERCH #####
## Calculate the number of fish found with each prey taxa
wp_spring_n <- as.data.frame(do.call(rbind,lapply(wp_spring_list,function(i) {
  wp_spring %<>% filter(food_item == i)
  nrow(wp_spring)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
wp_spring_freq <- as.data.frame(do.call(rbind,lapply(1:nrow(wp_spring_n),function(j) {
  round(as.numeric(((wp_spring_n[j,1])/sum(wp_spring_n))*100),1)
})))

## Add prey names, fish species, and season to data frame
wp_spring_freq %<>% mutate(percent = V1,
                           prey_type = wp_spring_list,
                           species = "white perch",
                           season = "spring") %>%
  select(prey_type,percent,species,season)

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
  round(as.numeric(((yp_fall_n[j,1])/sum(yp_fall_n))*100),1)
})))

## Add prey names, fish species, and season to data frame
yp_fall_freq %<>% mutate(percent = V1,
                         prey_type = yp_fall_list,
                         species = "yellow perch",
                         season = "fall") %>%
  select(prey_type,percent,species,season)

##### WHITE PERCH #####
## Calculate the number of fish found with each prey taxa
wp_fall_n <- as.data.frame(do.call(rbind,lapply(wp_fall_list,function(i) {
  wp_fall %<>% filter(food_item == i)
  nrow(wp_fall)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
wp_fall_freq <- as.data.frame(do.call(rbind,lapply(1:nrow(wp_fall_n),function(j) {
  round(as.numeric(((wp_fall_n[j,1])/sum(wp_fall_n))*100),1)
})))
## Add prey names, fish species, and season to data frame
wp_fall_freq %<>% mutate(percent = V1,
                         prey_type = wp_fall_list,
                         species = "white perch",
                         season = "fall") %>%
  select(prey_type,percent,species,season)

## -----------------------------------------------------------
## Combine into a final data frame
## -----------------------------------------------------------
freq_comb <- data.frame(rbind(yp_spring_freq,wp_spring_freq,yp_fall_freq,wp_fall_freq))

## -----------------------------------------------------------
## Save table data into Excel spreadsheet
## -----------------------------------------------------------
wb <- loadWorkbook("Data/wb_percent_occurrence_table.xlsx",create=T)
createSheet(wb,name="Table")
writeWorksheet(wb,data=freq_comb,sheet="Table",startRow=1,startCol=1,header=TRUE)
saveWorkbook(wb)
rm(wb,freq_comb,n_full,n_empty)

## -----------------------------------------------------------
## Clean up environment
## -----------------------------------------------------------
rm(wp_fall_n,wp_spring_n,yp_fall,yp_fall_n,yp_spring,yp_spring_n,
   wp_fall_list,wp_spring_list,yp_fall_list,yp_spring_list,diet_fall,
   diet_spring,wp_fall_freq,wp_spring_freq,yp_fall_freq,yp_spring_freq,
   wp_fall,wp_spring)
