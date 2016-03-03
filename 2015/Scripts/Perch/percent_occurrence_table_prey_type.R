## Load Data
source("2015/Scripts/Perch/data_init.R")

## -----------------------------------------------------------
## Remove stomachs with no diet contents
## -----------------------------------------------------------
diet_spring %<>% filter(food_item != "Empty")
diet_fall %<>% filter(food_item != "Empty")

## -----------------------------------------------------------
## Merge prey type (matching prey taxa)
## -----------------------------------------------------------
diet_spring %<>% left_join(prey_type)
diet_fall %<>% left_join(prey_type)
# Check for NA's in the new prey_type variable (NA = typos, inconsistency between prey names, etc.)

## -----------------------------------------------------------
## Filter into each species
## -----------------------------------------------------------
yp_spring <- filter(diet_spring,species=="Yellow Perch")
wp_spring <- filter(diet_spring,species=="White Perch")
yp_fall <- filter(diet_fall,species=="Yellow Perch")
wp_fall <- filter(diet_fall,species=="White Perch")

## -----------------------------------------------------------
## Creat list of prey types
## -----------------------------------------------------------
taxa_list <- c("Zooplankton","Benthic Invertebrates","Fish")

## -----------------------------------------------------------
### SPRING
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the number of fish found with each prey taxa
yp_spring_n <- as.data.frame(do.call(rbind,lapply(taxa_list,function(i) {
  yp_spring %<>% filter(type == i) %>% distinct(fid)
  nrow(yp_spring)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
yp_spring_perc <- as.data.frame(do.call(rbind,lapply(1:nrow(yp_spring_n),function(j) {
  round(as.numeric(((yp_spring_n[j,1])/length(unique(yp_spring$fid)))*100),1)
})))

## Add prey names, fish species, and season to data frame
yp_spring_perc %<>% transmute(type = taxa_list,
                              percent = V1,
                              species = "Yellow Perch",
                              year = 2015,
                              season = "Spring")

##### WHITE PERCH #####
## Calculate the number of fish found with each prey taxa
wp_spring_n <- as.data.frame(do.call(rbind,lapply(taxa_list,function(i) {
  wp_spring %<>% filter(type == i) %>% distinct(fid)
  nrow(wp_spring)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
wp_spring_perc <- as.data.frame(do.call(rbind,lapply(1:nrow(wp_spring_n),function(j) {
  round(as.numeric(((wp_spring_n[j,1])/length(unique(wp_spring$fid)))*100),1)
})))

## Add prey names, fish species, and season to data frame
wp_spring_perc %<>% transmute(type = taxa_list,
                              percent = V1,
                              species = "White Perch",
                              year = 2015,
                              season = "Spring")

## -----------------------------------------------------------
### FALL
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the number of fish found with each prey taxa
yp_fall_n <- as.data.frame(do.call(rbind,lapply(taxa_list,function(i) {
  yp_fall %<>% filter(type == i) %>% distinct(fid)
  nrow(yp_fall)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
yp_fall_perc <- as.data.frame(do.call(rbind,lapply(1:nrow(yp_fall_n),function(j) {
  round(as.numeric(((yp_fall_n[j,1])/length(unique(yp_fall$fid)))*100),1)
})))

## Add prey names, fish species, and season to data frame
yp_fall_perc %<>% transmute(type = taxa_list,
                            percent = V1,
                            species = "Yellow Perch",
                            year = 2015,
                            season = "Fall")

##### WHITE PERCH #####
## Calculate the number of fish found with each prey taxa
wp_fall_n <- as.data.frame(do.call(rbind,lapply(taxa_list,function(i) {
  wp_fall %<>% filter(type == i) %>% distinct(fid)
  nrow(wp_fall)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
wp_fall_perc <- as.data.frame(do.call(rbind,lapply(1:nrow(wp_fall_n),function(j) {
  round(as.numeric(((wp_fall_n[j,1])/length(unique(wp_fall$fid)))*100),1)
})))

## Add prey names, fish species, and season to data frame
wp_fall_perc %<>% transmute(type = taxa_list,
                            percent = V1,
                            species = "White Perch",
                            year = 2015,
                            season = "Fall")

## -----------------------------------------------------------
## Combine into a final data frame
## -----------------------------------------------------------
perc_comb <- bind_rows(yp_spring_perc,wp_spring_perc,yp_fall_perc,wp_fall_perc)
perc_comb
