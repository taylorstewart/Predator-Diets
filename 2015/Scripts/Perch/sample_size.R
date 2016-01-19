## Load Data
source("2015/Scripts/Perch/data_init.R")

diet_spring_full <- filter(diet_spring,food_item != "Empty")
diet_fall_full <- filter(diet_fall,food_item != "Empty")
diet_spring_empty <- filter(diet_spring,food_item == "Empty")
diet_fall_empty <- filter(diet_fall,food_item == "Empty")


n_full <- data.frame(cbind(length(unique(filter(diet_spring_full,species=="Yellow Perch")$fid)),
                           length(unique(filter(diet_fall_full,species=="Yellow Perch")$fid)),
                           length(unique(filter(diet_spring_full,species=="White Perch")$fid)),
                           length(unique(filter(diet_fall_full,species=="White Perch")$fid))))
colnames(n_full) <- c("yp_spring","yp_fall","wp_spring","wp_fall")

n_empty <- data.frame(cbind(length(unique(filter(diet_spring_empty,species=="Yellow Perch")$fid)),
                           length(unique(filter(diet_fall_empty,species=="Yellow Perch")$fid)),
                           length(unique(filter(diet_spring_empty,species=="White Perch")$fid)),
                           length(unique(filter(diet_fall_empty,species=="White Perch")$fid))))
colnames(n_empty) <- c("yp_spring","yp_fall","wp_spring","wp_fall")

unique(diet_spring_full$serial)
unique(diet_fall_full$serial)
