## Load Data
source("2014/Scripts/data_init.R")

diet_spring <- readWorksheet(db1,sheet="Calculated Data")
diet_fall <- readWorksheet(db2,sheet="Calculated Data")
diet_spring_full <- filter(diet_spring,!is.na(food_item))
diet_fall_full <- filter(diet_fall,!is.na(food_item))
diet_spring_empty <- filter(diet_spring,is.na(food_item))
diet_fall_empty <- filter(diet_fall,is.na(food_item))


n_full <- data.frame(cbind(length(unique(filter(diet_spring_full,species=="yellow perch")$fid)),
                           length(unique(filter(diet_fall_full,species=="yellow perch")$fid)),
                           length(unique(filter(diet_spring_full,species=="white perch")$fid)),
                           length(unique(filter(diet_fall_full,species=="white perch")$fid))))
colnames(n_full) <- c("yp_spring","yp_fall","wp_spring","wp_fall")

n_empty <- data.frame(cbind(length(unique(filter(diet_spring_empty,species=="yellow perch")$fid)),
                           length(unique(filter(diet_fall_empty,species=="yellow perch")$fid)),
                           length(unique(filter(diet_spring_empty,species=="white perch")$fid)),
                           length(unique(filter(diet_fall_empty,species=="white perch")$fid))))
colnames(n_empty) <- c("yp_spring","yp_fall","wp_spring","wp_fall")

rm(diet_spring_full,diet_spring_empty,diet_spring,diet_fall_full,diet_fall_empty,
   diet_fall)
