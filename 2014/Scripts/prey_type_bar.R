## Load Data
source("2014/Scripts/data_init.R")

diet_spring <- readWorksheet(db1,sheet="Calculated Data")
diet_fall <- readWorksheet(db2,sheet="Calculated Data")
diet_spring <- diet_spring %>% filter(!is.na(food_item))
diet_fall <- diet_fall %>% filter(!is.na(food_item))
ptype <- readWorksheet(db1,sheet="Prey Type")

## -----------------------------------------------------------
## Filter into each species and both seasons
## -----------------------------------------------------------
yp_spring <- filter(diet_spring,species=="yellow perch")
yp_spring_list <- unique(yp_spring$fid)
wp_spring <- filter(diet_spring,species=="white perch")
wp_spring_list <- unique(wp_spring$fid)
yp_fall <- filter(diet_fall,species=="yellow perch")
yp_fall_list <- unique(yp_fall$fid)
wp_fall <- filter(diet_fall,species=="white perch")
wp_fall_list <- unique(wp_fall$fid)

## -----------------------------------------------------------
## Creat lists of prey types found for both species and season
## -----------------------------------------------------------
diet_spring_list <- unique(diet_spring$food_item)
diet_fall_list <- unique(diet_fall$food_item)
diet_list <- unique(c(diet_spring_list,diet_fall_list))

## -----------------------------------------------------------
### SPRING
## -----------------------------------------------------------
##### YELLOW PERCH #####

## Calculate the mean percent by dry weight for each prey taxa
yp_spring_perc <- as.data.frame(do.call(cbind,lapply(yp_spring_list,function(i) {
  fish <- filter(yp_spring,fid == i)
  test <- as.data.frame(do.call(rbind,lapply(diet_list,function(j) {
    round((sum(filter(fish,food_item == j)$dw_g)/sum(fish$dw_g))*100,1)
  })))
})))

##
yp_spring_perc <- transform(yp_spring_perc,mean = apply(yp_spring_perc,1,mean))


## Add prey names to data frame
yp_spring_perc %<>% mutate(percent = mean,
                           prey_type = diet_list,
                           species = "yellow perch",
                           season = "spring") %>%
  select(prey_type,percent,species,season)

##### WHITE PERCH #####

## Calculate the mean percent by dry weight for each prey taxa
wp_spring_perc <- as.data.frame(do.call(cbind,lapply(wp_spring_list,function(i) {
  fish <- filter(wp_spring,fid == i)
  test <- as.data.frame(do.call(rbind,lapply(diet_list,function(j) {
    round((sum(filter(fish,food_item == j)$dw_g)/sum(fish$dw_g))*100,1)
  })))
})))

##
wp_spring_perc <- transform(wp_spring_perc,mean = apply(wp_spring_perc,1,mean))


## Add prey names to data frame
wp_spring_perc %<>% mutate(percent = mean,
                           prey_type = diet_list,
                           species = "white perch",
                           season = "spring") %>%
  select(prey_type,percent,species,season)

## -----------------------------------------------------------
### FALL
## -----------------------------------------------------------
##### YELLOW PERCH #####

## Calculate the percent by dry weight for each prey taxa
yp_fall_perc <- as.data.frame(do.call(cbind,lapply(yp_fall_list,function(i) {
  fish <- filter(yp_fall,fid == i)
  test <- as.data.frame(do.call(rbind,lapply(diet_list,function(j) {
    round((sum(filter(fish,food_item == j)$dw_g)/sum(fish$dw_g))*100,1)
  })))
})))

##
yp_fall_perc <- transform(yp_fall_perc,mean = apply(yp_fall_perc,1,mean))


## Add prey names to data frame
yp_fall_perc %<>% mutate(percent = mean,
                         prey_type = diet_list,
                         species = "yellow perch",
                         season = "fall") %>%
  select(prey_type,percent,species,season)

##### WHITE PERCH #####

## Calculate the percent by dry weight for each prey taxa
## Calculate the mean percent by dry weight for each prey taxa
wp_fall_perc <- as.data.frame(do.call(cbind,lapply(wp_fall_list,function(i) {
  fish <- filter(wp_fall,fid == i)
  test <- as.data.frame(do.call(rbind,lapply(diet_list,function(j) {
    round((sum(filter(fish,food_item == j)$dw_g)/sum(fish$dw_g))*100,1)
  })))
})))

##
wp_fall_perc <- transform(wp_fall_perc,mean = apply(wp_fall_perc,1,mean))


## Add prey names to data frame
wp_fall_perc %<>% mutate(percent = mean,
                         prey_type = diet_list,
                         species = "white perch",
                         season = "fall") %>%
  select(prey_type,percent,species,season)

## -----------------------------------------------------------
## Combine into a final data frame
## -----------------------------------------------------------
perc_comb <- data.frame(rbind(yp_spring_perc,wp_spring_perc,yp_fall_perc,wp_fall_perc))

## -----------------------------------------------------------
## Merge prey type (matching prey taxa).
## -----------------------------------------------------------
perc_comb <- merge(perc_comb,ptype,by.x="prey_type",by.y="item")
perc_comb %<>% group_by(type,species,season) %>%
  summarise(type_sum = sum(percent))

## -----------------------------------------------------------
## Create a matrix for plotting
## -----------------------------------------------------------
yp_table <- data.frame(Spring = c(as.vector(
  filter(perc_comb,species=="yellow perch" & season=="spring")$type_sum)),
  Fall = c(as.vector(
    filter(perc_comb,species=="yellow perch" & season=="fall")$type_sum)))
yp_table <- do.call(cbind,yp_table)

wp_table <- data.frame(Spring = c(as.vector(
  filter(perc_comb,species=="white perch" & season=="spring")$type_sum)),
  Fall = c(as.vector(
    filter(perc_comb,species=="white perch" & season=="fall")$type_sum)))
wp_table <- do.call(cbind,wp_table)

## -----------------------------------------------------------
## Set some constants for plotting
## -----------------------------------------------------------
clr <- "gray35"
# number of rows and cols of actual plots
nrow <- 2
ncol <- 1
# sets the base width for each plot
basew <- 5
baseh <- basew*0.6

## -----------------------------------------------------------
## Make a base plot
## -----------------------------------------------------------
# make the layout
layout(rbind(cbind(rep(1,nrow),
                   matrix(3:4,nrow=nrow,byrow=FALSE)),
             c(0,rep(2,ncol))),
       widths=c(1,basew,rep(basew,ncol-1),1),
       heights=c(rep(baseh,nrow-1),baseh,1),
       respect=TRUE)
# put on some axis labels
par(mar=c(0,0,0,0))
plot.new(); text(0.5,0.5,"Diet Composition (% dry weight)",srt=90,cex=1.8)
plot.new(); legend("top",c("Benthic Invertebrates","Fish","Zooplankton"),
                   fill=c("gray80","gray45","black"),cex=1.4)

## ---------------------------------------------------
## Put on individual plots
## ---------------------------------------------------
## Top
par(mar=c(3,1.5,2,1))
barplot(yp_table,beside=TRUE,axes=F,axisnames=F,legend.text = rownames(yp_table),ylim=c(0,100), 
        col=c("gray80","gray45","black"),args.legend=list(x = 10, bty="y"))
axis(1,c(1,4.5,8),labels=F,tcl=0,col="gray55")
axis(2,seq(0,100,20),tcl=-0.3,col="gray55",cex.axis=1.2,pos=0.925)

## Bottom
par(mar=c(3,1.5,2,1))
barplot(wp_table,beside=TRUE,cex.names=1.3,axes=F,ylim=c(0,100),
        col=c("gray80","gray45","black"))
axis(1,c(1,4.5,8),labels=F,tcl=0,col="gray55",cex.axis=1.2)
axis(2,seq(0,100,20),tcl=-0.3,col="gray55",cex.axis=1.2,pos=0.925)

## -----------------------------------------------------------
## Clean up environment
## -----------------------------------------------------------
rm(yp_fall,yp_spring,diet_fall_list,diet_spring_list,
   diet_fall,diet_spring,wp_fall_perc,wp_spring_perc,yp_fall_perc,
   yp_spring_perc,wp_spring,wp_fall,ptype,perc_comb,wp_table,yp_table,
   baseh,basew,clr,diet_list,ncol,nrow,wp_fall_list,wp_spring_list,
   yp_fall_list,yp_spring_list)