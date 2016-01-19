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
## Filter into each species and both seasons
## -----------------------------------------------------------
yp_spring <- filter(diet_spring,species=="Yellow Perch")
yp_spring_list <- unique(yp_spring$fid)
wp_spring <- filter(diet_spring,species=="White Perch")
wp_spring_list <- unique(wp_spring$fid)
yp_fall <- filter(diet_fall,species=="Yellow Perch")
yp_fall_list <- unique(yp_fall$fid)
wp_fall <- filter(diet_fall,species=="White Perch")
wp_fall_list <- unique(wp_fall$fid)

## -----------------------------------------------------------
## Creat list of prey types
## -----------------------------------------------------------
taxa_list <- c("Zooplankton","Benthic Invertebrates","Fish")

## -----------------------------------------------------------
### SPRING
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the mean percent by dry weight for each prey taxa
yp_spring_perc <- as.data.frame(do.call(cbind,lapply(yp_spring_list,function(i) {
  fish <- filter(yp_spring,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(taxa_list,function(j) {
    round((sum(filter(fish,type == j)$final_dw_mg)/sum(fish$final_dw_mg))*100,1)
  })))
})))

## Calculate row means
yp_spring_perc %<>% transform(mean = apply(yp_spring_perc,1,mean))

## Add prey names to data frame
yp_spring_perc %<>% transmute(percent = mean,
                              prey_type = taxa_list,
                              species = "Yellow Perch",
                              season = "Spring")

##### WHITE PERCH #####
## Calculate the mean percent by dry weight for each prey taxa
wp_spring_perc <- as.data.frame(do.call(cbind,lapply(wp_spring_list,function(i) {
  fish <- filter(wp_spring,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(taxa_list,function(j) {
    round((sum(filter(fish,type == j)$final_dw_mg)/sum(fish$final_dw_mg))*100,1)
  })))
})))

## Calculate row means
wp_spring_perc %<>% transform(mean = apply(wp_spring_perc,1,mean))

## Add prey names to data frame
wp_spring_perc %<>% transmute(percent = mean,
                              prey_type = taxa_list,
                              species = "White Perch",
                              season = "Spring")

## -----------------------------------------------------------
### FALL
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the percent by dry weight for each prey taxa
yp_fall_perc <- as.data.frame(do.call(cbind,lapply(yp_fall_list,function(i) {
  fish <- filter(yp_fall,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(taxa_list,function(j) {
    round((sum(filter(fish,type == j)$final_dw_mg)/sum(fish$final_dw_mg))*100,1)
  })))
})))

## Calculate row means
yp_fall_perc %<>% transform(mean = apply(yp_fall_perc,1,mean))

## Add prey names to data frame
yp_fall_perc %<>% transmute(percent = mean,
                            prey_type = taxa_list,
                            species = "Yellow Perch",
                            season = "Fall")

##### WHITE PERCH #####
## Calculate the percent by dry weight for each prey taxa
wp_fall_perc <- as.data.frame(do.call(cbind,lapply(wp_fall_list,function(i) {
  fish <- filter(wp_fall,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(taxa_list,function(j) {
    round((sum(filter(fish,type == j)$final_dw_mg)/sum(fish$final_dw_mg))*100,1)
  })))
})))

## Calculate row means
wp_fall_perc %<>% transform(mean = apply(wp_fall_perc,1,mean))

## Add prey names to data frame
wp_fall_perc %<>% transmute(percent = mean,
                            prey_type = taxa_list,
                            species = "White Perch",
                            season = "Fall")

## -----------------------------------------------------------
## Combine into a final data frame
## -----------------------------------------------------------
perc_comb <- bind_rows(yp_spring_perc,wp_spring_perc,yp_fall_perc,wp_fall_perc)

## -----------------------------------------------------------
## Merge prey type (matching prey taxa).
## -----------------------------------------------------------
perc_comb %<>% group_by(prey_type,species,season) %>%
  summarise(type_sum = sum(percent)) %>% ungroup() %>% 
  mutate(prey_type=factor(prey_type,levels = c("Zooplankton","Benthic Invertebrates","Fish"),ordered=TRUE)) %>% 
  arrange(prey_type,species,desc(season))

## -----------------------------------------------------------
## Create a matrix for plotting
## -----------------------------------------------------------
yp_table <- data.frame(Spring = c(as.vector(
  filter(perc_comb,species=="Yellow Perch" & season=="Spring")$type_sum)),
  Fall = c(as.vector(
    filter(perc_comb,species=="Yellow Perch" & season=="Fall")$type_sum)))
yp_table <- do.call(cbind,yp_table)

wp_table <- data.frame(Spring = c(as.vector(
  filter(perc_comb,species=="White Perch" & season=="Spring")$type_sum)),
  Fall = c(as.vector(
    filter(perc_comb,species=="White Perch" & season=="Fall")$type_sum)))
wp_table <- do.call(cbind,wp_table)

## -----------------------------------------------------------
## Set some constants for plotting
## -----------------------------------------------------------
clr <- "gray35"
## number of rows and cols of actual plots
nrow <- 2
ncol <- 1
## sets the base width for each plot
basew <- 5
baseh <- basew*0.6

## -----------------------------------------------------------
## Save the plot as a figure (comment out line 144 and 182 until you are ready to save)
## -----------------------------------------------------------
png("2015/Figures/prey_type_bar.PNG",width=5,height=7,units="in",family="Times",res=300)

## -----------------------------------------------------------
## Make a base plot
## -----------------------------------------------------------
## make the layout
layout(rbind(cbind(rep(1,nrow),
                   matrix(3:4,nrow=nrow,byrow=FALSE)),
             c(0,rep(2,ncol))),
       widths=c(1,basew,rep(basew,ncol-1),1),
       heights=c(rep(baseh,nrow-1),baseh,1),
       respect=TRUE)
## put on some axis labels
par(mar=c(0,0,0,0))
plot.new(); text(0.5,0.5,"Diet Composition (% dry mass)",srt=90,cex=2.1)
plot.new(); legend("bottom",c("Zooplankton","Benthic Macroinvertebrates","Fish"),
                   fill=c("gray80","gray45","black"),cex=1.4)

## ---------------------------------------------------
## Put on individual plots
## ---------------------------------------------------
## Top
par(mar=c(3,1.5,2,1.5),xaxs="i",yaxs="i",las=1)
barplot(yp_table,beside=TRUE,xlim=c(0.5,8.5),axes=F,axisnames=F,legend.text = rownames(yp_table),ylim=c(0,80), 
        col=c("gray80","gray45","black"),args.legend=list(x = 10, bty="y"))
axis(1,c(0.5,4.5,8.5),labels=F,tcl=-0.3,col="gray55")
axis(2,seq(0,100,20),tcl=-0.6,col="gray55",cex.axis=1.75)

## Bottom
par(mar=c(3,1.5,2,1.5),xaxs="i",yaxs="i",las=1)
barplot(wp_table,beside=TRUE,xlim=c(0.5,8.5),cex.names=2,axes=F,ylim=c(0,80),
        col=c("gray80","gray45","black"))
axis(1,c(0.5,4.5,8.5),labels=F,tcl=-0.3,col="gray55")
axis(2,seq(0,100,20),tcl=-0.6,col="gray55",cex.axis=1.75)

## -----------------------------------------------------------
## Close the device to make the actual PDF file
## -----------------------------------------------------------
dev.off()

