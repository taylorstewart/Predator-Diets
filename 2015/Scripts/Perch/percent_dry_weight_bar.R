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
diet_spring %<>% left_join(prey_type) %>% arrange(type,food_item)
diet_fall %<>% left_join(prey_type) %>% arrange(type,food_item)
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
## Create a list of prey types found for both seasons, uniquely combine, and order by functional group and alphabetically
## -----------------------------------------------------------
diet_spring_list <- distinct(diet_spring,food_item)
diet_fall_list <- distinct(diet_fall,food_item)
diet_list <- bind_rows(diet_spring_list,diet_fall_list) %>% 
  arrange(type,food_item) %>% 
  distinct(food_item) %>% 
  select(food_item)
diet_list <- as.character(diet_list$food_item)

## -----------------------------------------------------------
### SPRING
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the mean percent by dry weight for each prey taxa
yp_spring_perc <- as.data.frame(do.call(cbind,lapply(yp_spring_list,function(i) {
  fish <- filter(yp_spring,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet_list,function(j) {
    round((sum(filter(fish,food_item == j)$final_dw_mg)/sum(fish$final_dw_mg))*100,1)
  })))
})))

## Calculate row means
yp_spring_perc %<>% transform(mean = apply(yp_spring_perc,1,mean))

## Add prey names to data frame
yp_spring_perc %<>% transmute(percent = mean,
                              prey_type = diet_list,
                              species = "Yellow Perch",
                              season = "Spring")

##### WHITE PERCH #####
## Calculate the mean percent by dry weight for each prey taxa
wp_spring_perc <- as.data.frame(do.call(cbind,lapply(wp_spring_list,function(i) {
  fish <- filter(wp_spring,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet_list,function(j) {
    round((sum(filter(fish,food_item == j)$final_dw_mg)/sum(fish$final_dw_mg))*100,1)
  })))
})))

## Calculate row means
wp_spring_perc %<>% transform(mean = apply(wp_spring_perc,1,mean))

## Add prey names to data frame
wp_spring_perc %<>% transmute(percent = mean,
                              prey_type = diet_list,
                              species = "White Perch",
                              season = "Spring")

## -----------------------------------------------------------
### FALL
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the percent by dry weight for each prey taxa
yp_fall_perc <- as.data.frame(do.call(cbind,lapply(yp_fall_list,function(i) {
  fish <- filter(yp_fall,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet_list,function(j) {
    round((sum(filter(fish,food_item == j)$final_dw_mg)/sum(fish$final_dw_mg))*100,1)
  })))
})))

## Calculate row means
yp_fall_perc %<>% transform(mean = apply(yp_fall_perc,1,mean))

## Add prey names to data frame
yp_fall_perc %<>% transmute(percent = mean,
                            prey_type = diet_list,
                            species = "Yellow Perch",
                            season = "Autumn")

##### WHITE PERCH #####
## Calculate the percent by dry weight for each prey taxa
wp_fall_perc <- as.data.frame(do.call(cbind,lapply(wp_fall_list,function(i) {
  fish <- filter(wp_fall,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet_list,function(j) {
    round((sum(filter(fish,food_item == j)$final_dw_mg)/sum(fish$final_dw_mg))*100,1)
  })))
})))

## Calculate row means
wp_fall_perc %<>% transform(mean = apply(wp_fall_perc,1,mean))

## Add prey names to data frame
wp_fall_perc %<>% transmute(percent = mean,
                            prey_type = diet_list,
                            species = "White Perch",
                            season = "Autumn")

## -----------------------------------------------------------
## Combine into a final data frame
## -----------------------------------------------------------
perc_comb <- data.frame(rbind(yp_spring_perc,wp_spring_perc,yp_fall_perc,wp_fall_perc))

## -----------------------------------------------------------
## Merge prey type (matching prey taxa) - Used to summarize low dry weight species
## -----------------------------------------------------------
prey_type %<>% select(prey_type=food_item,type)
perc_comb %<>% left_join(prey_type)
# Check for NA's in the new prey_type variable (NA's = typos, inconsistency between prey names, etc.)

## -----------------------------------------------------------
## Filter all values less than 5% and summarize by functional group into "Other Species"
## -----------------------------------------------------------
perc_comb_more <- perc_comb %>% filter(prey_type %in% c("Amphipoda","Ephemeridae","Round Goby",
                                                        "Yellow Perch","Daphnidae","Chironomidae",
                                                        "Trichoptera","Dreissenidae",
                                                        "Emerald Shiner","Cercopagididae","Gastropoda",
                                                        "Leptodoridae","Unidentified fish")) # save to join back to later
perc_comb_less <- perc_comb %>% filter(prey_type %in% c("Sphaeriidae","Hirudinea","Spottail Shiner",
                                                        "Bosminidae","Nematoda","Sididae",
                                                        "Oligochaeta","Fish eggs","White Bass",
                                                        "Calanoida","Ostracoda","Gizzard Shad",
                                                        "White Perch","Cyclopoida","Hemimysis"))
perc_comb_less %<>% group_by(species,season,type) %>% 
  summarise(percent=sum(percent)) %>% ungroup() %>% 
  mutate(prey_type=paste(type,"spp.",sep=" "))
perc_comb_less$prey_type <- gsub("Benthic Invertebrates","Benthos",perc_comb_less$prey_type)

## -----------------------------------------------------------
## Join more and less data frames together
## -----------------------------------------------------------
perc_comb_final <- bind_rows(perc_comb_more,perc_comb_less) %>% 
  mutate(prey_type=factor(prey_type,levels = c('Cercopagididae','Daphnidae','Leptodoridae','Zooplankton spp.','Amphipoda','Chironomidae','Dreissenidae','Gastropoda','Hemimysis','Ephemeridae','Trichoptera','Benthos spp.','Emerald Shiner','Round Goby','Yellow Perch','Unidentified fish','Fish spp.'),ordered = TRUE)) %>% 
  arrange(prey_type)
taxa_list <- unique(perc_comb_final$prey_type)

## -----------------------------------------------------------
## Create a matrix of each species for plotting
## -----------------------------------------------------------
yp_table <- data.frame(Spring = c(as.vector(
  filter(perc_comb_final,species=="Yellow Perch" & season=="Spring")$percent)),
  Autumn = c(as.vector(
  filter(perc_comb_final,species=="Yellow Perch" & season=="Autumn")$percent)))
yp_table <- do.call(rbind,yp_table)

wp_table <- data.frame(Spring = c(as.vector(
  filter(perc_comb_final,species=="White Perch" & season=="Spring")$percent)),
  Autumn = c(as.vector(
    filter(perc_comb_final,species=="White Perch" & season=="Autumn")$percent)))
wp_table <- do.call(rbind,wp_table)

## -----------------------------------------------------------
## Set some constants for plotting
## -----------------------------------------------------------
clr <- "gray35"
# number of rows and cols of actual plots
nrow <- 2
ncol <- 1
# sets the base width for each plot
basew <- 10
baseh <- basew*0.4

## -----------------------------------------------------------
## Save the plot as a figure (comment out line 186 and 226 until you are ready to save)
## -----------------------------------------------------------
png("2015/Figures/percent_dry_weight_bar.PNG",width=11,height=8,units="in",family="Times",res=300)

## -----------------------------------------------------------
## Make a base plot
## -----------------------------------------------------------
# make the layout
layout(rbind(cbind(rep(1,nrow),
                   matrix(3:4,nrow=nrow,byrow=FALSE)),
             c(0,rep(2,ncol))),
       widths=c(1,basew,rep(basew,ncol-1),0.5),
       heights=c(rep(baseh,nrow-1),baseh,0.5),
       respect=TRUE)
# put on some axis labels
par(mar=c(0,0,0,0))
plot.new(); text(0.7,0.5,"Diet Composition (% dry mass)",srt=90,cex=2.5)
plot.new(); text(0.5,0.5,"Prey Taxa",cex=2)

## ---------------------------------------------------
## Put on individual plots
## ---------------------------------------------------
  ## Top
par(mar=c(6.2,5.0,2.5,2.0),las=1,xaxs="i",yaxs="i") # individual plot margins mar=c(bottom,left,top,right)
barplot(yp_table,beside=TRUE,axes=F,legend.text=rownames(yp_table), 
        args.legend=list(x=48,y=70,bty="n",cex=2),ylim=c(0,70))
axis(1,0.66:length(taxa_list)*3,labels=F,tcl=-0.4,col="gray55",pos=-0.2)
text(x=0.7:length(taxa_list)*3,y=-2.5,
     labels=taxa_list,srt=40,adj=1,xpd=TRUE,cex=1.3)
axis(2,seq(0,70,10),tcl=-0.3,col="gray55",cex.axis=2,pos=0.6)

## Bottom
par(mar=c(6.2,5.0,2.5,2.0),las=1,xaxs="i",yaxs="i")
barplot(wp_table,beside=TRUE,axes=F,ylim=c(0,70))
axis(1,0.66:length(taxa_list)*3,labels=F,tcl=-0.4,col="gray55",pos=-0.3)
text(x=0.7:length(taxa_list)*3,y=-2.5,
     labels=taxa_list,srt=40,adj=1,xpd=TRUE,cex=1.3)
axis(2,seq(0,70,10),tcl=-0.3,col="gray55",cex.axis=2,pos=0.6)

## -----------------------------------------------------------
## Close the device to make the actual PNG file
## -----------------------------------------------------------
dev.off()
