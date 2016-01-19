## Load Data
source("2015/Scripts/Walleye/data_init.R")

## -----------------------------------------------------------
## Remove stomachs with no diet contents
## -----------------------------------------------------------
walleye_diets %<>% filter(food_item != "Empty")

## -----------------------------------------------------------
## Merge prey type (matching prey taxa)
## -----------------------------------------------------------
walleye_diets %<>% left_join(prey_type) %>% arrange(type,food_item)
# Check for NA's in the new prey_type variable (NA = typos, inconsistency between prey names, etc.)

## -----------------------------------------------------------
## Filter into each species and both seasons
## -----------------------------------------------------------
walleye_list <- unique(walleye_diets$fid)

## -----------------------------------------------------------
## Create a list of prey types found for both seasons, uniquely combine, and order by functional group and alphabetically
## -----------------------------------------------------------
diet_list <- unique(walleye_diets$food_item)

## -----------------------------------------------------------
### Percent Dry Weight
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the mean percent by dry weight for each prey taxa
walleye_perc <- as.data.frame(do.call(cbind,lapply(walleye_list,function(i) {
  fish <- filter(walleye_diets,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet_list,function(j) {
    round(sum(as.numeric(filter(fish,food_item == j)$final_dw_mg))/sum(as.numeric(fish$final_dw_mg))*100,1)
  })))
})))

## Calculate row means
walleye_perc %<>% transform(mean = apply(walleye_perc,1,mean))

## Add prey names to data frame
walleye_perc %<>% transmute(percent = mean,
                              prey_type = diet_list)
                             

## -----------------------------------------------------------
## Merge prey type (matching prey taxa) - Used to summarize low dry weight species
## -----------------------------------------------------------
prey_type %<>% select(prey_type=food_item,type)
walleye_perc %<>% left_join(prey_type)
# Check for NA's in the new prey_type variable (NA's = typos, inconsistency between prey names, etc.)

## -----------------------------------------------------------
## Filter all values less than 5% and summarize by functional group into "Other Species"
## -----------------------------------------------------------
perc_more <- walleye_perc %>% filter(prey_type %in% c("Yellow Perch","Rainbow Smelt", "Gizzard Shad","Round Goby",
                                                        "Emerald Shiner","Cercopagididae","Leptodoridae","Unidentified fish")) # save to join back to later
perc_less <- walleye_perc %>% filter(prey_type %in% c("Amphipoda","Ephemeridae","Chironomidae","Nematoda","Oligochaeta"))
perc_less %<>% group_by(type) %>% 
  summarise(percent=sum(percent)) %>% ungroup() %>% 
  mutate(prey_type=paste(type,"spp.",sep=" "))
perc_less$prey_type <- gsub("Benthic Invertebrates","Benthos",perc_less$prey_type)

## -----------------------------------------------------------
## Join more and less data frames together
## -----------------------------------------------------------
perc_comb_final <- bind_rows(perc_more,perc_less) %>% 
  mutate(prey_type=factor(prey_type,levels = c('Cercopagididae','Leptodoridae','Benthos spp.','Emerald Shiner','Gizzard Shad','Rainbow Smelt','Round Goby','Yellow Perch','Unidentified fish'),ordered = TRUE)) %>% 
  arrange(prey_type)
taxa_list <- unique(perc_comb_final$prey_type)

## -----------------------------------------------------------
## Create a vector for plotting
## -----------------------------------------------------------
walleye_vector <- as.vector(perc_comb_final$percent)

## -----------------------------------------------------------
## Set some constants for plotting
## -----------------------------------------------------------
clr <- "gray35"
# number of rows and cols of actual plots
nrow <- 1
ncol <- 1
# sets the base width for each plot
basew <- 10
baseh <- basew*0.8

## -----------------------------------------------------------
## Save the plot as a figure (comment out line 186 and 226 until you are ready to save)
## -----------------------------------------------------------
png("2015/Figures/walleye_percent_dry_weight_bar.PNG",width=7,height=5,units="in",family="Times",res=300)

## -----------------------------------------------------------
## Make a base plot
## -----------------------------------------------------------
# make the layout
layout(rbind(cbind(rep(1,nrow),
                   matrix(3,nrow=nrow,byrow=FALSE)),
             c(0,rep(2,ncol))),
       widths=c(1,basew,rep(basew,ncol-1),0.5),
       heights=c(rep(baseh,nrow-1),baseh,0.5),
       respect=TRUE)
# put on some axis labels
par(mar=c(0,0,0,0))
plot.new(); text(0.9,0.575,"Diet Composition (% dry mass)",srt=90,cex=1.5)
plot.new(); text(0.5,0.5,"Prey Taxa",cex=1.4)

## ---------------------------------------------------
## Put on individual plots
## ---------------------------------------------------
  ## Top
par(mar=c(6.2,5.0,2.5,2.0),las=1,xaxs="i",yaxs="i") # individual plot margins mar=c(bottom,left,top,right)
barplot(walleye_vector,axes=F,ylim=c(0,60))
axis(1,0.55:length(taxa_list)*1.205,labels=F,tcl=-0.4,col="black",pos=0)
text(x=0.55:length(taxa_list)*1.205,y=-2.5,
     labels=taxa_list,srt=40,adj=1,xpd=TRUE,cex=1)
axis(2,seq(0,70,10),tcl=-0.3,col="black",cex.axis=1.3,pos=0.2)

## -----------------------------------------------------------
## Close the device to make the actual PNG file
## -----------------------------------------------------------
dev.off()
