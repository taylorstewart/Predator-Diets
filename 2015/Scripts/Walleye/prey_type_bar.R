## Load Data
source("2015/Scripts/Walleye/data_init.R")

## -----------------------------------------------------------
## Remove stomachs with no diet contents
## -----------------------------------------------------------
walleye_diets %<>% filter(food_item != "Empty")

## -----------------------------------------------------------
## Merge prey type (matching prey taxa)
## -----------------------------------------------------------
walleye_diets %<>% left_join(prey_type)
# Check for NA's in the new prey_type variable (NA = typos, inconsistency between prey names, etc.)

## -----------------------------------------------------------
## Filter into each species and both seasons
## -----------------------------------------------------------
walleye_list <- unique(walleye_diets$fid)

## -----------------------------------------------------------
## Creat list of prey types
## -----------------------------------------------------------
taxa_list <- c("Zooplankton","Benthic Invertebrates","Fish")

## -----------------------------------------------------------
### SPRING
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the mean percent by dry weight for each prey taxa
walleye_perc <- as.data.frame(do.call(cbind,lapply(walleye_list,function(i) {
  fish <- filter(walleye_diets,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(taxa_list,function(j) {
    round((sum(filter(fish,type == j)$final_dw_mg)/sum(fish$final_dw_mg))*100,1)
  })))
})))

## Calculate row means
walleye_perc %<>% transform(mean = apply(walleye_perc,1,mean))

## Add prey names to data frame
walleye_perc %<>% transmute(percent = mean,
                              prey_type = taxa_list)

## -----------------------------------------------------------
## Merge prey type (matching prey taxa).
## -----------------------------------------------------------
walleye_perc %<>% group_by(prey_type) %>%
  summarise(type_sum = sum(percent)) %>% ungroup() %>% 
  mutate(prey_type=factor(prey_type,levels = c("Zooplankton","Benthic Invertebrates","Fish"),ordered=TRUE)) %>% 
  arrange(prey_type)

## -----------------------------------------------------------
## Create a matrix for plotting
## -----------------------------------------------------------
walleye_vector <- as.vector(walleye_perc$type_sum)

## -----------------------------------------------------------
## Set some constants for plotting
## -----------------------------------------------------------
clr <- "gray35"
## number of rows and cols of actual plots
nrow <- 1
ncol <- 1
## sets the base width for each plot
basew <- 5
baseh <- basew*1.1

## -----------------------------------------------------------
## Save the plot as a figure (comment out line 144 and 182 until you are ready to save)
## -----------------------------------------------------------
png("2015/Figures/walleye_prey_type_bar.PNG",width=5,height=6,units="in",family="Times",res=300)

## -----------------------------------------------------------
## Make a base plot
## -----------------------------------------------------------
## make the layout
layout(rbind(cbind(rep(1,nrow),
                   matrix(3,nrow=nrow,byrow=FALSE)),
             c(0,rep(2,ncol))),
       widths=c(1,basew,rep(basew,ncol-1),1),
       heights=c(rep(baseh,nrow-1),baseh,1),
       respect=TRUE)
## put on some axis labels
par(mar=c(0,0,0,0))
plot.new(); text(0.5,0.52,"Diet Composition (% dry mass)",srt=90,cex=1.9)
plot.new(); legend(0.122,1,c("Zooplankton","Benthic Macroinvertebrates","Fish"),
                   fill=c("gray80","gray45","black"),cex=1.1)

## ---------------------------------------------------
## Put on individual plots
## ---------------------------------------------------
## Top
par(mar=c(1,2,0,0),xaxs="i",yaxs="i",las=1)
barplot(walleye_vector,xlim=c(0,5),axes=F,axisnames=F,ylim=c(0,80),
        col=c("gray80","gray45","black"))
axis(1,c(0,3.85),labels=F,tcl=-0.3,col="gray55")
axis(2,seq(0,100,20),tcl=-0.6,col="gray55",cex.axis=1.5)

## -----------------------------------------------------------
## Close the device to make the actual PDF file
## -----------------------------------------------------------
dev.off()

