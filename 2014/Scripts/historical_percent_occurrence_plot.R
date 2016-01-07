## Load Data
source("2014/Scripts/data_init.R")

diet_spring <- readWorksheet(db1,sheet="Calculated Data")
diet_fall <- readWorksheet(db2,sheet="Calculated Data")
diet_spring %<>% filter(!is.na(food_item))
diet_fall %<>% filter(!is.na(food_item))
hist_perc <- readWorksheet(db3,sheet="data")
ptype <- readWorksheet(db1,sheet="Prey Type")

## -----------------------------------------------------------
## Merge prey type (matching prey taxa)
## -----------------------------------------------------------
diet_spring %<>% left_join(ptype)
diet_fall %<>% left_join(ptype) 
# Check for NA's in the new prey_type variable (NA = typos, inconsistency between prey names, etc.)

## -----------------------------------------------------------
## Filter into each species and restrict serials to only historical sites
## -----------------------------------------------------------
yp_spring <- filter(diet_spring,species=="yellow perch" & serial %in% c(101,103,107,111,115,117,118,119,122,123,124,127,128,132,133,137))
wp_spring <- filter(diet_spring,species=="white perch" & serial %in% c(101,103,107,111,115,117,118,119,122,123,124,127,128,132,133,137))
yp_fall <- filter(diet_fall,species=="yellow perch" & serial %in% c(501,503,507,511,515,517,518,519,522,523,524,527,528,532,533,537))
wp_fall <- filter(diet_fall,species=="white perch" & serial %in% c(501,503,507,511,515,517,518,519,522,523,524,527,528,532,533,537))

## -----------------------------------------------------------
## Creat lists of prey types found for both species and season
## -----------------------------------------------------------
yp_spring_list <- unique(yp_spring$food_item)
wp_spring_list <- unique(wp_spring$food_item)
yp_fall_list <- unique(yp_fall$food_item)
wp_fall_list <- unique(wp_fall$food_item)

taxa_list <- c("Zooplankton","Benthic Invertebrates","Fish")

## -----------------------------------------------------------
### SPRING
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the number of fish found with each prey taxa
yp_spring_n <- as.data.frame(do.call(rbind,lapply(taxa_list,function(i) {
  yp_spring %<>% filter(type == i)
  nrow(yp_spring)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
yp_spring_perc <- as.data.frame(do.call(rbind,lapply(1:nrow(yp_spring_n),function(j) {
  round(as.numeric(((yp_spring_n[j,1])/length(unique(yp_spring$fid)))*100),1)
})))

## Add prey names, fish species, and season to data frame
yp_spring_perc %<>% mutate(percent = V1,
                           type = taxa_list,
                           species = "yellow perch",
                           year = "2014",
                           season = "spring") %>%
  select(type,percent,species,year,season)

##### WHITE PERCH #####
## Calculate the number of fish found with each prey taxa
wp_spring_n <- as.data.frame(do.call(rbind,lapply(taxa_list,function(i) {
  wp_spring %<>% filter(type == i)
  nrow(wp_spring)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
wp_spring_perc <- as.data.frame(do.call(rbind,lapply(1:nrow(wp_spring_n),function(j) {
  round(as.numeric(((wp_spring_n[j,1])/length(unique(wp_spring$fid)))*100),1)
})))

## Add prey names, fish species, and season to data frame
wp_spring_perc %<>% mutate(percent = V1,
                           type = taxa_list,
                           species = "white perch",
                           year = "2014",
                           season = "spring") %>%
  select(type,percent,species,year,season)

## -----------------------------------------------------------
### FALL
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the number of fish found with each prey taxa
yp_fall_n <- as.data.frame(do.call(rbind,lapply(taxa_list,function(i) {
  yp_fall %<>% filter(type == i)
  nrow(yp_fall)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
yp_fall_perc <- as.data.frame(do.call(rbind,lapply(1:nrow(yp_fall_n),function(j) {
  round(as.numeric(((yp_fall_n[j,1])/length(unique(yp_fall$fid)))*100),1)
})))

## Add prey names, fish species, and season to data frame
yp_fall_perc %<>% mutate(percent = V1,
                         type = taxa_list,
                         species = "yellow perch",
                         year = "2014",
                         season = "fall") %>%
  select(type,percent,species,year,season)

##### WHITE PERCH #####
## Calculate the number of fish found with each prey taxa
wp_fall_n <- as.data.frame(do.call(rbind,lapply(taxa_list,function(i) {
  wp_fall %<>% filter(type == i)
  nrow(wp_fall)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
wp_fall_perc <- as.data.frame(do.call(rbind,lapply(1:nrow(wp_fall_n),function(j) {
  round(as.numeric(((wp_fall_n[j,1])/length(unique(wp_fall$fid)))*100),1)
})))
## Add prey names, fish species, and season to data frame
wp_fall_perc %<>% mutate(percent = V1,
                         type = taxa_list,
                         species = "white perch",
                         year = "2014",
                         season = "fall") %>%
  select(type,percent,species,year,season)

## -----------------------------------------------------------
## Combine into a final data frame
## -----------------------------------------------------------
perc_comb <- data.frame(rbind(yp_spring_perc,wp_spring_perc,yp_fall_perc,wp_fall_perc))

## -----------------------------------------------------------
## Merge prey type (matching prey taxa).
## -----------------------------------------------------------
perc_comb %<>% group_by(type,species,year,season) %>%
  summarise(type_sum = sum(percent))

## -----------------------------------------------------------
## Combine historical data with current data.
## -----------------------------------------------------------
hist_perc <- rbind(hist_perc,perc_comb)

## -----------------------------------------------------------
### A helper function for plotting
## -----------------------------------------------------------
histPlot <- function(df,taxa_type,seas,xlim,ylim,clr) {
  tmp <- filter(df,type==taxa_type & species=="yellow perch" & season==seas)
  tmp2 <- filter(df,type==taxa_type & species=="white perch" & season==seas)
  plot(type_sum~year,data=tmp,type="l",lwd=2,col=clr,
       ylim=ylim,xlim=xlim,xaxt="n",yaxt="n",xlab="",ylab="",main="",axes=F)
  points(type_sum~year,data=tmp,pch=16,cex=1.25)
  lines(type_sum~year,data=tmp2,lty=2,lwd=2,col="gray70")  
  points(type_sum~year,data=tmp2,pch=1,cex=1.25)
}

## -----------------------------------------------------------
## Set some constants for plotting
## -----------------------------------------------------------
clr <- "gray35"
xlmt <- c(as.numeric(min(hist_perc$year))-0.2,as.numeric(max(hist_perc$year))+0.2)
ylmt <- c(0,102)
year_ticks <- seq(as.numeric(min(hist_perc$year)),as.numeric(max(hist_perc$year,1)))
freq_ticks <- seq(0,100,25)
# number of rows and cols of actual plots
nrow <- 3
ncol <- 2
# sets the base width for each plot
basew <- 5
baseh <- basew*0.6

## -----------------------------------------------------------
## Make a base plot
## -----------------------------------------------------------
# make the layout
layout(rbind(cbind(rep(1,nrow),
                   matrix(3:8,nrow=nrow,byrow=FALSE)),
             c(0,rep(2,ncol))),
       widths=c(1,basew,rep(basew,ncol-1),1),
       heights=c(rep(baseh,nrow-1),baseh,1),
       respect=TRUE)
# put on some axis labels
par(mar=c(0,0,0,0))
plot.new(); text(0.7,0.517,"Frequency of Occurrence (%)",srt=90,cex=1.6)
plot.new(); text(0.51,0.7,"Year",cex=1.6)

## ---------------------------------------------------
## Put on individual plots
## ---------------------------------------------------
## Top-left
par(mar=c(2.5,3.5,1,1))
histPlot(hist_perc,"Zooplankton","spring",xlmt,ylmt,clr)
axis(1,year_ticks,labels=F,tcl=-0.3,col="gray55")
axis(2,freq_ticks,labels=F,tcl=-0.3,col="gray55",cex.axis=1)
text(x=2004.2,y=freq_ticks,
     labels=freq_ticks,srt=0,adj=1,xpd=TRUE,cex=1.25)

## Middle-left
par(mar=c(2.5,3.5,1,1))
histPlot(hist_perc,"Benthic Invertebrates","spring",xlmt,ylmt,clr)
axis(1,year_ticks,labels=F,tcl=-0.3,col="gray55")
axis(2,freq_ticks,labels=F,tcl=-0.3,col="gray55",cex.axis=1)
text(x=2004.2,y=freq_ticks,
     labels=freq_ticks,srt=0,adj=1,xpd=TRUE,cex=1.25)

#Bottom-left
par(mar=c(2.5,3.5,1,1))
histPlot(hist_perc,"Fish","spring",xlmt,ylmt,clr)
axis(1,year_ticks,labels=F,tcl=-0.3,col="gray55")
text(x=year_ticks+.3,y=-10,
     labels=year_ticks,srt=0,adj=1,xpd=TRUE,cex=1)
axis(2,freq_ticks,labels=F,tcl=-0.3,col="gray55",cex.axis=1)
text(x=2004.2,y=freq_ticks,
     labels=freq_ticks,srt=0,adj=1,xpd=TRUE,cex=1.25)

## Top-right
par(mar=c(2.5,3.5,1,1))
histPlot(hist_perc,"Zooplankton","fall",xlmt,ylmt,clr)
axis(1,year_ticks,labels=F,tcl=-0.3,col="gray55")
axis(2,freq_ticks,labels=F,tcl=-0.3,col="gray55",cex.axis=1)

## Midlle-right
par(mar=c(2.5,3.5,1,1))
histPlot(hist_perc,"Benthic Invertebrates","fall",xlmt,ylmt,clr)
axis(1,year_ticks,labels=F,tcl=-0.3,col="gray55")
axis(2,freq_ticks,labels=F,tcl=-0.3,col="gray55",cex.axis=1)

## Bottom-right
par(mar=c(2.5,3.5,1,1))
histPlot(hist_perc,"Fish","fall",xlmt,ylmt,clr)
axis(1,year_ticks,labels=F,tcl=-0.3,col="gray55")
text(x=year_ticks+.3,y=-10,
     labels=year_ticks,srt=0,adj=1,xpd=TRUE,cex=1)
axis(2,freq_ticks,labels=F,tcl=-0.3,col="gray55",cex.axis=1)

## -----------------------------------------------------------
## Clean up environment
## -----------------------------------------------------------
rm(wp_fall_n,wp_spring_n,yp_fall,yp_fall_n,yp_spring,yp_spring_n,wp_fall_list,
   wp_spring_list,yp_fall_list,yp_spring_list,diet_fall,diet_spring,
   wp_fall_perc,wp_spring_perc,yp_fall_perc,yp_spring_perc,perc_comb,ptype,
   wp_fall,wp_spring,wp_fish_spring_zero,baseh,basew,clr,freq_ticks,ncol,
   nrow,xlmt,year_ticks,ylmt,hist_perc,histPlot)
