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
## Filter into each species and restrict serials to only historical sites
## -----------------------------------------------------------
yp_spring <- filter(diet_spring,species=="Yellow Perch" & serial %in% c(101,103,107,111,115,117,118,119,122,123,124,127,128,132,133,137))
wp_spring <- filter(diet_spring,species=="White Perch" & serial %in% c(101,103,107,111,115,117,118,119,122,123,124,127,128,132,133,137))
yp_fall <- filter(diet_fall,species=="Yellow Perch" & serial %in% c(501,503,507,511,515,517,518,519,522,523,524,527,528,532,533,537))
wp_fall <- filter(diet_fall,species=="White Perch" & serial %in% c(501,503,507,511,515,517,518,519,522,523,524,527,528,532,533,537))

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
  yp_spring %<>% filter(type == i)
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
  wp_spring %<>% filter(type == i)
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
  yp_fall %<>% filter(type == i)
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
  wp_fall %<>% filter(type == i)
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

## -----------------------------------------------------------
## Summarize by prey type for each season, year, and species
## -----------------------------------------------------------
perc_comb %<>% group_by(type,species,year,season) %>%
  summarise(type_sum = sum(percent))

## -----------------------------------------------------------
## Combine historical data with current data.
## -----------------------------------------------------------
hist_perc <- bind_rows(hist_perc,perc_comb)

## -----------------------------------------------------------
## Set some constants for plotting
## -----------------------------------------------------------
clr <- "gray35"
xlmt <- c(as.numeric(min(hist_perc$year))-0.2,as.numeric(max(hist_perc$year))+0.2)
ylmt <- c(0,250)
year_ticks <- seq(as.numeric(min(hist_perc$year)),as.numeric(max(hist_perc$year)),2)
year_ticks_2 <- seq(as.numeric(min(hist_perc$year)),as.numeric(max(hist_perc$year)),1)
freq_ticks <- seq(0,250,50)
# number of rows and cols of actual plots
nrow <- 3
ncol <- 2
# sets the base width for each plot
basew <- 5
baseh <- basew*0.4

## -----------------------------------------------------------
## Save the plot as a figure (comment out line 144 and 182 until you are ready to save)
## -----------------------------------------------------------
png("2015/Figures/historical_percent_occurence.PNG",width=11,height=8,units="in",family="Times",res=300)

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
plot.new(); text(0.8,0.5,"Percent Occurrence",srt=90,cex=2.75)
plot.new(); text(0.5,0.6,"Year",cex=2.75)

## ---------------------------------------------------
## Put on individual plots
## ---------------------------------------------------
## Top-left
par(mar=c(2.25,5,1,2),xaxs="i",yaxs="i",las=1)
plot(type_sum~year,data=filter(hist_perc,type=="Zooplankton" & species=="Yellow Perch" & season=="Spring"),
     type="l",lwd=2,col=clr,ylim=ylmt,xlim=xlmt,xaxt="n",yaxt="n",xlab="",ylab="",main="",axes=F)
points(type_sum~year,data=filter(hist_perc,type=="Zooplankton" & species=="Yellow Perch" & season=="Spring"),
       pch=16,cex=1.25)
lines(type_sum~year,data=filter(hist_perc,type=="Zooplankton" & species=="White Perch" & season=="Spring"),
      lty=2,lwd=2,col="gray55")  
points(type_sum~year,data=filter(hist_perc,type=="Zooplankton" & species=="White Perch" & season=="Spring"),
       pch=1,cex=1.25,col="gray55")
axis(1,year_ticks,labels=F,tcl=-0.35,col="gray55")
axis(2,freq_ticks,labels=F,tcl=-0.35,col="gray55")
text(x=2004.4,y=freq_ticks,
     labels=freq_ticks,srt=0,adj=1,xpd=TRUE,cex=1.6)
axis(1,year_ticks_2,labels=F,tcl=-0.35,col="gray55")

## Middle-left
par(mar=c(2.25,5,1,2),xaxs="i",yaxs="i",las=1)
plot(type_sum~year,data=filter(hist_perc,type=="Benthic Invertebrates" & species=="Yellow Perch" & season=="Spring"),
     type="l",lwd=2,col=clr,ylim=ylmt,xlim=xlmt,xaxt="n",yaxt="n",xlab="",ylab="",main="",axes=F)
points(type_sum~year,data=filter(hist_perc,type=="Benthic Invertebrates" & species=="Yellow Perch" & season=="Spring"),
       pch=16,cex=1.25)
lines(type_sum~year,data=filter(hist_perc,type=="Benthic Invertebrates" & species=="White Perch" & season=="Spring"),
      lty=2,lwd=2,col="gray55")  
points(type_sum~year,data=filter(hist_perc,type=="Benthic Invertebrates" & species=="White Perch" & season=="Spring"),
       pch=1,cex=1.25,col="gray55")
axis(1,year_ticks,labels=F,tcl=-0.35,col="gray55")
axis(2,freq_ticks,labels=F,tcl=-0.35,col="gray55")
text(x=2004.4,y=freq_ticks,
     labels=freq_ticks,srt=0,adj=1,xpd=TRUE,cex=1.6)
axis(1,year_ticks_2,labels=F,tcl=-0.35,col="gray55")

#Bottom-left
par(mar=c(2.25,5,1,2),xaxs="i",yaxs="i",las=1)
plot(type_sum~year,data=filter(hist_perc,type=="Fish" & species=="Yellow Perch" & season=="Spring"),
     type="l",lwd=2,col=clr,ylim=ylmt,xlim=xlmt,xaxt="n",yaxt="n",xlab="",ylab="",main="",axes=F)
points(type_sum~year,data=filter(hist_perc,type=="Fish" & species=="Yellow Perch" & season=="Spring"),
       pch=16,cex=1.25)
lines(type_sum~year,data=filter(hist_perc,type=="Fish" & species=="White Perch" & season=="Spring"),
      lty=2,lwd=2,col="gray55")  
points(type_sum~year,data=filter(hist_perc,type=="Fish" & species=="White Perch" & season=="Spring"),
       pch=1,cex=1.25,col="gray55")
axis(1,year_ticks,labels=F,tcl=-0.35,col="gray55")
text(x=year_ticks+0.47,y=-30,
     labels=year_ticks,srt=0,adj=1,xpd=TRUE,cex=1.6)
axis(2,freq_ticks,labels=F,tcl=-0.35,col="gray55")
text(x=2004.4,y=freq_ticks,
     labels=freq_ticks,srt=0,adj=1,xpd=TRUE,cex=1.6)
axis(1,year_ticks_2,labels=F,tcl=-0.35,col="gray55")

## Top-right
par(mar=c(2.25,2,1,5),xaxs="i",yaxs="i",las=1)
plot(type_sum~year,data=filter(hist_perc,type=="Zooplankton" & species=="Yellow Perch" & season=="Fall"),
     type="l",lwd=2,col=clr,ylim=ylmt,xlim=xlmt,xaxt="n",yaxt="n",xlab="",ylab="",main="",axes=F)
points(type_sum~year,data=filter(hist_perc,type=="Zooplankton" & species=="Yellow Perch" & season=="Fall"),
       pch=16,cex=1.25)
lines(type_sum~year,data=filter(hist_perc,type=="Zooplankton" & species=="White Perch" & season=="Fall"),
      lty=2,lwd=2,col="gray55")  
points(type_sum~year,data=filter(hist_perc,type=="Zooplankton" & species=="White Perch" & season=="Fall"),
       pch=1,cex=1.25,col="gray55")
axis(1,year_ticks,labels=F,tcl=-0.35,col="gray55")
axis(2,freq_ticks,labels=F,tcl=-0.35,col="gray55")
axis(1,year_ticks_2,labels=F,tcl=-0.35,col="gray55")

## Midlle-right
par(mar=c(2.25,2,1,5),xaxs="i",yaxs="i",las=1)
plot(type_sum~year,data=filter(hist_perc,type=="Benthic Invertebrates" & species=="Yellow Perch" & season=="Fall"),
     type="l",lwd=2,col=clr,ylim=ylmt,xlim=xlmt,xaxt="n",yaxt="n",xlab="",ylab="",main="",axes=F)
points(type_sum~year,data=filter(hist_perc,type=="Benthic Invertebrates" & species=="Yellow Perch" & season=="Fall"),
       pch=16,cex=1.25)
lines(type_sum~year,data=filter(hist_perc,type=="Benthic Invertebrates" & species=="White Perch" & season=="Fall"),
      lty=2,lwd=2,col="gray55")  
points(type_sum~year,data=filter(hist_perc,type=="Benthic Invertebrates" & species=="White Perch" & season=="Fall"),
       pch=1,cex=1.25,col="gray55")
axis(1,year_ticks,labels=F,tcl=-0.35,col="gray55")
axis(2,freq_ticks,labels=F,tcl=-0.35,col="gray55")
axis(1,year_ticks_2,labels=F,tcl=-0.35,col="gray55")

## Bottom-right
par(mar=c(2.25,2,1,5),xaxs="i",yaxs="i",las=1)
plot(type_sum~year,data=filter(hist_perc,type=="Fish" & species=="Yellow Perch" & season=="Fall"),
     type="l",lwd=2,col=clr,ylim=ylmt,xlim=xlmt,xaxt="n",yaxt="n",xlab="",ylab="",main="",axes=F)
points(type_sum~year,data=filter(hist_perc,type=="Fish" & species=="Yellow Perch" & season=="Fall"),
       pch=16,cex=1.25)
lines(type_sum~year,data=filter(hist_perc,type=="Fish" & species=="White Perch" & season=="Fall"),
      lty=2,lwd=2,col="gray55")  
points(type_sum~year,data=filter(hist_perc,type=="Fish" & species=="White Perch" & season=="Fall"),
       pch=1,cex=1.25,col="gray55")
axis(1,year_ticks,labels=F,tcl=-0.35,col="gray55")
text(x=year_ticks+0.47,y=-30,
     labels=year_ticks,srt=0,adj=1,xpd=TRUE,cex=1.6)
axis(2,freq_ticks,labels=F,tcl=-0.35,col="gray55")
axis(1,year_ticks_2,labels=F,tcl=-0.35,col="gray55")

## -----------------------------------------------------------
## Close the device to make the actual PNG file
## -----------------------------------------------------------
dev.off()
