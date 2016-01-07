## Load Data
source("2015/Scripts/data_init.R")

## -----------------------------------------------------------
## Filter data and remove duplicates, leaving only unique Fish IDs.
## -----------------------------------------------------------
yp_spring_full <- as.data.frame(unique(filter(diet_spring,food_item!="Empty" & species=="Yellow Perch")$fid))
colnames(yp_spring_full) <- "fish_id"
wp_spring_full <- as.data.frame(unique(filter(diet_spring,food_item!="Empty" & species=="White Perch")$fid))
colnames(wp_spring_full) <- "fish_id"
yp_fall_full <- as.data.frame(unique(filter(diet_fall,food_item!="Empty" & species=="Yellow Perch")$fid))
colnames(yp_fall_full) <- "fish_id"
wp_fall_full <- as.data.frame(unique(filter(diet_fall,food_item!="Empty" & species=="White Perch")$fid))
colnames(wp_fall_full) <- "fish_id"
yp_spring_empty <- as.data.frame(unique(filter(diet_spring,food_item=="Empty" & species=="Yellow Perch")$fid))
colnames(yp_spring_empty) <- "fish_id"
wp_spring_empty <- as.data.frame(unique(filter(diet_spring,food_item == "Empty" & species=="White Perch")$fid))
colnames(wp_spring_empty) <- "fish_id"
yp_fall_empty <- as.data.frame(unique(filter(diet_fall,food_item == "Empty" & species=="Yellow Perch")$fid))
colnames(yp_fall_empty) <- "fish_id"
wp_fall_empty <- as.data.frame(unique(filter(diet_fall,food_item=="Empty" & species=="White Perch")$fid))
colnames(wp_fall_empty) <- "fish_id"

## -----------------------------------------------------------
## Merge length-weight data, and assign season (Spring, Fall)
##  and whether or not it contained stomach contents (Y/N).
## -----------------------------------------------------------
yp_spring_full_len <- left_join(yp_spring_full,spring_lw) %>% 
  mutate(contents="Y",
         season="Spring")
wp_spring_full_len <- left_join(wp_spring_full,spring_lw) %>% 
  mutate(contents="Y",
         season="Spring")
yp_fall_full_len <- left_join(yp_fall_full,fall_lw) %>% 
  mutate(contents="Y",
         season="Autumn")
wp_fall_full_len <- left_join(wp_fall_full,fall_lw) %>% 
  mutate(contents="Y",
         season="Autumn")
yp_spring_empty_len <- left_join(yp_spring_empty,spring_lw) %>% 
  mutate(contents="N",
         season="Spring")
wp_spring_empty_len <- left_join(wp_spring_empty,spring_lw) %>% 
  mutate(contents="N",
         season="Spring")
yp_fall_empty_len <- left_join(yp_fall_empty,fall_lw) %>% 
  mutate(contents="N",
         season="Autumn")
wp_fall_empty_len <- left_join(wp_fall_empty,fall_lw) %>% 
  mutate(contents="N",
         season="Autumn")

## -----------------------------------------------------------
## Combine into a final data frame
## -----------------------------------------------------------
lw_comb <- bind_rows(yp_spring_full_len,wp_spring_full_len,yp_fall_full_len,
                    wp_fall_full_len,yp_spring_empty_len,wp_spring_empty_len,
                    yp_fall_empty_len,wp_fall_empty_len)

## -----------------------------------------------------------
## Set some constants for plotting
## -----------------------------------------------------------
clr <- "gray35"
brks <- seq(140,310,10)
xlmt <- range(brks)
len_ticks <- seq(150,300,50)
freq_ticks <- seq(0,25,5)
prob <- TRUE
ylmt <- range(freq_ticks)
# number of rows and cols of actual plots
nrow <- 2
ncol <- 2
# sets the base width for each plot
basew <- 5.0
baseh <- basew*0.8

## -----------------------------------------------------------
## Save the plot as a figure (comment out line 144 and 182 until you are ready to save)
## -----------------------------------------------------------
png("2015/Figures/length_distribution.PNG",width=11,height=9,units="in",family="Times",res=300)

## -----------------------------------------------------------
## Make a base plots
## -----------------------------------------------------------
# make the layout
layout(rbind(cbind(rep(1,nrow),
                    matrix(4:7,nrow=nrow,byrow=FALSE)),
              c(0,rep(2,ncol)),
              c(0,rep(3,ncol))),
        widths=c(1,basew,rep(basew,ncol-1),1),
        heights=c(rep(baseh,nrow-1),baseh,1),
        respect=TRUE)
# put on some axis labels
par(mar=c(0,0,0,0))
plot.new(); text(0.2,0.5,"Frequency",srt=90,cex=3)
plot.new(); text(0.5,0.45,"Length Group (mm)",cex=2.75)
plot.new(); legend("top",c("Diet Contents Observed","Empty"),fill=c("gray80",clr),cex=2)

## ---------------------------------------------------
## Put on individual histograms
## ---------------------------------------------------
  ## Top-left
  par(mar=c(2,2.5,2.5,2.75),las=1,xaxs="i",yaxs="i")
  plot(hist(filter(lw_comb,season=="Spring" & species=="Yellow Perch")$tl,breaks=brks,plot=FALSE,right=FALSE)
       ,xlim=xlmt,ylim=ylmt,xaxt="n",yaxt="n",xlab="",ylab="",main="",col=clr)
  plot(hist(filter(lw_comb,season=="Spring" & species=="Yellow Perch" & contents=="Y")$tl,breaks=brks,plot=FALSE,right=FALSE),
       col="gray80",add=TRUE)
  axis(1,len_ticks,labels=NA,tcl=-0.4,col="gray55")
  axis(2,freq_ticks,labels=TRUE,tcl=-0.4,col="gray55",cex.axis=2.25)
  axis(1,brks,labels=NA,tcl=-0.4,col="gray55")

  ## Bottom-left
  par(mar=c(2.5,2.5,2,2.75),las=1,xaxs="i",yaxs="i")
  plot(hist(filter(lw_comb,season=="Spring" & species=="White Perch")$tl,breaks=brks,plot=FALSE,right=FALSE)
       ,xlim=xlmt,ylim=ylmt,xaxt="n",yaxt="n",xlab="",ylab="",main="",col=clr)
  plot(hist(filter(lw_comb,season=="Spring" & species=="White Perch" & contents=="Y")$tl,breaks=brks,plot=FALSE,right=FALSE),
       col="gray80",add=TRUE)
  axis(1,len_ticks,labels=NA,tcl=0,col="gray55")
  text(x=len_ticks+9.5,y=-2,
       labels=len_ticks,srt=0,adj=1,xpd=TRUE,cex=2.25)
  axis(2,freq_ticks,labels=TRUE,tcl=-0.4,col="gray55",cex.axis=2.25)
  axis(1,brks,labels=NA,tcl=-0.4,col="gray55")

  ## Top-right
  par(mar=c(2,2.25,2.5,2.5),las=1,xaxs="i",yaxs="i")
  plot(hist(filter(lw_comb,season=="Autumn" & species=="Yellow Perch")$tl,breaks=brks,plot=FALSE,right=FALSE)
       ,xlim=xlmt,ylim=ylmt,xaxt="n",yaxt="n",xlab="",ylab="",main="",col=clr)
  plot(hist(filter(lw_comb,season=="Autumn" & species=="Yellow Perch" & contents=="Y")$tl,breaks=brks,plot=FALSE,right=FALSE),
       col="gray80",add=TRUE)
  axis(1,len_ticks,labels=NA,tcl=-0.4,col="gray55")
  axis(2,freq_ticks,labels=NA,tcl=-0.4,col="gray55")
  axis(1,brks,labels=NA,tcl=-0.4,col="gray55")

  ## Bottom-right
  par(mar=c(2.5,2.25,2,2.5),las=1,xaxs="i",yaxs="i")
  plot(hist(filter(lw_comb,season=="Autumn" & species=="White Perch")$tl,breaks=brks,plot=FALSE,right=FALSE)
       ,xlim=xlmt,ylim=ylmt,xaxt="n",yaxt="n",xlab="",ylab="",main="",col=clr)
  plot(hist(filter(lw_comb,season=="Autumn" & species=="White Perch" & contents=="Y")$tl,breaks=brks,plot=FALSE,right=FALSE),
       col="gray80",add=TRUE)
  axis(1,len_ticks,labels=NA,tcl=-0.4,col="gray55")
  text(x=len_ticks+9.5,y=-2,
       labels=len_ticks,srt=0,adj=1,xpd=TRUE,cex=2.25)
  axis(2,freq_ticks,labels=NA,tcl=-0.4,col="gray55")
  axis(1,brks,labels=NA,tcl=-0.4,col="gray55")

## -----------------------------------------------------------
## Close the device to make the actual PNG file
## -----------------------------------------------------------
dev.off()
