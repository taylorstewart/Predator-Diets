## Load Data
source("2014/Scripts/data_init.R")

diet_spring <- readWorksheet(db1,sheet="Calculated Data")
lw_spring <- readWorksheet(db1,sheet="Spring LW Data")
diet_fall <- readWorksheet(db2,sheet="Calculated Data")
lw_fall <- readWorksheet(db2,sheet="Fall LW Data")

## -----------------------------------------------------------
## Filter data and remove duplicates, leaving only unique Fish IDs.
## -----------------------------------------------------------
yp_spring_full <- as.data.frame(unique(filter(diet_spring,!is.na(food_item) & species == "yellow perch")$fid))
colnames(yp_spring_full) <- "fish_id"
wp_spring_full <- as.data.frame(unique(filter(diet_spring,!is.na(food_item) & species == "white perch")$fid))
colnames(wp_spring_full) <- "fish_id"
yp_fall_full <- as.data.frame(unique(filter(diet_fall,!is.na(food_item) & species == "yellow perch")$fid))
colnames(yp_fall_full) <- "fish_id"
wp_fall_full <- as.data.frame(unique(filter(diet_fall,!is.na(food_item) & species == "white perch")$fid))
colnames(wp_fall_full) <- "fish_id"
yp_spring_empty <- as.data.frame(unique(filter(diet_spring,is.na(food_item) & species == "yellow perch")$fid))
colnames(yp_spring_empty) <- "fish_id"
wp_spring_empty <- as.data.frame(unique(filter(diet_spring,is.na(food_item) & species == "white perch")$fid))
colnames(wp_spring_empty) <- "fish_id"
yp_fall_empty <- as.data.frame(unique(filter(diet_fall,is.na(food_item) & species == "yellow perch")$fid))
colnames(yp_fall_empty) <- "fish_id"
wp_fall_empty <- as.data.frame(unique(filter(diet_fall,is.na(food_item) & species == "white perch")$fid))
colnames(wp_fall_empty) <- "fish_id"

## -----------------------------------------------------------
## Merge effort and length-weight data, and assign season (Spring, Fall)
##  and whether or not it contained stomach contents (Y/N).
## -----------------------------------------------------------
yp_spring_full_len <- merge(yp_spring_full,lw_spring,by="fish_id")
yp_spring_full_len$contents <- "Y" 
yp_spring_full_len$season <- "spring"
wp_spring_full_len <- merge(wp_spring_full,lw_spring,by="fish_id")
wp_spring_full_len$contents <- "Y"
wp_spring_full_len$season <- "spring"
yp_fall_full_len <- merge(yp_fall_full,lw_fall,by="fish_id")
yp_fall_full_len$contents <- "Y"
yp_fall_full_len$season <- "fall"
wp_fall_full_len <- merge(wp_fall_full,lw_fall,by="fish_id")
wp_fall_full_len$contents <- "Y"
wp_fall_full_len$season <- "fall"
yp_spring_empty_len <- merge(yp_spring_empty,lw_spring,by="fish_id")
yp_spring_empty_len$contents <- "N"
yp_spring_empty_len$season <- "spring"
wp_spring_empty_len <- merge(wp_spring_empty,lw_spring,by="fish_id")
wp_spring_empty_len$contents <- "N"
wp_spring_empty_len$season <- "spring"
yp_fall_empty_len <- merge(yp_fall_empty,lw_fall,by="fish_id")
yp_fall_empty_len$contents <- "N"
yp_fall_empty_len$season <- "fall"
wp_fall_empty_len <- merge(wp_fall_empty,lw_fall,by="fish_id")
wp_fall_empty_len$contents <- "N"
wp_fall_empty_len$season <- "fall"

## -----------------------------------------------------------
## Combine into a final data frame
## -----------------------------------------------------------
lw_comb <- rbind(yp_spring_full_len,wp_spring_full_len,yp_fall_full_len,
                 wp_fall_full_len,yp_spring_empty_len,wp_spring_empty_len,
                 yp_fall_empty_len,wp_fall_empty_len)

## -----------------------------------------------------------
### A helper function for plotting
## -----------------------------------------------------------
dietHist <- function(df,spec,seas,brks,xlim,ylim,clr,show.xaxis,show.yaxis,len.ticks,freq.ticks,...) {
  par(mgp=c(0,0.4,0),tcl=-0.25,las=1,xaxs="i",yaxs="i")
  lm <- bm <- 1.5;   lrm <- btm <- 1.0
  if (show.xaxis & show.yaxis) { par(mar=c(bm,lm,btm,lrm))
  } else if (show.xaxis & !show.yaxis) { par(mar=c(bm,lm,btm,lrm))
  } else if (!show.xaxis & show.yaxis) { par(mar=c(bm,lm,btm,lrm))
  } else if (!show.xaxis & !show.yaxis) { par(mar=c(bm,lm,btm,lrm))
  }
  tmp <- filter(df,season == seas & species == spec & contents == "Y")
  tmp2 <- filter(df,season == seas & contents == "N")
  h1 <- hist(tmp2$tl,breaks=brks,plot=FALSE,right=FALSE)
  plot(h1,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="",main="",col="gray80")
  h2 <- hist(tmp$tl,breaks=brks,plot=FALSE,right=FALSE)
  plot(h2,col=clr,add=TRUE)
}

## -----------------------------------------------------------
## Set some constants for plotting
## -----------------------------------------------------------
clr <- "gray35"
brks <- seq(150,330,10)
xlmt <- range(brks)
len.ticks <- seq(150,330,10)
freq.ticks <- seq(0,15,5)
prob <- TRUE
ylmt <- range(freq.ticks)
# number of rows and cols of actual plots
nrow <- 2
ncol <- 2
# sets the base width for each plot
basew <- 5.0
baseh <- basew*0.6

## -----------------------------------------------------------
## Make a base plot
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
plot.new(); text(0.6,0.5,"Count",srt=90,cex=1.8)
plot.new(); text(0.5,0.6,"Length Group (mm)",cex=1.8)
plot.new(); legend("top",c("Contained prey items","Empty"),fill=c(clr,"gray80"),cex=1.6)


## ---------------------------------------------------
## Put on individual histograms
## ---------------------------------------------------
  ## Top-left
  dietHist(lw_comb,"yellow perch","spring",brks,xlmt,ylmt,clr,
           len.ticks,freq.ticks
  )
  axis(1,len.ticks,labels=NA,tcl=-0.3,col="gray55")
  axis(2,freq.ticks,labels=TRUE,tcl=-0.3,col="gray55",cex.axis=1.1)

  ## Bottom-left
  dietHist(lw_comb,"white perch","spring",brks,xlmt,ylmt,clr,
           len.ticks,freq.ticks
  )
  axis(1,len.ticks,labels=TRUE,tcl=-0.3,col="gray55",cex.axis=1.1)
  axis(2,freq.ticks,labels=TRUE,tcl=-0.3,col="gray55",cex.axis=1.1)

  ## Top-right
  dietHist(lw_comb,"yellow perch","fall",brks,xlmt,ylmt,clr,
           len.ticks,freq.ticks
  )
  axis(1,len.ticks,labels=NA,tcl=-0.3,col="gray55")
  axis(2,freq.ticks,labels=NA,tcl=-0.3,col="gray55")
  
  ## Bottom-right
  dietHist(lw_comb,"white perch","fall",brks,xlmt,ylmt,clr,
           len.ticks,freq.ticks
  )
  axis(1,len.ticks,labels=TRUE,tcl=-0.3,col="gray55",cex.axis=1.1)
  axis(2,freq.ticks,labels=NA,tcl=-0.3,col="gray55")

## -----------------------------------------------------------
## Clean up environment
## -----------------------------------------------------------
rm(yp_spring_full,wp_spring_full,yp_fall_full,wp_fall_full,
   yp_spring_empty,wp_spring_empty,yp_fall_empty,wp_fall_empty,
   yp_spring_full_len,wp_spring_full_len,yp_fall_full_len,
   wp_fall_full_len,yp_spring_empty_len,wp_spring_empty_len,
   yp_fall_empty_len,wp_fall_empty_len,lw_spring,lw_fall,diet_fall,
   diet_spring,lw_comb,baseh,basew,brks,clr,freq.ticks,len.ticks,
   ncol,nrow,prob,xlmt,ylmt,dietHist)
