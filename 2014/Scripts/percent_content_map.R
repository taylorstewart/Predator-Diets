## Load Data
source("2014/Scripts/data_init.R")

diet_spring <- readWorksheet(db1,sheet="Calculated Data")
effort_spring <- readWorksheet(db1,sheet="Effort Data")
effort_spring %<>% select(serial,lat_st,long_st)
lw_spring <- readWorksheet(db1,sheet="Spring LW Data")
diet_fall <- readWorksheet(db2,sheet="Calculated Data")
effort_fall <- readWorksheet(db2,sheet="Effort Data")
effort_fall %<>% select(serial,lat_st,long_st)
lw_fall <- readWorksheet(db2,sheet="Fall LW Data")
wb_shore <- readWorksheet(db4,sheet="LatLong")

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

yp_spring_full_len <- merge(yp_spring_full,lw_spring,by="fish_id")
yp_spring_full_len <- merge(effort_spring,yp_spring_full_len,by.x="serial",by.y="serial.")
yp_spring_full_len$contents <- "Y" 
yp_spring_full_len$season <- "spring"
wp_spring_full_len <- merge(wp_spring_full,lw_spring,by="fish_id")
wp_spring_full_len <- merge(effort_spring,wp_spring_full_len,by.x="serial",by.y="serial.")
wp_spring_full_len$contents <- "Y"
wp_spring_full_len$season <- "spring"
yp_fall_full_len <- merge(yp_fall_full,lw_fall,by="fish_id")
yp_fall_full_len <- merge(effort_fall,yp_fall_full_len,by.x="serial",by.y="serial.")
yp_fall_full_len$contents <- "Y"
yp_fall_full_len$season <- "fall"
wp_fall_full_len <- merge(wp_fall_full,lw_fall,by="fish_id")
wp_fall_full_len <- merge(effort_fall,wp_fall_full_len,by.x="serial",by.y="serial.")
wp_fall_full_len$contents <- "Y"
wp_fall_full_len$season <- "fall"
yp_spring_empty_len <- merge(yp_spring_empty,lw_spring,by="fish_id")
yp_spring_empty_len <- merge(effort_spring,yp_spring_empty_len,by.x="serial",by.y="serial.")
yp_spring_empty_len$contents <- "N"
yp_spring_empty_len$season <- "spring"
wp_spring_empty_len <- merge(wp_spring_empty,lw_spring,by="fish_id")
wp_spring_empty_len <- merge(effort_spring,wp_spring_empty_len,by.x="serial",by.y="serial.")
wp_spring_empty_len$contents <- "N"
wp_spring_empty_len$season <- "spring"
yp_fall_empty_len <- merge(yp_fall_empty,lw_fall,by="fish_id")
yp_fall_empty_len <- merge(effort_fall,yp_fall_empty_len,by.x="serial",by.y="serial.")
yp_fall_empty_len$contents <- "N"
yp_fall_empty_len$season <- "fall"
wp_fall_empty_len <- merge(wp_fall_empty,lw_fall,by="fish_id")
wp_fall_empty_len <- merge(effort_fall,wp_fall_empty_len,by.x="serial",by.y="serial.")
wp_fall_empty_len$contents <- "N"
wp_fall_empty_len$season <- "fall"

diet_comb <- rbind(yp_spring_full_len,wp_spring_full_len,yp_fall_full_len,
                   wp_fall_full_len,yp_spring_empty_len,wp_spring_empty_len,
                   yp_fall_empty_len,wp_fall_empty_len)

## -----------------------------------------------------------
### 
## -----------------------------------------------------------
serial <- unique(diet_comb$serial)
test <- lapply(serial,function(i) {
  round((sum(nrow(filter(diet_comb,serial==i & contents=="Y")))/sum(nrow(filter(diet_comb,serial==i))))*100,1)
})
test2 <- data.frame(do.call(rbind,test))
test2$serial <- serial
colnames(test2) <- c("Percent","serial")

final <- merge(test2,diet_comb,by="serial")
final_yp <- filter(final,species=="yellow perch")
final_wp <- filter(final,species=="white perch")

final_yp2 <- final_yp[!duplicated(final_yp$serial),]
final_wp2 <- final_wp[!duplicated(final_wp$serial),]

final_comb <- rbind(final_yp2,final_wp2)
final_comb$species <- factor(final_comb$species,levels=c("yellow perch","white perch"))
final_comb$season <- factor(final_comb$season,levels=c("spring","fall"))

## -----------------------------------------------------------
### 
## -----------------------------------------------------------
shore <- ggplot(data=filter(wb_shore,piece=="1" & group=="3.1"),aes(long,lat)) +
  geom_polygon(fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="2"),aes(long,lat),fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="3"),aes(long,lat),fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="4"),aes(long,lat),fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="5"),aes(long,lat),fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="6"),aes(long,lat),fill="white",colour="black") +
  geom_point(data=final_comb,aes(long_st,lat_st,size=Percent)) +
  scale_size_continuous(limits=c(0,100),breaks=c(0,20,40,60,80,100)) +
  xlim(-83.514,-82.12) +
  coord_fixed(ratio = 1.1) +
  facet_wrap(~species+season) +
  theme(axis.ticks=element_blank(),axis.title=element_blank(),axis.text=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),strip.background = element_blank(),
        strip.text.x = element_blank(),legend.key=element_rect(fill="white"),
        legend.title=element_text(size=15),legend.background=element_rect(colour="black",fill="white"))

plot(shore)

## -----------------------------------------------------------
### clean up environment
## -----------------------------------------------------------
rm(yp_spring_full,wp_spring_full,yp_fall_full,wp_fall_full,yp_spring_empty,
   wp_spring_empty,yp_fall_empty,wp_fall_empty,yp_spring_full_len,
   wp_spring_full_len,yp_fall_full_len,wp_fall_full_len,
   yp_spring_empty_len,wp_spring_empty_len,yp_fall_empty_len,
   wp_fall_empty_len,lw_spring,lw_fall,effort_spring,effort_fall,
   diet_fall,diet_spring,diet_comb,final,final_comb,final_wp,final_wp2,
   final_yp,final_yp2,test2,wb_shore,serial,test)
