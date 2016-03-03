## Load Data
source("2015/Scripts/Perch/data_init.R")

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
## Merge effort and length-weight data, and assign season (Spring, Fall)
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
diet_comb <- bind_rows(yp_spring_full_len,wp_spring_full_len,yp_fall_full_len,
                       wp_fall_full_len,yp_spring_empty_len,wp_spring_empty_len,
                       yp_fall_empty_len,wp_fall_empty_len) %>% 
  left_join(effort)

## -----------------------------------------------------------
### Calculate percentange of stomach with contents
## -----------------------------------------------------------
## Create a unique list of serials
serial <- unique(diet_comb$serial)
## Run loop to calculate percentage
perc <- data.frame(do.call(rbind,lapply(serial,function(i) {
  round((sum(nrow(filter(diet_comb,serial==i & contents=="Y")))/sum(nrow(filter(diet_comb,serial==i))))*100,1)
})))
## Assign serials to data frame from the serial list
perc$serial <- serial
## rename column headers
colnames(perc) <- c("Percent","serial")

## Join individual fish data with percentage
final <- left_join(perc,diet_comb)

## Create a vector for each species where each row is season and column is prey taxa
final_yp <- filter(final,species=="Yellow Perch") %>% 
  distinct(serial) %>% 
  mutate(species = factor(species,levels=c("Yellow Perch","White Perch")),
         season = factor(season,levels=c("Spring","Autumn")))
final_wp <- filter(final,species=="White Perch") %>% 
  distinct(serial) %>% 
  mutate(species = factor(species,levels=c("Yellow Perch","White Perch")),
         season = factor(season,levels=c("Spring","Autumn")))

## Combine ech vector into one data frame
final_comb <- bind_rows(final_yp,final_wp)

## -----------------------------------------------------------
### Create each individual plot
## -----------------------------------------------------------
yp_spring <- ggplot(data=filter(wb_shore,piece=="1" & group=="3.1"),aes(long,lat)) +
  geom_polygon(fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="2"),aes(long,lat),fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="3"),aes(long,lat),fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="4"),aes(long,lat),fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="5"),aes(long,lat),fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="6"),aes(long,lat),fill="white",colour="black") +
  geom_point(data=filter(final_comb,species=="Yellow Perch",season=="Spring"),aes(long_st,lat_st,size=Percent*0.9),
             fill="gray70",colour="black",pch=21,stroke=0.8) +
  scale_size_continuous(name="Percent",limits=c(0,100),breaks=c(0,20,40,60,80,100)) +
  xlim(-83.514,-82.12) +
  coord_fixed(ratio = 1.3) +
  labs(title='Spring',x='',y='Yellow Perch') +
  theme(axis.ticks=element_blank(),axis.title=element_text(size=12),axis.text=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),strip.background=element_blank(),
        strip.text=element_text(size=10),legend.background=element_rect(colour="black",fill="white"),
        plot.title=element_text(size=11))

wp_spring <- ggplot(data=filter(wb_shore,piece=="1" & group=="3.1"),aes(long,lat)) +
  geom_polygon(fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="2"),aes(long,lat),fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="3"),aes(long,lat),fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="4"),aes(long,lat),fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="5"),aes(long,lat),fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="6"),aes(long,lat),fill="white",colour="black") +
  geom_point(data=filter(final_comb,species=="White Perch",season=="Spring"),aes(long_st,lat_st,size=Percent*0.9),
             fill="gray70",colour="black",pch=21,stroke=0.8) +
  scale_size_continuous(name="Percent",limits=c(0,100),breaks=c(0,20,40,60,80,100)) +
  xlim(-83.514,-82.12) +
  coord_fixed(ratio = 1.3) +
  labs(title='',x='',y='White Perch') +
  theme(axis.ticks=element_blank(),axis.title=element_text(size=12),axis.text=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),strip.background=element_blank(),
        strip.text=element_text(size=10),legend.background=element_rect(colour="black",fill="white"),
        plot.title=element_text(size=11))

yp_fall <- ggplot(data=filter(wb_shore,piece=="1" & group=="3.1"),aes(long,lat)) +
  geom_polygon(fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="2"),aes(long,lat),fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="3"),aes(long,lat),fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="4"),aes(long,lat),fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="5"),aes(long,lat),fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="6"),aes(long,lat),fill="white",colour="black") +
  geom_point(data=filter(final_comb,species=="Yellow Perch",season=="Autumn"),aes(long_st,lat_st,size=Percent*0.9),
             fill="gray70",colour="black",pch=21,stroke=0.8) +
  scale_size_continuous(name="Percent",limits=c(0,100),breaks=c(0,20,40,60,80,100)) +
  xlim(-83.514,-82.12) +
  coord_fixed(ratio = 1.3) +
  labs(title='Autumn',x='',y='') +
  theme(axis.ticks=element_blank(),axis.title=element_text(size=12),axis.text=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),legend.key=element_rect(fill="white"),
        legend.title=element_text(size=10),legend.background=element_rect(colour="black",fill="white"),
        plot.title=element_text(size=11))

wp_fall <- ggplot(data=filter(wb_shore,piece=="1" & group=="3.1"),aes(long,lat)) +
  geom_polygon(fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="2"),aes(long,lat),fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="3"),aes(long,lat),fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="4"),aes(long,lat),fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="5"),aes(long,lat),fill="white",colour="black") +
  geom_polygon(data=filter(wb_shore,piece=="6"),aes(long,lat),fill="white",colour="black") +
  geom_point(data=filter(final_comb,species=="White Perch",season=="Autumn"),aes(long_st,lat_st,size=Percent*0.9),
             fill="gray70",colour="black",pch=21,stroke=0.8) +
  scale_size_continuous(name="Percent",limits=c(0,100),breaks=c(0,20,40,60,80,100)) +
  xlim(-83.514,-82.12) +
  coord_fixed(ratio = 1.3) +
  labs(title='',x='',y='') +
  theme(axis.ticks=element_blank(),axis.title=element_text(size=12),axis.text=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),legend.key=element_rect(fill="white"),
        legend.title=element_text(size=10),legend.background=element_rect(colour="black",fill="white"),
        plot.title=element_text(size=11))

## -----------------------------------------------------------
## Create common legend
## -----------------------------------------------------------
legend = gtable_filter(ggplot_gtable(ggplot_build(yp_spring)), "guide-box") 

## -----------------------------------------------------------
## Save the plot as a figure (comment out line 88 and 118 until you are ready to save)
## -----------------------------------------------------------
png("2015/Figures/percent_content_map.PNG",width=8,height=5.5,units="in",family="Times",res=300)

## -----------------------------------------------------------
## Put plots into a matrix
## -----------------------------------------------------------
grid.arrange(arrangeGrob(yp_spring + theme(legend.position="none"),
                         yp_fall + theme(legend.position="none"),
                         wp_spring + theme(legend.position="none"),
                         wp_fall + theme(legend.position="none"),
                         ncol=2,
                         nrow=2),
             legend,
             widths=c(6,1))

## -----------------------------------------------------------
## Close the device to make the actual PNG file
## -----------------------------------------------------------
dev.off()
