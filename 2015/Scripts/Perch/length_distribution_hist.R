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
                    yp_fall_empty_len,wp_fall_empty_len) %>% 
  mutate(contents=factor(contents,levels=c("Y","N"),ordered=TRUE),
         species=factor(species,levels=c("Yellow Perch","White Perch"),ordered=TRUE),
         season=factor(season,levels=c("Spring","Autumn"),ordered=TRUE))

## -----------------------------------------------------------
## Make plots
## -----------------------------------------------------------
yp_spring <- ggplot(filter(lw_comb,species=="Yellow Perch",season=="Spring"),aes(tl,fill=contents)) +
  geom_histogram(binwidth=10,col=I("black")) +
  scale_x_continuous(limits=c(140,310),expand=c(0,0)) +
  scale_y_continuous(limits=c(0,25),expand=c(0,0)) +
  scale_fill_grey(start=0.7,end=0.3,
                  labels=c("Diet Contents Observed", "Empty")) +
  labs(title='Spring',x='',y='\nYellow Perch\n') +
  theme(axis.text.x=element_blank(),axis.text.y=element_text(size=19),
        axis.line=element_line(),axis.title=element_text(size=20),axis.ticks.length=unit(1.75,'mm'),
        legend.position="bottom",legend.title=element_blank(),legend.text=element_text(size=18),
        panel.background=element_blank(),strip.text=element_blank(),strip.background=element_blank(),
        plot.title=element_text(size=20),plot.margin=unit(c(5,0,3,0),"mm"))

wp_spring <- ggplot(filter(lw_comb,species=="White Perch",season=="Spring"),aes(tl,fill=contents)) +
  geom_histogram(binwidth=10,col=I("black")) +
  scale_x_continuous(limits=c(140,310),expand=c(0,0)) +
  scale_y_continuous(limits=c(0,25),expand=c(0,0)) +
  scale_fill_grey(start=0.7,end=0.3,
                  labels=c("Diet Contents Observed", "Empty")) +
  labs(title='',x='',y='\nWhite Perch\n') +
  theme(axis.text.x=element_text(size=19),axis.text.y=element_text(size=19),
        axis.line=element_line(),axis.title=element_text(size=20),axis.ticks.length=unit(1.75,'mm'),
        legend.position="bottom",legend.title=element_blank(),legend.text=element_text(size=18),
        panel.background=element_blank(),strip.text=element_blank(),strip.background=element_blank(),
        plot.title=element_text(size=20),plot.margin=unit(c(0,0,3,0),"mm"))

yp_fall <- ggplot(filter(lw_comb,species=="Yellow Perch",season=="Autumn"),aes(tl,fill=contents)) +
  geom_histogram(binwidth=10,col=I("black")) +
  scale_x_continuous(limits=c(140,310),expand=c(0,0)) +
  scale_y_continuous(limits=c(0,25),expand=c(0,0)) +
  scale_fill_grey(start=0.7,end=0.3,
                  labels=c("Diet Contents Observed", "Empty")) +
  labs(title='Autumn',x='',y='') +
  theme(axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.line=element_line(),axis.title=element_text(size=20),axis.ticks.length=unit(1.75,'mm'),
        legend.position="bottom",legend.title=element_blank(),legend.text=element_text(size=18),
        panel.background=element_blank(),strip.text=element_blank(),strip.background=element_blank(),
        plot.title=element_text(size=20),plot.margin=unit(c(5,15,3,0),"mm"))

wp_fall <- ggplot(filter(lw_comb,species=="White Perch",season=="Autumn"),aes(tl,fill=contents)) +
  geom_histogram(binwidth=10,col=I("black")) +
  scale_x_continuous(limits=c(140,310),expand=c(0,0)) +
  scale_y_continuous(limits=c(0,25),expand=c(0,0)) +
  scale_fill_grey(start=0.7,end=0.3,
                  labels=c("Diet Contents Observed", "Empty")) +
  labs(title='',x='',y='') +
  theme(axis.text.x=element_text(size=19),axis.text.y=element_blank(),
        axis.line=element_line(),axis.title=element_text(size=20),axis.ticks.length=unit(1.75,'mm'),
        legend.position="bottom",legend.title=element_blank(),legend.text=element_text(size=18),
        panel.background=element_blank(),strip.text=element_blank(),strip.background=element_blank(),
        plot.title=element_text(size=20),plot.margin=unit(c(0,15,3,0),"mm"))

## -----------------------------------------------------------
## Create common legend
## -----------------------------------------------------------
legend = gtable_filter(ggplot_gtable(ggplot_build(yp_spring)), "guide-box") 

## -----------------------------------------------------------
## Save the plot as a figure (comment out line 144 and 182 until you are ready to save)
## -----------------------------------------------------------
png("2015/Figures/length_distribution.PNG",width=11,height=9,units="in",family="Times",res=300)

## -----------------------------------------------------------
## Put plots into a matrix
## -----------------------------------------------------------
grid.arrange(arrangeGrob(yp_spring + theme(legend.position="none"),
                         yp_fall + theme(legend.position="none"),
                         wp_spring + theme(legend.position="none"),
                         wp_fall + theme(legend.position="none"),
                         ncol=2,
                         nrow=2,
                         left=textGrob("Frequency",y=unit(110,'mm'),rot=90,gp=gpar(fontsize=30)),
                         bottom=textGrob("Length Group (mm)",gp=gpar(fontsize=30))),
             legend,
             heights=c(8,1))

## -----------------------------------------------------------
## Close the device to make the actual PNG file
## -----------------------------------------------------------
dev.off()
