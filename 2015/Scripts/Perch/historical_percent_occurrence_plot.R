<<<<<<< HEAD
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
taxa_list <- c("Zooplankton","Benthic Invertebrates","Fish Prey")

## -----------------------------------------------------------
### SPRING
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the number of fish found with each prey taxa
yp_spring_n <- as.data.frame(do.call(rbind,lapply(taxa_list,function(i) {
  yp_spring %<>% filter(type == i) %>% distinct(fid)
  nrow(yp_spring)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
yp_spring_perc <- as.data.frame(do.call(rbind,lapply(1:nrow(yp_spring_n),function(j) {
  round(as.numeric(((yp_spring_n[j,1])/length(unique(yp_spring$fid)))*100),1)
})))

## Add prey names, fish species, and season to data frame
yp_spring_perc %<>% transmute(type = taxa_list,
                              type_sum = V1,
                              species = "Yellow Perch",
                              year = 2015,
                              season = "Spring")

##### WHITE PERCH #####
## Calculate the number of fish found with each prey taxa
wp_spring_n <- as.data.frame(do.call(rbind,lapply(taxa_list,function(i) {
  wp_spring %<>% filter(type == i) %>% distinct(fid)
  nrow(wp_spring)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
wp_spring_perc <- as.data.frame(do.call(rbind,lapply(1:nrow(wp_spring_n),function(j) {
  round(as.numeric(((wp_spring_n[j,1])/length(unique(wp_spring$fid)))*100),1)
})))

## Add prey names, fish species, and season to data frame
wp_spring_perc %<>% transmute(type = taxa_list,
                              type_sum = V1,
                              species = "White Perch",
                              year = 2015,
                              season = "Spring")

## -----------------------------------------------------------
### FALL
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the number of fish found with each prey taxa
yp_fall_n <- as.data.frame(do.call(rbind,lapply(taxa_list,function(i) {
  yp_fall %<>% filter(type == i) %>% distinct(fid)
  nrow(yp_fall)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
yp_fall_perc <- as.data.frame(do.call(rbind,lapply(1:nrow(yp_fall_n),function(j) {
  round(as.numeric(((yp_fall_n[j,1])/length(unique(yp_fall$fid)))*100),1)
})))

## Add prey names, fish species, and season to data frame
yp_fall_perc %<>% transmute(type = taxa_list,
                            type_sum = V1,
                            species = "Yellow Perch",
                            year = 2015,
                            season = "Autumn")

##### WHITE PERCH #####
## Calculate the number of fish found with each prey taxa
wp_fall_n <- as.data.frame(do.call(rbind,lapply(taxa_list,function(i) {
  wp_fall %<>% filter(type == i) %>% distinct(fid)
  nrow(wp_fall)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
wp_fall_perc <- as.data.frame(do.call(rbind,lapply(1:nrow(wp_fall_n),function(j) {
  round(as.numeric(((wp_fall_n[j,1])/length(unique(wp_fall$fid)))*100),1)
})))

## Add prey names, fish species, and season to data frame
wp_fall_perc %<>% transmute(type = taxa_list,
                            type_sum = V1,
                            species = "White Perch",
                            year = 2015,
                            season = "Autumn")

## -----------------------------------------------------------
## Combine into a final data frame
## -----------------------------------------------------------
perc_comb <- bind_rows(yp_spring_perc,wp_spring_perc,yp_fall_perc,wp_fall_perc)

## -----------------------------------------------------------
## Combine historical data with current data.
## -----------------------------------------------------------
hist_perc <- bind_rows(hist_perc,perc_comb) %>% 
  mutate(species=factor(species,levels=c("Yellow Perch","White Perch"),ordered=TRUE),
         season=factor(season,levels=c("Spring","Autumn"),ordered=TRUE),
         type=factor(type,levels=c("Zooplankton","Benthic Invertebrates","Fish Prey"),ordered=TRUE))

## ---------------------------------------------------
## Make individual plots
## ---------------------------------------------------
zoop_spring <- ggplot(filter(hist_perc,type=="Zooplankton",season=="Spring"),aes(year,type_sum)) +
  geom_point(aes(colour=species),size=1.5) +
  geom_line(aes(linetype=species,colour=species),size=0.85) +
  scale_color_grey(start=0.1,end=0.6) +
  scale_y_continuous(limits = c(-2,102),expand=c(0,0)) +
  scale_x_continuous(limits=c(2004.9,2015.1),breaks=seq(2005,2015,2),expand=c(0,0)) +
  labs(title='Spring',x='',y='Zooplankton') +
  theme(axis.text.y=element_text(size=15),axis.text.x=element_blank(),axis.ticks.length=unit(1.75,'mm'),
        axis.line=element_line(),legend.title=element_blank(),legend.position="top",legend.text=element_text(size=18),
        panel.background=element_blank(),strip.text=element_blank(),axis.title=element_text(size=13.5),
        plot.margin=unit(c(1,6,0,6),"mm"),panel.margin=unit(1.5,"lines"))

zoop_fall <- ggplot(filter(hist_perc,type=="Zooplankton",season=="Autumn"),aes(year,type_sum)) +
  geom_point(aes(colour=species),size=1.5) +
  geom_line(aes(linetype=species,colour=species),size=0.85) +
  scale_color_grey(start=0.1,end=0.6) +
  scale_y_continuous(limits = c(-2,102),expand=c(0,0)) +
  scale_x_continuous(limits=c(2004.9,2015.1),breaks=seq(2005,2015,2),expand=c(0,0)) +
  labs(title='Autumn',x='',y='') +
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.ticks.length=unit(1.75,'mm'),
        axis.line=element_line(),axis.title=element_text(size=13.5),
        panel.background=element_blank(),strip.text=element_blank(),
        plot.margin=unit(c(1,14,0,3),"mm"),panel.margin=unit(1.5,"lines"))

benthos_spring <- ggplot(filter(hist_perc,type=="Benthic Invertebrates",season=="Spring"),aes(year,type_sum)) +
  geom_point(aes(colour=species),size=1.5) +
  geom_line(aes(linetype=species,colour=species),size=0.85) +
  scale_color_grey(start=0.1,end=0.6) +
  scale_y_continuous(limits = c(-2,102),expand=c(0,0)) +
  scale_x_continuous(limits=c(2004.9,2015.1),breaks=seq(2005,2015,2),expand=c(0,0)) +
  labs(title='',x='',y='Benthic Macroinvertebrates') +
  theme(axis.text.y=element_text(size=15),axis.text.x=element_blank(),axis.ticks.length=unit(1.75,'mm'),
        axis.line=element_line(),axis.title=element_text(size=13.5),
        panel.background=element_blank(),strip.text=element_blank(),
        plot.margin=unit(c(1,6,0,6),"mm"),panel.margin=unit(1.5,"lines"))

benthos_fall <- ggplot(filter(hist_perc,type=="Benthic Invertebrates",season=="Autumn"),aes(year,type_sum)) +
  geom_point(aes(colour=species),size=1.5) +
  geom_line(aes(linetype=species,colour=species),size=0.85) +
  scale_color_grey(start=0.1,end=0.6) +
  scale_y_continuous(limits = c(-2,102),expand=c(0,0)) +
  scale_x_continuous(limits=c(2004.9,2015.1),breaks=seq(2005,2015,2),expand=c(0,0)) +
  labs(title='',x='',y='') +
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.ticks.length=unit(1.75,'mm'),
        axis.line=element_line(),axis.title=element_text(size=13.5),
        panel.background=element_blank(),strip.text=element_blank(),
        plot.margin=unit(c(1,14,0,3),"mm"),panel.margin=unit(1.5,"lines"))

fish_spring <- ggplot(filter(hist_perc,type=="Fish Prey",season=="Spring"),aes(year,type_sum)) +
  geom_point(aes(colour=species),size=1.5) +
  geom_line(aes(linetype=species,colour=species),size=0.85) +
  scale_color_grey(start=0.1,end=0.6) +
  scale_y_continuous(limits = c(-2,102),expand=c(0,0)) +
  scale_x_continuous(limits=c(2004.9,2015.1),breaks=seq(2005,2015,2),expand=c(0,0)) +
  labs(title='',x='',y='Fish Prey') +
  theme(axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),axis.ticks.length=unit(1.75,'mm'),
        axis.line=element_line(),axis.title=element_text(size=13.5),
        panel.background=element_blank(),strip.text=element_blank(),
        plot.margin=unit(c(1,6,0,6),"mm"),panel.margin=unit(1.5,"lines"))

fish_fall <- ggplot(filter(hist_perc,type=="Fish Prey",season=="Autumn"),aes(year,type_sum)) +
  geom_point(aes(colour=species),size=1.5) +
  geom_line(aes(linetype=species,colour=species),size=0.85) +
  scale_color_grey(start=0.1,end=0.6) +
  scale_y_continuous(limits = c(-2,102),expand=c(0,0)) +
  scale_x_continuous(limits=c(2004.9,2015.1),breaks=seq(2005,2015,2),expand=c(0,0)) +
  labs(title='',x='',y='') +
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=15),axis.ticks.length=unit(1.75,'mm'),
        axis.line=element_line(),axis.title=element_text(size=13.5),
        panel.background=element_blank(),strip.text=element_blank(),
        plot.margin=unit(c(1,14,0,3),"mm"),panel.margin=unit(1.5,"lines"))

## -----------------------------------------------------------
## Create common legend
## -----------------------------------------------------------
legend = gtable_filter(ggplot_gtable(ggplot_build(zoop_spring)), "guide-box") 

## -----------------------------------------------------------
## Save the plot as a figure (comment out line 144 and 182 until you are ready to save)
## -----------------------------------------------------------
png("2015/Figures/historical_percent_occurence.PNG",width=11,height=8,units="in",family="Times",res=300)

## -----------------------------------------------------------
## Put plots into a matrix
## -----------------------------------------------------------
grid.arrange(arrangeGrob(zoop_spring + theme(legend.position="none"),
                                zoop_fall + theme(legend.position="none"),
                                benthos_spring + theme(legend.position="none"),
                                benthos_fall + theme(legend.position="none"),
                                fish_spring + theme(legend.position="none"),
                                fish_fall + theme(legend.position="none"),
                                ncol=2,
                                nrow=3,
                                left=textGrob("% Frequency of Occurrence",rot=90,vjust=0.5,gp=gpar(fontsize=24)),
                                bottom=textGrob("Year",gp=gpar(fontsize=20))),
             legend,
             heights=c(10,1))

## -----------------------------------------------------------
## Close the device to make the actual PNG file
## -----------------------------------------------------------
dev.off()
=======
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
taxa_list <- c("Zooplankton","Benthic Invertebrates","Fish Prey")

## -----------------------------------------------------------
### SPRING
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the number of fish found with each prey taxa
yp_spring_n <- as.data.frame(do.call(rbind,lapply(taxa_list,function(i) {
  yp_spring %<>% filter(type == i) %>% distinct(fid)
  nrow(yp_spring)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
yp_spring_perc <- as.data.frame(do.call(rbind,lapply(1:nrow(yp_spring_n),function(j) {
  round(as.numeric(((yp_spring_n[j,1])/length(unique(yp_spring$fid)))*100),1)
})))

## Add prey names, fish species, and season to data frame
yp_spring_perc %<>% transmute(type = taxa_list,
                              type_sum = V1,
                              species = "Yellow Perch",
                              year = 2015,
                              season = "Spring")

##### WHITE PERCH #####
## Calculate the number of fish found with each prey taxa
wp_spring_n <- as.data.frame(do.call(rbind,lapply(taxa_list,function(i) {
  wp_spring %<>% filter(type == i) %>% distinct(fid)
  nrow(wp_spring)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
wp_spring_perc <- as.data.frame(do.call(rbind,lapply(1:nrow(wp_spring_n),function(j) {
  round(as.numeric(((wp_spring_n[j,1])/length(unique(wp_spring$fid)))*100),1)
})))

## Add prey names, fish species, and season to data frame
wp_spring_perc %<>% transmute(type = taxa_list,
                              type_sum = V1,
                              species = "White Perch",
                              year = 2015,
                              season = "Spring")

## -----------------------------------------------------------
### FALL
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the number of fish found with each prey taxa
yp_fall_n <- as.data.frame(do.call(rbind,lapply(taxa_list,function(i) {
  yp_fall %<>% filter(type == i) %>% distinct(fid)
  nrow(yp_fall)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
yp_fall_perc <- as.data.frame(do.call(rbind,lapply(1:nrow(yp_fall_n),function(j) {
  round(as.numeric(((yp_fall_n[j,1])/length(unique(yp_fall$fid)))*100),1)
})))

## Add prey names, fish species, and season to data frame
yp_fall_perc %<>% transmute(type = taxa_list,
                            type_sum = V1,
                            species = "Yellow Perch",
                            year = 2015,
                            season = "Autumn")

##### WHITE PERCH #####
## Calculate the number of fish found with each prey taxa
wp_fall_n <- as.data.frame(do.call(rbind,lapply(taxa_list,function(i) {
  wp_fall %<>% filter(type == i) %>% distinct(fid)
  nrow(wp_fall)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
wp_fall_perc <- as.data.frame(do.call(rbind,lapply(1:nrow(wp_fall_n),function(j) {
  round(as.numeric(((wp_fall_n[j,1])/length(unique(wp_fall$fid)))*100),1)
})))

## Add prey names, fish species, and season to data frame
wp_fall_perc %<>% transmute(type = taxa_list,
                            type_sum = V1,
                            species = "White Perch",
                            year = 2015,
                            season = "Autumn")

## -----------------------------------------------------------
## Combine into a final data frame
## -----------------------------------------------------------
perc_comb <- bind_rows(yp_spring_perc,wp_spring_perc,yp_fall_perc,wp_fall_perc)

## -----------------------------------------------------------
## Combine historical data with current data.
## -----------------------------------------------------------
hist_perc <- bind_rows(hist_perc,perc_comb) %>% 
  mutate(species=factor(species,levels=c("Yellow Perch","White Perch"),ordered=TRUE),
         season=factor(season,levels=c("Spring","Autumn"),ordered=TRUE),
         type=factor(type,levels=c("Zooplankton","Benthic Invertebrates","Fish Prey"),ordered=TRUE))

## ---------------------------------------------------
## Make individual plots
## ---------------------------------------------------
zoop_spring <- ggplot(filter(hist_perc,type=="Zooplankton",season=="Spring"),aes(year,type_sum)) +
  geom_point(aes(colour=species),size=1.5) +
  geom_line(aes(linetype=species,colour=species),size=0.85) +
  scale_color_grey(start=0.1,end=0.6) +
  scale_y_continuous(limits = c(-2,102),expand=c(0,0)) +
  scale_x_continuous(limits=c(2004.9,2015.1),breaks=seq(2005,2015,2),expand=c(0,0)) +
  labs(title='Spring',x='',y='Zooplankton') +
  theme(axis.text.y=element_text(size=15),axis.text.x=element_blank(),
        axis.line=element_line(),legend.title=element_blank(),legend.position="top",legend.text=element_text(size=18),
        panel.background=element_blank(),strip.text=element_blank(),axis.title=element_text(size=13.5),
        plot.margin=unit(c(1,6,0,6),"mm"),panel.margin=unit(1.5,"lines"))

zoop_fall <- ggplot(filter(hist_perc,type=="Zooplankton",season=="Autumn"),aes(year,type_sum)) +
  geom_point(aes(colour=species),size=1.5) +
  geom_line(aes(linetype=species,colour=species),size=0.85) +
  scale_color_grey(start=0.1,end=0.6) +
  scale_y_continuous(limits = c(-2,102),expand=c(0,0)) +
  scale_x_continuous(limits=c(2004.9,2015.1),breaks=seq(2005,2015,2),expand=c(0,0)) +
  labs(title='Autumn',x='',y='') +
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),
        axis.line=element_line(),axis.title=element_text(size=13.5),
        panel.background=element_blank(),strip.text=element_blank(),
        plot.margin=unit(c(1,14,0,3),"mm"),panel.margin=unit(1.5,"lines"))

benthos_spring <- ggplot(filter(hist_perc,type=="Benthic Invertebrates",season=="Spring"),aes(year,type_sum)) +
  geom_point(aes(colour=species),size=1.5) +
  geom_line(aes(linetype=species,colour=species),size=0.85) +
  scale_color_grey(start=0.1,end=0.6) +
  scale_y_continuous(limits = c(-2,102),expand=c(0,0)) +
  scale_x_continuous(limits=c(2004.9,2015.1),breaks=seq(2005,2015,2),expand=c(0,0)) +
  labs(title='',x='',y='Benthic Macroinvertebrates') +
  theme(axis.text.y=element_text(size=15),axis.text.x=element_blank(),
        axis.line=element_line(),axis.title=element_text(size=13.5),
        panel.background=element_blank(),strip.text=element_blank(),
        plot.margin=unit(c(1,6,0,6),"mm"),panel.margin=unit(1.5,"lines"))

benthos_fall <- ggplot(filter(hist_perc,type=="Benthic Invertebrates",season=="Autumn"),aes(year,type_sum)) +
  geom_point(aes(colour=species),size=1.5) +
  geom_line(aes(linetype=species,colour=species),size=0.85) +
  scale_color_grey(start=0.1,end=0.6) +
  scale_y_continuous(limits = c(-2,102),expand=c(0,0)) +
  scale_x_continuous(limits=c(2004.9,2015.1),breaks=seq(2005,2015,2),expand=c(0,0)) +
  labs(title='',x='',y='') +
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),
        axis.line=element_line(),axis.title=element_text(size=13.5),
        panel.background=element_blank(),strip.text=element_blank(),
        plot.margin=unit(c(1,14,0,3),"mm"),panel.margin=unit(1.5,"lines"))

fish_spring <- ggplot(filter(hist_perc,type=="Fish Prey",season=="Spring"),aes(year,type_sum)) +
  geom_point(aes(colour=species),size=1.5) +
  geom_line(aes(linetype=species,colour=species),size=0.85) +
  scale_color_grey(start=0.1,end=0.6) +
  scale_y_continuous(limits = c(-2,102),expand=c(0,0)) +
  scale_x_continuous(limits=c(2004.9,2015.1),breaks=seq(2005,2015,2),expand=c(0,0)) +
  labs(title='',x='',y='Fish Prey') +
  theme(axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),
        axis.line=element_line(),axis.title=element_text(size=13.5),
        panel.background=element_blank(),strip.text=element_blank(),
        plot.margin=unit(c(1,6,0,6),"mm"),panel.margin=unit(1.5,"lines"))

fish_fall <- ggplot(filter(hist_perc,type=="Fish Prey",season=="Autumn"),aes(year,type_sum)) +
  geom_point(aes(colour=species),size=1.5) +
  geom_line(aes(linetype=species,colour=species),size=0.85) +
  scale_color_grey(start=0.1,end=0.6) +
  scale_y_continuous(limits = c(-2,102),expand=c(0,0)) +
  scale_x_continuous(limits=c(2004.9,2015.1),breaks=seq(2005,2015,2),expand=c(0,0)) +
  labs(title='',x='',y='') +
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=15),
        axis.line=element_line(),axis.title=element_text(size=13.5),
        panel.background=element_blank(),strip.text=element_blank(),
        plot.margin=unit(c(1,14,0,3),"mm"),panel.margin=unit(1.5,"lines"))

## -----------------------------------------------------------
## Create common legend
## -----------------------------------------------------------
legend = gtable_filter(ggplot_gtable(ggplot_build(zoop_spring)), "guide-box") 

## -----------------------------------------------------------
## Save the plot as a figure (comment out line 144 and 182 until you are ready to save)
## -----------------------------------------------------------
png("2015/Figures/historical_percent_occurence.PNG",width=11,height=8,units="in",family="Times",res=300)

## -----------------------------------------------------------
## Put plots into a matrix
## -----------------------------------------------------------
grid.arrange(arrangeGrob(zoop_spring + theme(legend.position="none"),
                                zoop_fall + theme(legend.position="none"),
                                benthos_spring + theme(legend.position="none"),
                                benthos_fall + theme(legend.position="none"),
                                fish_spring + theme(legend.position="none"),
                                fish_fall + theme(legend.position="none"),
                                ncol=2,
                                nrow=3,
                                left=textGrob("% Frequency of Occurrence",rot=90,vjust=0.5,gp=gpar(fontsize=24)),
                                bottom=textGrob("Year",gp=gpar(fontsize=20))),
             legend,
             heights=c(10,1))

## -----------------------------------------------------------
## Close the device to make the actual PNG file
## -----------------------------------------------------------
dev.off()
>>>>>>> origin/master
