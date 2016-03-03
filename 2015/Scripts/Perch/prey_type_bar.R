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
## Filter into each species and both seasons
## -----------------------------------------------------------
yp_spring <- filter(diet_spring,species=="Yellow Perch")
yp_spring_list <- unique(yp_spring$fid)
wp_spring <- filter(diet_spring,species=="White Perch")
wp_spring_list <- unique(wp_spring$fid)
yp_fall <- filter(diet_fall,species=="Yellow Perch")
yp_fall_list <- unique(yp_fall$fid)
wp_fall <- filter(diet_fall,species=="White Perch")
wp_fall_list <- unique(wp_fall$fid)

## -----------------------------------------------------------
## Creat list of prey types
## -----------------------------------------------------------
taxa_list <- c("Zooplankton","Benthic Invertebrates","Fish Prey")

## -----------------------------------------------------------
### SPRING
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the mean percent by dry weight for each prey taxa
yp_spring_perc <- as.data.frame(do.call(cbind,lapply(yp_spring_list,function(i) {
  fish <- filter(yp_spring,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(taxa_list,function(j) {
    round((sum(filter(fish,type == j)$final_dw_mg)/sum(fish$final_dw_mg))*100,1)
  })))
})))

## Calculate row means
yp_spring_perc %<>% transform(mean = apply(yp_spring_perc,1,mean),
                              sd = apply(yp_spring_perc,1,sd)) %>% 
  mutate(n=rowSums(yp_spring_perc > 0),
         se=sd/(sqrt(n)))

## Add prey names to data frame
yp_spring_perc %<>% transmute(percent = mean,
                              se=se,
                              prey_type = taxa_list,
                              species = "Yellow Perch",
                              season = "Spring")

##### WHITE PERCH #####
## Calculate the mean percent by dry weight for each prey taxa
wp_spring_perc <- as.data.frame(do.call(cbind,lapply(wp_spring_list,function(i) {
  fish <- filter(wp_spring,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(taxa_list,function(j) {
    round((sum(filter(fish,type == j)$final_dw_mg)/sum(fish$final_dw_mg))*100,1)
  })))
})))

## Calculate row means
wp_spring_perc %<>% transform(mean = apply(wp_spring_perc,1,mean),
                              sd = apply(wp_spring_perc,1,sd)) %>% 
  mutate(n=rowSums(wp_spring_perc > 0),
         se=sd/(sqrt(n)))

## Add prey names to data frame
wp_spring_perc %<>% transmute(percent = mean,
                              se=se,
                              prey_type = taxa_list,
                              species = "White Perch",
                              season = "Spring")

## -----------------------------------------------------------
### FALL
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the percent by dry weight for each prey taxa
yp_fall_perc <- as.data.frame(do.call(cbind,lapply(yp_fall_list,function(i) {
  fish <- filter(yp_fall,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(taxa_list,function(j) {
    round((sum(filter(fish,type == j)$final_dw_mg)/sum(fish$final_dw_mg))*100,1)
  })))
})))

## Calculate row means
yp_fall_perc %<>% transform(mean = apply(yp_fall_perc,1,mean),
                            sd = apply(yp_fall_perc,1,sd)) %>% 
  mutate(n=rowSums(yp_fall_perc > 0),
         se=sd/(sqrt(n)))

## Add prey names to data frame
yp_fall_perc %<>% transmute(percent = mean,
                            se=se,
                            prey_type = taxa_list,
                            species = "Yellow Perch",
                            season = "Fall")

##### WHITE PERCH #####
## Calculate the percent by dry weight for each prey taxa
wp_fall_perc <- as.data.frame(do.call(cbind,lapply(wp_fall_list,function(i) {
  fish <- filter(wp_fall,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(taxa_list,function(j) {
    round((sum(filter(fish,type == j)$final_dw_mg)/sum(fish$final_dw_mg))*100,1)
  })))
})))

## Calculate row means
wp_fall_perc %<>% transform(mean = apply(wp_fall_perc,1,mean),
                            sd = apply(wp_fall_perc,1,sd)) %>% 
  mutate(n=rowSums(wp_fall_perc > 0),
         se=sd/(sqrt(n)))

## Add prey names to data frame
wp_fall_perc %<>% transmute(percent = mean,
                            se = se,
                            prey_type = taxa_list,
                            species = "White Perch",
                            season = "Fall")

## -----------------------------------------------------------
## Combine into a final data frame
## -----------------------------------------------------------
perc_comb <- bind_rows(yp_spring_perc,wp_spring_perc,yp_fall_perc,wp_fall_perc) %>% 
  mutate(prey_type=factor(prey_type,levels = c("Zooplankton","Benthic Invertebrates","Fish Prey"),ordered=TRUE))

## -----------------------------------------------------------
## Make plots
## -----------------------------------------------------------
yp <- ggplot(filter(perc_comb,species=="Yellow Perch"),aes(season,percent,fill=prey_type)) +
  geom_errorbar(aes(x=season,ymin=0,ymax=percent+se),
                width=0.25,
                position = position_dodge(.9)) +
  geom_bar(stat="identity",position="dodge",col=I("black")) +
  scale_fill_grey(start=0.2,end=0.8) +
  scale_y_continuous(limits = c(0,80),expand=c(0,0)) +
  labs(title="Yellow Perch") +
  theme(axis.text.x=element_text(size=22,vjust=0),axis.text.y=element_text(size=24,hjust=1),
        axis.line=element_line(),axis.title=element_blank(),axis.ticks.length=unit(1.75,'mm'),
        legend.position="bottom",legend.title=element_blank(),legend.text=element_text(size=24),
        panel.background=element_blank(),axis.title=element_blank(),plot.title=element_text(size=24),
        plot.margin=unit(c(2,2,2,10),"mm"))

wp <- ggplot(filter(perc_comb,species=="White Perch"),aes(season,percent,fill=prey_type)) +
  geom_errorbar(aes(x=season,ymin=0,ymax=percent+se),
                width=0.25,
                position = position_dodge(.9)) +
  geom_bar(stat="identity",position="dodge",col=I("black")) +
  scale_fill_grey(start=0.2,end=0.8) +
  scale_y_continuous(limits = c(0,80),expand=c(0,0)) +
  labs(title="White Perch") +
  theme(axis.text.x=element_text(size=22,vjust=0),axis.text.y=element_text(size=24,hjust=1),
        axis.line=element_line(),axis.title=element_blank(),axis.ticks.length=unit(1.75,'mm'),
        legend.position="bottom",legend.title=element_blank(),legend.text=element_text(size=24),
        panel.background=element_blank(),plot.title=element_text(size=24),
        plot.margin=unit(c(2,2,2,10),"mm"))

## -----------------------------------------------------------
## Create common legend
## -----------------------------------------------------------
legend = gtable_filter(ggplot_gtable(ggplot_build(yp)), "guide-box") 

## -----------------------------------------------------------
## Save the plot as a figure (comment out line 186 and 226 until you are ready to save)
## -----------------------------------------------------------
png("2015/Figures/prey_type_bar.PNG",width=10,height=12,units="in",family="Times",res=300)

## -----------------------------------------------------------
## Put plots into a matrix
## -----------------------------------------------------------
grid.arrange(arrangeGrob(yp + theme(legend.position="none"),
                         wp + theme(legend.position="none"),
                         ncol=1,
                         nrow=2,
                         left=textGrob("Diet Composition (% Dry Weight)",rot=90,gp=gpar(fontsize=32))),
             legend,
             heights=c(8,1))

## -----------------------------------------------------------
## Close the device to make the actual PNG file
## -----------------------------------------------------------
dev.off()
