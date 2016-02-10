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
diet_spring %<>% left_join(prey_type) %>% arrange(type,food_item)
diet_fall %<>% left_join(prey_type) %>% arrange(type,food_item)
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
## Create a list of prey types found for both seasons, uniquely combine, and order by functional group and alphabetically
## -----------------------------------------------------------
diet_spring_list <- distinct(diet_spring,food_item)
diet_fall_list <- distinct(diet_fall,food_item)
diet_list <- bind_rows(diet_spring_list,diet_fall_list) %>% 
  arrange(type,food_item) %>% 
  distinct(food_item) %>% 
  select(food_item)
diet_list <- as.character(diet_list$food_item)

## -----------------------------------------------------------
### SPRING
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the mean percent by dry weight for each prey taxa
yp_spring_perc <- as.data.frame(do.call(cbind,lapply(yp_spring_list,function(i) {
  fish <- filter(yp_spring,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet_list,function(j) {
    round((sum(filter(fish,food_item == j)$final_dw_mg)/sum(fish$final_dw_mg))*100,2)
  })))
})))

## Calculate row means and standard error
yp_spring_perc %<>% transform(mean = apply(yp_spring_perc,1,mean),
                              sd = apply(yp_spring_perc,1,sd)) %>% 
  mutate(n=rowSums(yp_spring_perc > 0),
         se=sd/(sqrt(n)))

## Add prey names to data frame
yp_spring_perc %<>% transmute(percent = mean,
                              se = se,
                              prey_type = diet_list,
                              species = "Yellow Perch",
                              season = "Spring")

##### WHITE PERCH #####
## Calculate the mean percent by dry weight for each prey taxa
wp_spring_perc <- as.data.frame(do.call(cbind,lapply(wp_spring_list,function(i) {
  fish <- filter(wp_spring,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet_list,function(j) {
    round((sum(filter(fish,food_item == j)$final_dw_mg)/sum(fish$final_dw_mg))*100,2)
  })))
})))

## Calculate row means and standard error
wp_spring_perc %<>% transform(mean = apply(wp_spring_perc,1,mean),
                              sd = apply(wp_spring_perc,1,sd)) %>% 
  mutate(n=rowSums(wp_spring_perc > 0),
         se=sd/(sqrt(n)))

## Add prey names to data frame
wp_spring_perc %<>% transmute(percent = mean,
                              se = se,
                              prey_type = diet_list,
                              species = "White Perch",
                              season = "Spring")

## -----------------------------------------------------------
### FALL
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the percent by dry weight for each prey taxa
yp_fall_perc <- as.data.frame(do.call(cbind,lapply(yp_fall_list,function(i) {
  fish <- filter(yp_fall,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet_list,function(j) {
    round((sum(filter(fish,food_item == j)$final_dw_mg)/sum(fish$final_dw_mg))*100,2)
  })))
})))

## Calculate row means and standard error
yp_fall_perc %<>% transform(mean = apply(yp_fall_perc,1,mean),
                            sd = apply(yp_fall_perc,1,sd)) %>% 
  mutate(n=rowSums(yp_fall_perc > 0),
         se=sd/(sqrt(n)))

## Add prey names to data frame
yp_fall_perc %<>% transmute(percent = mean,
                            se = se,
                            prey_type = diet_list,
                            species = "Yellow Perch",
                            season = "Autumn")

##### WHITE PERCH #####
## Calculate the percent by dry weight for each prey taxa
wp_fall_perc <- as.data.frame(do.call(cbind,lapply(wp_fall_list,function(i) {
  fish <- filter(wp_fall,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet_list,function(j) {
    round((sum(filter(fish,food_item == j)$final_dw_mg)/sum(fish$final_dw_mg))*100,2)
  })))
})))

## Calculate row means and standard error
wp_fall_perc %<>% transform(mean = apply(wp_fall_perc,1,mean),
                            sd = apply(wp_fall_perc,1,sd)) %>% 
  mutate(n=rowSums(wp_fall_perc > 0),
         se=sd/(sqrt(n)))

## Add prey names to data frame
wp_fall_perc %<>% transmute(percent = mean,
                            se = se,
                            prey_type = diet_list,
                            species = "White Perch",
                            season = "Autumn")

## -----------------------------------------------------------
## Combine into a final data frame
## -----------------------------------------------------------
perc_comb <- data.frame(rbind(yp_spring_perc,wp_spring_perc,yp_fall_perc,wp_fall_perc))
perc_comb[is.na(perc_comb)] <- 0

## -----------------------------------------------------------
## Merge prey type (matching prey taxa) - Used to summarize low dry weight species
## -----------------------------------------------------------
prey_type %<>% select(prey_type=food_item,type)
perc_comb %<>% left_join(prey_type)
# Check for NA's in the new prey_type variable (NA's = typos, inconsistency between prey names, etc.)

## -----------------------------------------------------------
## Filter all values less than 5% and summarize by functional group into "Other Species"
## -----------------------------------------------------------
perc_comb_more <- perc_comb %>% filter(prey_type %in% c("Amphipoda","Ephemeridae","Round Goby",
                                                        "Yellow Perch","Daphnidae","Chironomidae",
                                                        "Trichoptera","Dreissenidae",
                                                        "Emerald Shiner","Cercopagididae","Gastropoda",
                                                        "Leptodoridae","Unidentified fish")) # save to join back to later
perc_comb_less <- perc_comb %>% filter(prey_type %in% c("Sphaeriidae","Hirudinea","Spottail Shiner",
                                                        "Bosminidae","Nematoda","Sididae",
                                                        "Oligochaeta","Fish eggs","White Bass",
                                                        "Calanoida","Ostracoda","Gizzard Shad",
                                                        "White Perch","Cyclopoida","Hemimysis"))
perc_comb_less %<>% group_by(species,season,type) %>% 
  summarise(percent=sum(percent),
            se=mean(se)) %>% ungroup() %>% 
  mutate(prey_type=paste(type,"spp.",sep=" "))
perc_comb_less$prey_type <- gsub("Benthic Invertebrates","Benthic",perc_comb_less$prey_type)
perc_comb_less$prey_type <- gsub(" Prey","",perc_comb_less$prey_type)

## -----------------------------------------------------------
## Join more and less data frames together
## -----------------------------------------------------------
perc_comb_final <- bind_rows(perc_comb_more,perc_comb_less) %>% 
  mutate(prey_type=factor(prey_type,levels = c('Cercopagididae','Daphnidae','Leptodoridae','Zooplankton spp.','Amphipoda','Chironomidae','Dreissenidae','Ephemeridae','Gastropoda','Hemimysis','Trichoptera','Benthic spp.','Emerald Shiner','Round Goby','Yellow Perch','Unidentified fish','Fish spp.'),ordered = TRUE),
         season=factor(season,levels = c('Spring','Autumn',ordered=TRUE)),
         species=factor(species,levels=c('Yellow Perch','White Perch'))) %>% 
  arrange(species,season,type)

## -----------------------------------------------------------
## Make plots
## -----------------------------------------------------------
yp <- ggplot(filter(perc_comb_final,species=="Yellow Perch"),aes(prey_type,percent,fill=season)) +
  geom_errorbar(aes(x=prey_type,ymin=0,ymax=percent+se),
                width=0.25,
                position = position_dodge(.9)) +
  geom_bar(stat="identity",position="dodge") +
  scale_fill_grey(start=0.3,end=0.7) +
  scale_y_continuous(limits = c(0,80),expand=c(0,0)) +
  labs(title="Yellow Perch") +
  theme(axis.text.x=element_text(angle=90,vjust=0.3,hjust=1,size=20),axis.text.y=element_text(size=24,hjust=1),
        axis.line=element_line(),legend.position="bottom",legend.title=element_blank(),legend.text=element_text(size=24),
        panel.background=element_blank(),axis.title=element_blank(),plot.title=element_text(size=24),
        plot.margin=unit(c(2,2,0,10),"mm"),axis.ticks.length=unit(1.75,'mm'))

wp <- ggplot(filter(perc_comb_final,species=="White Perch"),aes(prey_type,percent,fill=season)) +
  geom_errorbar(aes(x=prey_type,ymin=0,ymax=percent+se),
                width=0.25,
                position = position_dodge(.9)) +
  geom_bar(stat="identity",position="dodge") +
  scale_fill_grey(start=0.3,end=0.7) +
  scale_y_continuous(limits = c(0,80),expand=c(0,0)) +
  labs(title="White Perch") +
  theme(axis.text.x=element_text(angle=90,vjust=0.3,hjust=1,size=20),axis.text.y=element_text(size=24,hjust=1),
        axis.line=element_line(),legend.position="bottom",legend.title=element_blank(),legend.text=element_text(size=24),
        panel.background=element_blank(),axis.title=element_blank(),plot.title=element_text(size=24),
        plot.margin=unit(c(2,2,0,10),"mm"),axis.ticks.length=unit(1.75,'mm'))

## -----------------------------------------------------------
## Create common legend
## -----------------------------------------------------------
legend = gtable_filter(ggplot_gtable(ggplot_build(yp)), "guide-box") 

## -----------------------------------------------------------
## Save the plot as a figure (comment out line 186 and 226 until you are ready to save)
## -----------------------------------------------------------
png("2015/Figures/percent_dry_weight_bar.PNG",width=12,height=12,units="in",family="Times",res=300)

## -----------------------------------------------------------
## Put plots into a matrix
## -----------------------------------------------------------
grid.arrange(arrangeGrob(yp + theme(legend.position="none"),
                         wp + theme(legend.position="none"),
                         ncol=1,
                         nrow=2,
                         left=textGrob("Diet Composition (% Dry Weight)",y=unit(160,'mm'),rot=90,gp=gpar(fontsize=30))),
             legend,
             heights=c(8,1))

## -----------------------------------------------------------
## Close the device to make the actual PNG file
## -----------------------------------------------------------
dev.off()
