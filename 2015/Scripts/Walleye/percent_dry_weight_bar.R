## Load Data
source("2015/Scripts/Walleye/data_init.R")

## -----------------------------------------------------------
## Remove stomachs with no diet contents
## -----------------------------------------------------------
walleye_diets %<>% filter(food_item != "Empty")

## -----------------------------------------------------------
## Merge prey type (matching prey taxa)
## -----------------------------------------------------------
walleye_diets %<>% left_join(prey_type) %>% arrange(type,food_item)
# Check for NA's in the new prey_type variable (NA = typos, inconsistency between prey names, etc.)

## -----------------------------------------------------------
## Filter all values less than 5% and summarize by functional group into "Other Species"
## -----------------------------------------------------------
taxa_more <- walleye_diets %>% filter(food_item %in% c("Yellow Perch","Rainbow Smelt", "Gizzard Shad","Round Goby",
                                                      "Emerald Shiner","Cercopagididae","Leptodoridae","Unidentified fish")) # save to join back to later
taxa_less <- walleye_diets %>% filter(food_item %in% c("Amphipoda","Ephemeridae","Chironomidae","Nematoda","Oligochaeta"))
taxa_less$food_item <- 'Benthic Macroinvertebrates'
taxa_comb <- bind_rows(taxa_more,taxa_less) %>% 
  mutate(food_item=factor(food_item,levels = c('Cercopagididae','Leptodoridae','Benthic Macroinvertebrates','Emerald Shiner','Gizzard Shad','Rainbow Smelt','Round Goby','Yellow Perch','Unidentified fish'),ordered = TRUE)) %>% 
  group_by(fid,serial,food_item) %>% 
  summarize(final_dw_mg = sum(final_dw_mg))


## -----------------------------------------------------------
## Filter into each species and both seasons
## -----------------------------------------------------------
walleye_list <- unique(taxa_comb$fid)

## -----------------------------------------------------------
## Create a list of prey types found for both seasons, uniquely combine, and order by functional group and alphabetically
## -----------------------------------------------------------
diet_list <- unique(taxa_comb$food_item)

## -----------------------------------------------------------
### Percent Dry Weight
## -----------------------------------------------------------
##### YELLOW PERCH #####
## Calculate the mean percent by dry weight for each prey taxa
walleye_perc <- as.data.frame(do.call(cbind,lapply(walleye_list,function(i) {
  fish <- filter(taxa_comb,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet_list,function(j) {
    round(sum(as.numeric(filter(fish,food_item == j)$final_dw_mg))/sum(as.numeric(fish$final_dw_mg))*100,1)
  })))
})))

## Calculate row means
walleye_perc %<>% transform(mean = apply(walleye_perc,1,mean),
                            sd = apply(walleye_perc,1,sd)) %>% 
  mutate(n=rowSums(walleye_perc > 0),
         se=sd/(sqrt(n)))

## Add prey names to data frame
walleye_perc %<>% transmute(percent = mean,
                            se=se,
                            prey_type = diet_list)
walleye_perc[is.na(walleye_perc)] <- 0

## -----------------------------------------------------------
## Join more and less data frames together
## -----------------------------------------------------------
walleye_perc %<>% mutate(prey_type=factor(prey_type,levels = c('Cercopagididae','Leptodoridae','Benthic Macroinvertebrates','Emerald Shiner','Gizzard Shad','Rainbow Smelt','Round Goby','Yellow Perch','Unidentified fish'),ordered = TRUE)) %>% 
  arrange(prey_type)

## -----------------------------------------------------------
## Save the plot as a figure (comment out line 186 and 226 until you are ready to save)
## -----------------------------------------------------------
png("2015/Figures/walleye_percent_dry_weight_bar.PNG",width=5,height=7,units="in",family="Times",res=300)

## Make plot
ggplot(walleye_perc,aes(prey_type,percent)) +
  geom_errorbar(aes(x=prey_type,ymin=0,ymax=percent+se),
                width=0.2,
                position = position_dodge(.9)) +
  geom_bar(stat="identity",width=0.75) +
  ylab('Diet Composition (% dry weight)') +
  xlab('') +
  scale_y_continuous(limits = c(0,75),breaks=c(0,25,50,75),expand=c(0,0)) +
  theme(axis.text.x=element_text(angle=90,vjust=0.3,hjust=1,size=12),
        axis.text.y=element_text(size=16),
        axis.line=element_line(size=0.5),panel.background=element_blank(),
        axis.title.y=element_text(size=16,margin=margin(0,20,0,0)))

## -----------------------------------------------------------
## Close the device to make the actual PNG file
## -----------------------------------------------------------
dev.off()
