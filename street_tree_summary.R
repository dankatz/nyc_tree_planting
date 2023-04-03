#set up work environment
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(readr)
library("stringr")
library(tibble)

options(scipen=9)

#rm(list=ls())

here::i_am("street_tree_summary.R") #using the 'here' package for consistent file structure system

### load in street tree data and create derived vars ####################################
st_trees <- read_csv( "C:/Users/danka/Box/NYC projects/tree data/2015 Street Tree Census - Tree Data/2015StreetTreesCensus_TREES.csv")
st_trees <- st_trees %>% 
  mutate(genus = sub(" .*", "", spc_latin),
         dbh_cm = 2.54 * tree_dbh,
         BA_m2 = 0.00007854 *  dbh_cm^2 ) %>% 
  filter(!is.na(genus) &
           status == "Alive") %>% 
  filter(dbh_cm < 170) %>% #there area  few trees that have implausibly large DBH; I am removing those here
  mutate(spc_latin =  case_when(spc_latin == "Acer platanoides 'Crimson King'" ~ "Acer platanoides",
                                spc_latin == "Cedrus atlantica glauca" ~ "Cedrus atlantica",
                                spc_latin == "Gleditsia triacanthos var. inermis" ~ "Gleditsia triacanthos",
                                TRUE ~ spc_latin)) 

#test$genus
hist(st_trees$BA_m2)

#export table with basal area summary at genus level
st_genus_summary <- st_trees %>%  group_by(genus) %>% 
  summarize(total_BA = sum(BA_m2, na.rm = TRUE),
            n_trees = n()) %>% 
  arrange(-total_BA) %>% 
  mutate(prop_ba = total_BA / sum(total_BA),
         prop_n = n_trees / sum(n_trees))

#write_csv(st_genus_summary, here("st_tree_genus_summary.csv"))


#export table with basal area summary at species level
st_spp_summary <- st_trees %>%  group_by(spc_latin) %>% 
  summarize(total_BA = sum(BA_m2, na.rm = TRUE),
            n_trees = n()) %>% 
  arrange(-total_BA) %>% 
  mutate(prop_ba = total_BA / sum(total_BA),
         prop_n = n_trees / sum(n_trees))

#write_csv(st_spp_summary, here("st_tree_species_summary.csv"))

#test<-st_trees %>% filter(genus == "Ulmus")

### add itree data ######################################################
it_trees <- read_csv( "C:/Users/danka/Box/NYC projects/tree data/2013_itree_plots/itree_2013_spp_summary.csv") 
it_trees_join <- it_trees %>% 
  mutate(spc_latin = paste(Genus, Species),
         spc_latin = gsub(x = spc_latin, pattern = " NA", replacement = ""),
         prop_ba = basal_area / sum(basal_area),
         prop_n = Trees / sum(Trees)) %>%
  rename(genus = Genus,
         n_trees_it = Trees,
         total_BA_it = basal_area,
         prop_ba_it = prop_ba,
         prop_n_it = prop_n
         ) %>% 
  dplyr::select(-dbh_median, -dbh_mean) #check: sum(it_trees$prop_n)
# it_common_trees <- it_trees %>% 
#                     filter(prop_ba > 0.001 | prop_n > 0.001)


### combine street tree and itree data for table 1: species###################################
st_spp_summary 
  
it_trees_join <- it_trees_join %>% 
  mutate(spc_latin = case_when(spc_latin == "Acer species" ~ "Acer",
                               spc_latin == "Cedrus atlantica glauca" ~ "Cedrus atlantica",
                               spc_latin == "Platanus hybrida" ~ "Platanus x acerifolia",
                               TRUE ~ spc_latin))

unique(st_spp_summary$spc_latin)
unique(it_trees_join$spc_latin)

setdiff(st_spp_summary$spc_latin, it_trees_join$spc_latin) %>% as.character() %>% sort()
setdiff(it_trees_join$spc_latin, st_spp_summary$spc_latin) %>% as.character() %>% sort()

#write.table(st_spp_summary$spc_latin, "clipboard", sep="\t", row.names=FALSE)


it_st_species <- full_join(it_trees_join, st_spp_summary) %>% 
  dplyr::select(species = spc_latin, prop_ba_it, prop_ba, prop_n_it, prop_n) %>% 
  arrange(species)
  #arrange(-prop_ba_it)
#write_csv(it_trees, "C:/Users/danka/Box/NYC projects/tree data/2013_itree_plots/itree_2013_spp_summary_derived_vars.csv")

### combine street tree and itree data for table 1: genus ###################################
 
it_trees_genus_join <- it_trees_join %>% 
  group_by(genus) %>% 
  summarize(prop_ba_it = round(sum(prop_ba_it, na.rm = TRUE), 4),
            prop_n_it  = round(sum(prop_n_it , na.rm = TRUE), 4)) %>% 
  dplyr::select(genus, prop_ba_it, prop_n_it)
st_genus_summary

it_st_genus <- full_join(it_trees_genus_join, st_genus_summary) %>% 
   mutate(prop_ba_st = round(prop_ba, 4)*100,
         prop_n_st = round(prop_n, 4)*100,
         prop_ba_it = prop_ba_it * 100,
         prop_n_it = prop_n_it * 100) %>% 
  dplyr::select(-n_trees, - total_BA, -prop_ba, -prop_n) %>% 
  arrange(-prop_ba_it)

it_st_genus <- it_st_genus %>% mutate_all(~replace(., is.na(.), 0))




### add in NYC airborne pollen data #########################################################
p <- read_csv("C:/Users/danka/Box/collaborations/NAB_NPN/NAB_pollen_220128c.csv") %>% 
  filter(Station.City == "New York City") %>% 
  filter(!is.na(Acer))
p_mean <- p %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% t() %>% as.data.frame() %>% 
  rownames_to_column(var = "genus") %>% 
  rename(pollen_mean = V1)

p_max <- p %>%
  summarise_if(is.numeric, max, na.rm = TRUE) %>% t() %>% as.data.frame() %>% 
  rownames_to_column(var = "genus") %>% 
  rename(pollen_max = V1)

p_sum <- p %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% t() %>% as.data.frame() %>% 
  rownames_to_column(var = "genus") %>% 
  rename(pollen_sum = V1)


p_mean_max <- left_join(p_mean, p_max)

#proportion of pollen collected by genus
p_sum %>% 
  mutate(pollen_prop = pollen_sum/sum(p_sum$pollen_sum)) %>% 
  filter(pollen_prop > 0.01) %>% 
ggplot(aes(x = reorder(genus, -pollen_prop), y = pollen_prop * 100)) + geom_col() + ggthemes::theme_few() + xlab("taxon") + ylab("pollen collected (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) #ylab(average~pollen~(grains/m^3))
#ggsave("pollen_percent_genus.jpeg", dpi = 300, width = 7, height = 4, units = "in")

p_prop <- p_sum %>% 
  mutate(pollen_prop = pollen_sum/sum(p_sum$pollen_sum)) %>% 
  #filter(pollen_prop > 0.01) %>% 
  arrange(-pollen_prop) %>% 
  dplyr::select(-pollen_sum)


p_prop_max <- left_join(p_prop, p_max)


### export table 1 ########################################################################
table1 <- left_join(it_st_genus, p_prop_max)
#write_csv(it_st_genus, "C:/Users/danka/Box/NYC projects/tree data/itree_st_trees_genus_summary.csv")
#write.table(table1, "clipboard", sep="\t", row.names=FALSE)

setdiff(it_st_genus$genus, p_mean_max$genus) %>% as.character() %>% sort()
setdiff(p_mean_max$genus, it_st_genus$genus) %>% as.character() %>% sort()

table1_small <- table1 %>% 
  filter(pollen_prop > .01 | prop_ba_it > 1 | prop_ba_st > 1)

write.table(table1_small, "clipboard", sep="\t", row.names=FALSE)



### calculate pollen production from each spp for i-Tree plots ##############################################

#load DBH table and convert to long format
it_dbh_genus <- read_csv("C:/Users/danka/Box/NYC projects/tree data/2013_itree_plots/DBH_of_species_v2.csv") %>% 
  select(-contains("SE")) %>% 
  pivot_longer(cols = contains("DBH"), names_to = "DBH", values_to = "perc_trees") %>% 
  mutate(DBH = as.numeric(gsub("[^0-9.-]", "", DBH)))
  
#add in latin names
it_common_latin_join <- it_trees %>% select(1:3) %>% rename(common_name = 3)
it_dbh_genus <- left_join(it_dbh_genus, it_common_latin_join) 

#select only genera with pollen production equations
pollen_prod_focal_genera <- c("Acer", "Betula", "Platanus", "Quercus", "Morus", "Populus", "Gleditsia", "Juglans", "Ulmus")
it_dbh_genus <- it_dbh_genus %>% filter(Genus %in% pollen_prod_focal_genera)

#multiply by number of trees for that species to get number in that DBH category
it_dbh_genus_n <- left_join(it_dbh_genus, it_trees) %>% 
  mutate(n_trees_DBH_cat = (perc_trees/100) * Trees) %>% 
  mutate(tree_BA = 0.00007854 * DBH * DBH)

#calculate pollen production for each species
unique(it_dbh_genus_n$Species)

it_dbh_genus_np <- 
  it_dbh_genus_n %>%  
  mutate(per_tree_pollen_prod = case_when(
    Genus == "Acer" & Species == "negundo" ~ 253.71 * tree_BA + 0.38,
    Genus == "Acer" & Species == "platanoides" ~ 25.59 * tree_BA + 1.22,
    Genus == "Acer" & Species == "rubrum" ~ (62.32 * tree_BA + 1.27)*0.106, #.106 is the sex ratio
    Genus == "Acer" & Species == "saccharinum" ~ (exp(2.28 * tree_BA + 21.98))/1000000000, #convert to billions
    Genus == "Betula"  ~ 561.16 * tree_BA + 5.03,
    Genus == "Gleditsia"  ~ 659.91 * tree_BA -3.25,
    Genus == "Juglans"  ~ 239.08 * tree_BA + 11.47,
    Genus == "Morus"  ~ (6021.57 * tree_BA^2 + 1366.09 * tree_BA + 254.06)*0.578, #.58 adjusts for sex ratio
    Genus == "Platanus"  ~ 1066.75 * tree_BA + 1.26,
    Genus == "Populus"  ~ exp(2.01 * tree_BA + 24.17) * 0.482, #weren't any recorded in the dbh data so this won't be included
    Genus == "Quercus"  ~ 423.56 * tree_BA + 36.20, #red oaks and unknown oaks
    Genus == "Quercus" & Species == "palustris" ~ 327.2 * tree_BA + 14.9, #pin oaks
    Genus == "Ulmus"  ~ (exp(5.86 * tree_BA + 23.11))/1000000000 #convert to billions
  )) #did a gut check against fig 3 in Katz et al. 2020; all of these currently line up

#calculate total pollen production for each taxon
citywide_pol <- 
  it_dbh_genus_np %>% 
  mutate(p_all_trees = per_tree_pollen_prod * n_trees_DBH_cat,
         genus_species = paste(Genus, Species, sep = " ")) %>% 
  group_by(Genus, Species, genus_species) %>% 
  summarize(total_p_bil = sum(p_all_trees)) %>%  #adding each tree and also removing the billions
  filter(!is.na(total_p_bil)) %>% 
  mutate(total_p = total_p_bil * 1000000000,
         total_p_tril = total_p / 10^12,
         total_p_quad = total_p / 10^15)
  
citywide_pol %>% 
  group_by(Genus) %>% 
  summarize(total_p_quad_gen = sum(total_p_quad)) %>% 
ggplot(aes(x = reorder(Genus, -total_p_quad_gen), y = total_p_quad_gen)) + geom_col()+ ggthemes::theme_few() + xlab("genus") +
  ylab("pollen produced (quadrillions)") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) #ylab(average~pollen~(grains/m^3))
ggsave("pollen_prod_total_genus.jpeg", dpi = 300, width = 7, height = 4, units = "in")

###  calculate pollen production for all street trees ##############################################

names(st_trees)
test <- st_trees %>% filter(spc_latin == "Ulmus americana") 

unique(st_trees$spc_latin) %>% as.character() %>% sort()#arrange(.)
citywide_pol_st <- st_trees %>% 
  select(spc_latin, genus, BA_m2, dbh_cm) %>% 
  mutate(tree_BA = BA_m2) %>% 
  mutate(tree_BA = case_when(genus == "Ulmus" & tree_BA > 0.7 ~ 0.7, 
                             genus == "Morus" & tree_BA > 1 ~ 1, 
                             TRUE ~ tree_BA)) %>% #restricting Ulmus BA to the range in which pollen production was measured
  mutate(per_tree_pollen_prod = case_when(
    spc_latin == "Acer negundo" ~ 253.71 * tree_BA + 0.38,
    spc_latin == "Acer platanoides" ~ 25.59 * tree_BA + 1.22,
    spc_latin == "Acer rubrum" ~ (62.32 * tree_BA + 1.27)*0.106, #.106 is the sex ratio
    spc_latin == "Acer saccharinum" ~ (exp(2.28 * tree_BA + 21.98))/1000000000, #convert to billions
    genus == "Betula"  ~ 561.16 * tree_BA + 5.03,
    genus == "Gleditsia"  ~ 659.91 * tree_BA -3.25,
    genus == "Juglans"  ~ 239.08 * tree_BA + 11.47,
    genus == "Morus"  ~ (6021.57 * tree_BA^2 + 1366.09 * tree_BA + 254.06)*0.578, #.58 adjusts for sex ratio
    genus == "Platanus"  ~ 1066.75 * tree_BA + 1.26,
    genus == "Populus"  ~ (exp(2.01 * tree_BA + 24.17) * 0.482)/1000000000, #convert to billions
    genus == "Quercus"  ~ 423.56 * tree_BA + 36.20, #red oaks and unknown oaks
    spc_latin == "Quercus palustris" ~ 327.2 * tree_BA + 14.9, #pin oaks
    genus == "Ulmus"  ~ (exp(5.86 * tree_BA + 23.11))/1000000000 #convert to billions
  )) %>% 
  filter(per_tree_pollen_prod > 0) %>% 
  filter(!is.na(per_tree_pollen_prod)) %>% 
  group_by(spc_latin, genus) %>% 
  summarize(total_p_bil = sum(per_tree_pollen_prod)) %>%  #adding each tree and also removing the billions
  mutate(total_p = total_p_bil * 1000000000,
         total_p_tril = total_p / 10^12,
         total_p_quad = total_p / 10^15)

citywide_pol_st %>% 
  group_by(genus) %>% 
  summarize(total_p_quad_gen = sum(total_p_quad)) %>% 
  ggplot(aes(x = reorder(genus, -total_p_quad_gen), y = total_p_quad_gen)) + geom_col()+ ggthemes::theme_few() + xlab("genus") +
  ylab("pollen produced (quadrillions)") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) #ylab(average~pollen~(grains/m^3))


#visualize both total city and street tree pollen
citywide_pol_join <- citywide_pol %>% 
  rename(genus = Genus) %>% 
  group_by(genus) %>% 
  summarize(total_p_quad_gen = sum(total_p_quad))

citywide_pol_st_join <- citywide_pol_st %>% 
  group_by(genus) %>% 
  summarize(street = sum(total_p_quad)) 

full_join(citywide_pol_join, citywide_pol_st_join) %>% 
  pivot_longer(cols = c(2,3), values_to = "pollen_quadrillion", names_to = "data_source") %>% 
  ggplot(aes(x = reorder(genus, -pollen_quadrillion), y = pollen_quadrillion, fill = data_source)) + geom_col(position = "dodge")+ 
  ggthemes::theme_few() + xlab("genus") +
  scale_fill_manual(name = "data source", labels = c("street trees", "city-wide"), values = c("gray24", "gray78")) + 
  ylab("pollen produced (quadrillions)") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) #ylab(average~pollen~(grains/m^3))

ggsave("pollen_prod_total_genus_st_it.jpeg", dpi = 300, width = 7, height = 5, units = "in")


### calculating tree growth and pollen production over time ###########################################################

#load in tree growth equations for available species

#create pollen production ~ tree age for taxa with pollen production eqns

#what if 200,000 trees were planted that either a) reflected current street tree comp or b) were only low pollen spp