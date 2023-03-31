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
st_trees <- read_csv( "C:/Users/dsk273/Box/NYC projects/tree data/2015 Street Tree Census - Tree Data/2015StreetTreesCensus_TREES.csv")
st_trees <- st_trees %>% 
  mutate(genus = sub(" .*", "", spc_latin),
         dbh_cm = 2.54 * tree_dbh,
         BA_m2 = 0.00007854 *  dbh_cm^2 ) %>% 
  filter(!is.na(genus) &
           status == "Alive") %>% 
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
it_trees <- read_csv( "C:/Users/dsk273/Box/NYC projects/tree data/2013_itree_plots/itree_2013_spp_summary.csv") 
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
#write_csv(it_trees, "C:/Users/dsk273/Box/NYC projects/tree data/2013_itree_plots/itree_2013_spp_summary_derived_vars.csv")

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
p <- read_csv("C:/Users/dsk273/Box/collaborations/NAB_NPN/NAB_pollen_220128c.csv") %>% 
  filter(Station.City == "New York City")
p_mean <- p %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% t() %>% as.data.frame() %>% 
  rownames_to_column(var = "genus") %>% 
  rename(pollen_mean = V1)

p_max <- p %>%
  summarise_if(is.numeric, max, na.rm = TRUE) %>% t() %>% as.data.frame() %>% 
  rownames_to_column(var = "genus") %>% 
  rename(pollen_max = V1)

p_mean_max <- left_join(p_mean, p_max)


### export table 1 ########################################################################
table1 <- left_join(it_st_genus, p_mean_max)

setdiff(it_st_genus$genus, p_mean_max$genus) %>% as.character() %>% sort()
setdiff(p_mean_max$genus, it_st_genus$genus) %>% as.character() %>% sort()

#write_csv(it_st_genus, "C:/Users/dsk273/Box/NYC projects/tree data/itree_st_trees_genus_summary.csv")
#write.table(table1, "clipboard", sep="\t", row.names=FALSE)
