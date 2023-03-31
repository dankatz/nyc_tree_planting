#set up work environment
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(readr)
library(stringr)



#rm(list=ls())

here::i_am("street_tree_summary.R") #using the 'here' package for consistent file structure system

### load in i tree data and create derived vars ####################################
it_trees <- read_csv( "C:/Users/dsk273/Box/NYC projects/tree data/2013_itree_plots/itree_2013_spp_summary.csv") 
it_trees <- it_trees %>% 
            mutate(prop_ba = basal_area / sum(basal_area),
                   prop_n = Trees / sum(Trees)) #check: sum(it_trees$prop_n)
# it_common_trees <- it_trees %>% 
#                     filter(prop_ba > 0.001 | prop_n > 0.001)

#test <- read_csv("C:/Users/dsk273/Box/NYC projects/tree data/2013_itree_plots/itree_2013_spp_summary.csv")


