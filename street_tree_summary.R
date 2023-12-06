#The effects of tree planting on allergenic pollen production in New York City
#Daniel Katz, Guy Robinson, Alexis Ellis, and David Nowak
#corresponding author: Daniel Katz, dankatz@cornell.edu
#published in Urban Forestry & Urban Greening
#DOI: 

#set up work environment
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(readr)
library(stringr)
library(tibble)
library(janitor) 

options(scipen=9)

#rm(list=ls())

here::i_am("street_tree_summary.R") #using the 'here' package for consistent file structure system

### load in street tree data and create derived variables ####################################
#Street tree data is available at: https://data.cityofnewyork.us/Environment/2015-Street-Tree-Census-Tree-Data/pi5s-9p35
st_trees <- read_csv( "C:/Users/dsk273/Box/NYC projects/tree data/2015 Street Tree Census - Tree Data/2015StreetTreesCensus_TREES.csv")
st_trees <- st_trees %>% 
  mutate(genus = sub(" .*", "", spc_latin),
         dbh_cm = 2.54 * tree_dbh, #convert DBH to cm from inches
         BA_m2 = 0.00007854 *  dbh_cm^2 ) %>% 
  filter(!is.na(genus) &
           status == "Alive") %>% 
  filter(dbh_cm < 170) %>% #there are a  few trees that have implausibly large DBHs that appear to be typos; I am removing those here
  mutate(spc_latin =  case_when(spc_latin == "Acer platanoides 'Crimson King'" ~ "Acer platanoides",
                                spc_latin == "Cedrus atlantica glauca" ~ "Cedrus atlantica",
                                spc_latin == "Gleditsia triacanthos var. inermis" ~ "Gleditsia triacanthos",
                                TRUE ~ spc_latin)) 


#export table with basal area summary at genus level
st_genus_summary <- st_trees %>%  group_by(genus) %>% 
  summarize(total_BA = sum(BA_m2, na.rm = TRUE),
            n_trees = n()) %>% 
  arrange(-total_BA) %>% 
  mutate(prop_ba = total_BA / sum(total_BA),
         prop_n = n_trees / sum(n_trees))
#write_csv(st_genus_summary, here("st_tree_genus_summary.csv"))


#SI2: export table with basal area summary at species level
st_spp_summary <- st_trees %>%  group_by(spc_latin) %>% 
  summarize(total_BA = sum(BA_m2, na.rm = TRUE),
            n_trees = n()) %>% 
  arrange(-total_BA) %>% 
  mutate(prop_ba = total_BA / sum(total_BA),
         prop_n = n_trees / sum(n_trees))

SI2_st_spp_summary <- st_spp_summary %>% 
  mutate(total_BA = round(total_BA, 2),
         total_BA = formatC(total_BA, format = "f", big.mark =",", digits = 2),
         n_trees = formatC(n_trees, format = "d", big.mark =","),
         perc_ba = round(prop_ba*100, 3),
         perc_n = round(prop_n * 100, 3)
         ) %>% 
  dplyr::select(-prop_ba, - prop_n)
#write_csv(SI2_st_spp_summary, here("st_tree_species_summary.csv"))



### add i-Tree Eco census data ######################################################
#i-Tree Eco project file for NYC was provided by Rob ('Bob') Hoehn on 6/8/2023 via email
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




### NYC airborne pollen data #########################################################
p <- read_csv("C:/Users/dsk273/Box/collaborations/NAB_NPN/NAB_pollen_220128c.csv") %>% 
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

p_mean_yr <- p %>%
  mutate(study_yr = lubridate::year(Date)) %>% 
  group_by(study_yr) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% t() %>% as.data.frame() %>% 
  rownames_to_column(var = "genus") %>% 
  rename(pollen_mean_2009 = V1, pollen_mean_2010 = V2, pollen_mean_2011 = V3, pollen_mean_2012 = V4) %>% 
  filter(genus != "study_yr") %>% 
  pivot_longer(cols = 2:5, names_to = "year", values_to = "annual_sum") %>% 
  group_by(genus) %>% 
  summarize(pollen_sum_mean = mean(annual_sum), #calculate the mean of each year's total pollen for each genus
            pollen_sum_sd = sd(annual_sum)) #calculate the inter-annual sd in pollen sum


#proportion of pollen collected by genus
p_mean_yr %>% 
  mutate(pollen_mean_prop = pollen_sum_mean/sum(p_mean_yr$pollen_sum_mean, na.rm = TRUE)) %>% 
  filter(pollen_mean_prop > 0.01) %>% 
  mutate(genus = case_when(genus == "Unidentified.Pollen" ~ "Unidentified pollen",
                           genus == "Gramineae...Poaceae" ~ "Poaceae",
                           TRUE ~ genus)) %>% 
ggplot(aes(x = reorder(genus, -pollen_sum_mean), y = pollen_sum_mean, ymax = pollen_sum_mean + pollen_sum_sd,
           ymin = pollen_sum_mean - pollen_sum_sd)) + 
   geom_col(fill = "gray50") + geom_errorbar(width = 0.2, col = "gray35") + ggthemes::theme_few() + xlab("taxon") + #ylab("pollen collected (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + ylab(annual~pollen~integral~(pollen~"×"~day/m^3)~~"") +
  coord_cartesian(ylim = c(700, 16000))
#ggsave("C:/Users/dsk273/Box/writing/tree planting and pollen in NYC/fig_4_pollen_percent_genus.jpeg", dpi = 300, width = 7, height = 4, units = "in")

p_prop <- p_sum %>% 
  mutate(pollen_prop = pollen_sum/sum(p_sum$pollen_sum)) %>% 
  #filter(pollen_prop > 0.01) %>% 
  arrange(-pollen_prop) %>% 
  dplyr::select(-pollen_sum)


p_prop_max <- left_join(p_prop, p_max)

#distinguishing between Picea and Pinus by going back to Guy's raw data
lincoln_raw_path <- "C:/Users/dsk273/Box/collaborations/NAB_NPN/NAB_unprocessed_data/Pollen_LincolnCenter_allyears_072913 (1)(1).xlsx"
lincoln_raw_2009 <- readxl::read_xlsx(path = lincoln_raw_path, sheet = "2009")
lincoln_raw_2010 <- readxl::read_xlsx(path = lincoln_raw_path, sheet = "2010")
lincoln_raw_2011 <- readxl::read_xlsx(path = lincoln_raw_path, sheet = "2011")
lincoln_raw_2012 <- readxl::read_xlsx(path = lincoln_raw_path, sheet = "2012")
lincoln_raw <- bind_rows(lincoln_raw_2009, lincoln_raw_2010, lincoln_raw_2011, lincoln_raw_2012)
sum(as.numeric(lincoln_raw$Picea), na.rm = T) / sum(p_sum$pollen_sum)
max(as.numeric(lincoln_raw$Picea), na.rm = T)
sum(as.numeric(lincoln_raw$Pinus), na.rm = T)/ sum(p_sum$pollen_sum)
max(as.numeric(lincoln_raw$Pinus), na.rm = T)

p_prop_max  <- p_prop_max %>% 
  filter(genus != "Pinaceae") %>% 
  add_row(genus = "Pinus", pollen_prop = sum(as.numeric(lincoln_raw$Pinus), na.rm = T)/ sum(p_sum$pollen_sum),
          pollen_max = max(as.numeric(lincoln_raw$Pinus), na.rm = T)) %>% 
  add_row(genus = "Picea", pollen_prop = sum(as.numeric(lincoln_raw$Picea), na.rm = T)/ sum(p_sum$pollen_sum),
          pollen_max = max(as.numeric(lincoln_raw$Picea), na.rm = T)) %>% 
  mutate(pollen_perc = round(pollen_prop * 100, 2),
         pollen_max = round(pollen_max, 1)) %>% 
  dplyr::select(genus, pollen_perc, pollen_max)
  
#are we potentially missing the pollen season for any taxa?
p %>%
  mutate(doy = lubridate::yday(date),
         e_year = lubridate::year(date)) %>%
  select(doy, e_year, Cupressaceae, Ulmus, Populus) %>% 
  pivot_longer(cols = c(Cupressaceae, Ulmus, Populus), names_to = "taxon", values_to = "pollen") %>% 
  ggplot(aes(x = doy, y = pollen)) + geom_point(alpha = 0.5) + theme_bw() + facet_grid(taxon~e_year, scales = "free_y")+ ylab(pollen ~(grains/m^3)) +
  scale_x_continuous(limits = c(0, 360)) + xlab("day of year")



### export table 1 ########################################################################
table1 <- left_join(it_st_genus, p_prop_max) 
#write_csv(it_st_genus, "C:/Users/danka/Box/NYC projects/tree data/itree_st_trees_genus_summary.csv")
#write.table(table1, "clipboard", sep="\t", row.names=FALSE)

setdiff(it_st_genus$genus, p_mean_max$genus) %>% as.character() %>% sort()
setdiff(p_mean_max$genus, it_st_genus$genus) %>% as.character() %>% sort()

table1_small <- table1 %>% 
  filter(pollen_perc > 1 | prop_ba_it > 1 | prop_ba_st > 1)

#write.table(table1_small, "clipboard", sep="\t", row.names=FALSE)



### calculate pollen production from each spp for i-Tree plots ##############################################
# original version used the publicly available summary info (now at the bottom of the script)

#actual i-Tree raw data
#load DBH table and convert to long format
it_dbh_genus <- read_csv("C:/Users/dsk273/Box/NYC projects/tree data/itree_2013_trees.csv") %>% 
  clean_names() %>% 
  mutate(ba_dbh1 =  0.00007854 * (dbh_1_in * 2.54)^2,  #convert dbh from inches to cm and then calculate basal area from dbh
         ba_dbh2 =  0.00007854 * (dbh_2_in * 2.54)^2,
         ba_dbh3 =  0.00007854 * (dbh_3_in * 2.54)^2,
         ba_dbh4 =  0.00007854 * (dbh_4_in * 2.54)^2,
         ba_dbh5 =  0.00007854 * (dbh_5_in * 2.54)^2,
         ba_dbh6 =  0.00007854 * (dbh_6_in * 2.54)^2) %>% 
  rowwise() %>% 
  mutate(tree_BA = sum(ba_dbh1, ba_dbh2, ba_dbh3, ba_dbh4, ba_dbh5, ba_dbh6, na.rm = TRUE)) %>% 
  mutate(sp = gsub("[\\(\\)]", "", regmatches(species, gregexpr("\\(.*?\\)", species))[[1]])) %>% 
  mutate(Genus = gsub( " .*$", "", sp ),
         Species = sub("^\\S+\\s+", '', sp),
         Species = case_when(Species == Genus ~ "sp.",
                             TRUE ~ Species)) %>% ungroup()
#it_dbh_genus$Species
#test <- filter(it_dbh_genus, Genus == "Ulmus")

#note: based on an email from Bob Hoehn, apparently the i-Tree data is stratified and weighted by county/borough 
#this means my city-wide estimates won't exactly match up with their summary data
#total area of NYC: 194,333.38 acres
#total areas surveyed in the 296 plots = 296*0.1
itree_to_nyc_scaling_factor <- (194333.38/(296 * 0.1))

#export Table SI 3: Summary of 2013 i-Tree data by species
table_SI3 <- it_dbh_genus %>% 
  select(Genus, Species, tree_BA) %>% 
  mutate(total_itree_BA = sum(tree_BA)) %>% 
  group_by(Genus, Species) %>% 
  summarize(n_trees_itree = n(),
            trees_percent = round(100*(n_trees_itree/1075), 2),
            total_trees_nyc = formatC(round(n_trees_itree * itree_to_nyc_scaling_factor, -3), format="d", big.mark=","),
            taxon_ba_itree = round(sum(tree_BA), 3),
            taxon_ba_rel_itree = round(100 * (taxon_ba_itree /50.49559), 3)) %>% #total BA measured in iTree plots = 50.49559
  arrange(-taxon_ba_itree) %>% ungroup() #%>% summarize(sum_ba = sum(taxon_rel_BA_itree))
#write_csv(table_SI3, "table_SI3_230721.csv")


#select only genera with pollen production equations
pollen_prod_focal_genera <- c("Acer", "Betula", "Platanus", "Quercus", "Morus", "Populus", "Gleditsia", "Juglans", "Ulmus")
it_dbh_genus_n <- it_dbh_genus %>% filter(Genus %in% pollen_prod_focal_genera)


#calculate pollen production for each individual, now including SDs
# unique(it_dbh_genus$sp)
for(i in 1:1000){
  Acne_param_a <- rnorm(n = 1, mean = 253.71, sd = 47.75)
  Acne_param_b <- rnorm(n = 1, mean = 0.38, sd = 3.26)
  Acpl_param_a <- rnorm(n = 1, mean = 25.59, sd = 7.00)
  Acpl_param_b <- rnorm(n = 1, mean = 1.22, sd = 0.46)
  Acru_param_a <- rnorm(n = 1, mean = 62.32, sd = 13.50)
  Acru_param_b <- rnorm(n = 1, mean = 1.27, sd = 0.44)
  Acsa_param_a <- rnorm(n = 1, mean = 2.28, sd =0.49)
  Acsa_param_b <- rnorm(n = 1, mean = 21.98, sd =0.28)
  Bepa_param_a <- rnorm(n = 1, mean = 561.16, sd = 228.86)
  Bepa_param_b <- rnorm(n = 1, mean = 5.03, sd =4.42)
  Gltr_param_a <- rnorm(n = 1, mean = 659.91, sd =103.36)
  Gltr_param_b <- rnorm(n = 1, mean = -3.25, sd = 1.97)
  Juni_param_a <- rnorm(n = 1, mean = 239.08, sd = 64.85)
  Juni_param_b <- rnorm(n = 1, mean = 11.47, sd = 8.22)
  Mosp_param_a <- rnorm(n = 1, mean = -67.95, sd = 1366.09)
  Mosp_param_b <- rnorm(n = 1, mean = 254.06, sd = 93.26)*0.578
  Mosp_param_c <- rnorm(n = 1, mean = 6021.57, sd =2011.79)
  Plac_param_a <- rnorm(n = 1, mean = 1066.75, sd = 251.73)
  Plac_param_b <- rnorm(n = 1, mean = 1.26, sd = 8.15)
  Posp_param_a <- rnorm(n = 1, mean = 2.01, sd = 0.24)
  Posp_param_b <- rnorm(n = 1, mean = 24.17, sd = 0.19)
  Qusp_param_a <- rnorm(n = 1, mean = 423.56, sd = 85.45)
  Qusp_param_b <- rnorm(n = 1, mean = 36.20, sd = 11.42)
  Qupa_param_a <- rnorm(n = 1, mean = 327.2, sd =100.94)
  Qupa_param_b <- rnorm(n = 1, mean = 14.9, sd = 7.41)
  Ulsp_param_a <- rnorm(n = 1, mean = 546.56, sd = 89.86) #rnorm(n = 1, mean = 5.86, sd = 0.35)
  Ulsp_param_b <- rnorm(n = 1, mean = 23.76, sd = 17.06) #rnorm(n = 1, mean = 23.11, sd = 0.15)
  
it_dbh_genus_np_i <-  #
  it_dbh_genus_n %>%  
  mutate(per_tree_pollen_prod = case_when(
    Genus == "Acer" & Species == "negundo"  ~ ( Acne_param_a * tree_BA + Acne_param_b) *0.558, #.558 is the sex ratio,
    Genus == "Acer" & Species == "platanoides"  ~ Acpl_param_a * tree_BA + Acpl_param_b,
    Genus == "Acer" & Species == "rubrum"  ~ ( Acru_param_a * tree_BA + Acru_param_b) * 0.106, #.106 is the sex ratio
    Genus == "Acer" & Species == "saccharinum"~ (exp( Acsa_param_a * tree_BA + Acsa_param_b))/1000000000, #convert to billions
    Genus == "Betula"  ~ Bepa_param_a* tree_BA + Bepa_param_b,
    Genus == "Gleditsia"  ~ Gltr_param_a * tree_BA + Gltr_param_b,
    Genus == "Juglans"  ~ Juni_param_a * tree_BA + Juni_param_b,
    Genus == "Morus"  ~ (Mosp_param_c * tree_BA^2 + Mosp_param_a * tree_BA + Mosp_param_b) *0.578, #.58 adjusts for sex ratio
    Genus == "Platanus"  ~ Plac_param_a * tree_BA + Plac_param_b,
    Genus == "Populus"  ~ (exp( Posp_param_a * tree_BA + Posp_param_b) * 0.482)/1000000000, #convert to billions
    Genus == "Quercus"  ~ Qusp_param_a * tree_BA + Qusp_param_b, #red oaks and unknown oaks
    Genus == "Quercus" & Species == "palustris"  ~ Qupa_param_a * tree_BA + Qupa_param_b, #pin oaks
    Genus == "Ulmus"  ~ ( Ulsp_param_a * tree_BA + Ulsp_param_b) 
    ),
    iter = i ) #did a gut check against fig 3 in Katz et al. 2020; all of these currently line up


ifelse(i == 1,
       it_dbh_genus_np_all <- it_dbh_genus_np_i,
       it_dbh_genus_np_all <- bind_rows(it_dbh_genus_np_all, it_dbh_genus_np_i))
print(i)
}

#calculate total pollen production for each taxon
citywide_pol <- 
  it_dbh_genus_np_all %>% 
  mutate(p_all_trees = per_tree_pollen_prod,
         genus_species = paste(Genus, Species, sep = " ")) %>% 
  group_by(iter, Genus) %>% 
  summarize(total_p_bil = sum(p_all_trees) * itree_to_nyc_scaling_factor) %>%  #adding each tree and also removing the billions
    #also adding in a multiplication term to scale from the plots to the city; see above
  filter(!is.na(total_p_bil)) %>% 
  mutate(total_p = total_p_bil * 1000000000,
         total_p_tril = total_p / 10^12,
         total_p_quad = total_p / 10^15) %>% 
  group_by(Genus) %>% 
  summarize(total_p_quad_mean = mean(total_p_quad),
            total_p_quad_sd = sd(total_p_quad))


# citywide_pol %>% 
#   ggplot(aes(x = reorder(Genus, -total_p_quad_mean), y = total_p_quad_mean, ymax = total_p_quad_mean + total_p_quad_sd, ymin = total_p_quad_mean)) + 
#   geom_col()+ geom_errorbar(width = 0.2) + ggthemes::theme_few() + xlab("genus") +
#   ylab("pollen produced (quadrillions)") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1)) #ylab(average~pollen~(grains/m^3))
#ggsave("pollen_prod_total_genus.jpeg", dpi = 300, width = 7, height = 4, units = "in")


#histogram of iTree DBH
names(it_dbh_genus_np_all)
it_dbh_genus_np_all %>% 
  mutate(dbh_tree_total = sqrt(tree_BA/0.00007854)) %>% 
  filter(!is.na(per_tree_pollen_prod)) %>% 
  ggplot(aes(x = dbh_tree_total)) + geom_histogram() + facet_wrap(~Genus, scales = "free_y") + 
  xlab("DBH (cm)")+
  ggthemes::theme_few() + theme(strip.text = element_text(face = "italic"))

  
  
#calculate total pollen production for each genus by whether planted or unplanted
it_dbh_genus_np_all %>% 
  filter(!is.na(per_tree_pollen_prod)) %>% 
  mutate(Species = case_when(Genus == "Betula" & Species != "papyrifera" ~ "sp.", 
                             Genus == "Quercus" & Species != "rubra" & Species != "palustris" ~ "sp.", 
                             Genus == "Ulmus" & Species != "americana" ~ "sp.", 
                             TRUE ~ Species)) %>% 
  group_by(iter, Genus, status) %>% 
  summarize(total_p_bil = sum(per_tree_pollen_prod) * itree_to_nyc_scaling_factor) %>%  #adding each tree and also removing the billions
  mutate(total_p = total_p_bil * 1000000000,
         total_p_tril = total_p / 10^12,
         total_p_quad = total_p / 10^15) %>% 
  group_by(Genus, status) %>% 
  summarize(total_p_quad_mean = mean(total_p_quad),
            total_p_quad_sd = sd(total_p_quad)) %>% 
  mutate(status = case_when(status == "Ingrowth" ~ "Unplanted",
                            TRUE ~ status),
         status = factor(status, ordered = TRUE, levels = c("Unplanted","Planted","Unknown"))) %>% 
  ggplot(aes(x = reorder(Genus, -total_p_quad_mean), y  = total_p_quad_mean, ymax = total_p_quad_mean + total_p_quad_sd, 
             ymin = total_p_quad_mean, 
             fill = status, color = status)) + 
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) + 
  geom_errorbar(aes(x = reorder(Genus, -total_p_quad_mean), y  = total_p_quad_mean, ymax = total_p_quad_mean, #adding in negative errors
                        ymin = total_p_quad_mean - total_p_quad_sd), 
                position = position_dodge2(preserve = "single", width = 0.2, padding = 0.5), color = "white", alpha = 0.3)+
  geom_errorbar(position = position_dodge2(preserve = "single", width = 0.2, padding = 0.5)) + #positive error bars
  ggthemes::theme_few() + xlab("genus") +
  ylab(pollen~production~(pollen~grains/year)~" \u00D7"~10^15) +
  scale_color_manual(name = "", values = c("gray0","goldenrod",  "gray70")) +
  scale_fill_manual(name = "", values = c("gray0","goldenrod",  "gray70")) + theme(axis.text.x = element_text(face = "italic"))
#ggsave("C:/Users/dsk273/Box/writing/tree planting and pollen in NYC/fig2.jpeg", dpi = 300, width = 7, height = 7, units = "in" )


#calculate total pollen production for each genus by i-Tree land use
it_dbh_genus_np_all %>% 
  filter(!is.na(per_tree_pollen_prod)) %>% 
  mutate(Species = case_when(Genus == "Betula" & Species != "papyrifera" ~ "sp.", 
                             Genus == "Quercus" & Species != "rubra" & Species != "palustris" ~ "sp.", 
                             Genus == "Ulmus" & Species != "americana" ~ "sp.", 
                             TRUE ~ Species)) %>% 
  mutate(p_all_trees = per_tree_pollen_prod,
         genus_species = paste(Genus, Species, sep = " ")) %>% 
  group_by(Genus, land_use) %>% 
  summarize(total_p_bil = sum(p_all_trees) ,
            n = n())  %>% #adding each tree 
  ggplot(aes(x = reorder(Genus, -total_p_bil), y  = total_p_bil, fill = land_use)) + geom_bar(stat = "identity") + ggthemes::theme_few() + xlab("genus") +
  ylab("pollen produced (billions)")  + theme(axis.text.x = element_text(face = "italic")) + scale_fill_discrete(name = "land use")
#ggsave("C:/Users/dsk273/Box/writing/tree planting and pollen in NYC/SI4_itree_landuse.jpeg", dpi = 300, width = 8, height = 6, units = "in" )




###  calculate pollen production for all street trees ##############################################

#creating a version that perpetuates the variance from the pollen production equations
for(i in 1:1000){

  Acne_param_a <- rnorm(n = 1, mean = 253.71, sd = 47.75)
  Acne_param_b <- rnorm(n = 1, mean = 0.38, sd = 3.26)
  Acpl_param_a <- rnorm(n = 1, mean = 25.59, sd = 7.00)
  Acpl_param_b <- rnorm(n = 1, mean = 1.22, sd = 0.46)
  Acru_param_a <- rnorm(n = 1, mean = 62.32, sd = 13.50)
  Acru_param_b <- rnorm(n = 1, mean = 1.27, sd = 0.44)
  Acsa_param_a <- rnorm(n = 1, mean = 2.28, sd =0.49)
  Acsa_param_b <- rnorm(n = 1, mean =21.98, sd =0.28)
  Bepa_param_a <- rnorm(n = 1, mean = 561.16, sd = 228.86)
  Bepa_param_b <- rnorm(n = 1, mean = 5.03, sd =4.42)
  Gltr_param_a <- rnorm(n=1, mean = 659.91, sd =103.36)
  Gltr_param_b <- rnorm(n = 1, mean = -3.25, sd = 1.97)
  Juni_param_a <- rnorm(n = 1, mean = 239.08, sd = 64.85)
  Juni_param_b <- rnorm(n = 1, mean = 11.47, sd = 8.22)
  Mosp_param_a <- rnorm(n = 1, mean = -67.95, sd = 1366.09)
  Mosp_param_b <- rnorm(n = 1, mean = 254.06, sd = 93.26)
  Mosp_param_c <- rnorm(n = 1, mean = 6021.57, sd =2011.79)
  Plac_param_a <- rnorm(n = 1, mean = 1066.75, sd = 251.73)
  Plac_param_b <- rnorm(n = 1, mean = 1.26, sd = 8.15)
  Posp_param_a <- rnorm(n = 1, mean = 2.01, sd = 0.24)
  Posp_param_b <- rnorm(n = 1, mean = 24.17, sd = 0.19)
  Qusp_param_a <- rnorm(n = 1, mean = 423.56, sd = 85.45)
  Qusp_param_b <- rnorm(n = 1, mean = 36.20, sd = 11.42)
  Qupa_param_a <- rnorm(n = 1, mean = 327.2, sd =100.94)
  Qupa_param_b <- rnorm(n = 1, mean = 14.9, sd = 7.41)
  Ulsp_param_a <- rnorm(n = 1, mean = 546.56, sd = 89.86) #rnorm(n = 1, mean = 5.86, sd = 0.35)
  Ulsp_param_b <- rnorm(n = 1, mean = 23.76, sd = 17.06) #rnorm(n = 1, mean = 23.11, sd = 0.15)
  
  
citywide_pol_st_i <- st_trees %>% 
  select(spc_latin, genus, BA_m2, dbh_cm) %>% 
  mutate(tree_BA = BA_m2) %>% 
  # mutate(tree_BA = case_when(genus == "Ulmus" & tree_BA > 0.7 ~ 0.7, 
  #                            genus == "Morus" & tree_BA > 1 ~ 1, 
  #                            TRUE ~ tree_BA)) %>% #restricting Ulmus BA to the range in which pollen production was measured
  mutate(per_tree_pollen_prod = case_when(
    spc_latin == "Acer negundo" ~ ( Acne_param_a * tree_BA + Acne_param_b) *0.558, #.558 is the sex ratio,
    spc_latin == "Acer platanoides" ~ Acpl_param_a * tree_BA + Acpl_param_b,
    spc_latin == "Acer rubrum" ~ ( Acru_param_a * tree_BA + Acru_param_b) * 0.106, #.106 is the sex ratio
    spc_latin == "Acer saccharinum" ~ (exp( Acsa_param_a * tree_BA + Acsa_param_b))/1000000000, #convert to billions
    genus == "Betula"  ~ Bepa_param_a* tree_BA + Bepa_param_b,
    genus == "Gleditsia"  ~ Gltr_param_a * tree_BA + Gltr_param_b,
    genus == "Juglans"  ~ Juni_param_a * tree_BA + Juni_param_b,
    genus == "Morus"  ~ (Mosp_param_c * tree_BA^2 + Mosp_param_a * tree_BA + Mosp_param_b) *0.578, #.58 adjusts for sex ratio
    genus == "Platanus"  ~ Plac_param_a * tree_BA + Plac_param_b,
    genus == "Populus"  ~ (exp( Posp_param_a * tree_BA + Posp_param_b) * 0.482)/1000000000, #convert to billions
    genus == "Quercus"  ~ Qusp_param_a * tree_BA + Qusp_param_b, #red oaks and unknown oaks
    spc_latin == "Quercus palustris" ~ Qupa_param_a * tree_BA + Qupa_param_b, #pin oaks
    genus == "Ulmus"  ~ (Ulsp_param_a * tree_BA + Ulsp_param_b) #convert to billions
  )) %>% 
  filter(per_tree_pollen_prod > 0) %>% 
  filter(!is.na(per_tree_pollen_prod)) %>% 
  group_by(spc_latin, genus) %>% 
  summarize(total_p_bil = sum(per_tree_pollen_prod)) %>%  #adding each tree and also removing the billions
  mutate(total_p = total_p_bil * 1000000000,
         total_p_tril = total_p / 10^12,
         total_p_quad = total_p / 10^15,
         iter = i)

ifelse(i == 1, 
       citywide_pol_st_i_all <- citywide_pol_st_i,
       citywide_pol_st_i_all <- bind_rows(citywide_pol_st_i_all, citywide_pol_st_i))

print(i)
}


citywide_pol_st_pre_join <- citywide_pol_st_i_all %>% 
  group_by(iter, genus) %>% 
  summarize(total_p_quad_gen = sum(total_p_quad)) %>% 
  group_by(genus) %>% 
  summarize(total_p_quad_gen_mean = mean(total_p_quad_gen),
            total_p_quad_gen_sd = sd(total_p_quad_gen)) 

citywide_pol_st_pre_join %>% 
  ggplot(aes(x = reorder(genus, -total_p_quad_gen_mean), y = total_p_quad_gen_mean, ymax = total_p_quad_gen_mean + total_p_quad_gen_sd,
             ymin = total_p_quad_gen_mean )) + 
  geom_col()+ geom_errorbar(width = 0.2)+ggthemes::theme_few() + xlab("genus") +
  ylab("pollen produced (quadrillions)") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) #ylab(average~pollen~(grains/m^3))

citywide_pol_st_join <- citywide_pol_st_pre_join %>% 
  rename(pollen_quadrillion_mean = total_p_quad_gen_mean,
         pollen_quadrillion_sd = total_p_quad_gen_sd) %>% 
  mutate(data_source = "street trees")

#visualize both total city and street tree pollen
citywide_pol_join <- citywide_pol  %>% 
   rename(genus = Genus,
          pollen_quadrillion_mean = total_p_quad_mean,
          pollen_quadrillion_sd = total_p_quad_sd) %>% 
  mutate( data_source = "city-wide")
  # group_by(genus) %>% 
  # summarize(total_p_quad_gen = sum(total_p_quad))

bind_rows(citywide_pol_st_join, citywide_pol_join)%>% 
  ggplot(aes(x = reorder(genus, -pollen_quadrillion_mean ), y = pollen_quadrillion_mean, ymax = pollen_quadrillion_mean + pollen_quadrillion_sd, 
             ymin = pollen_quadrillion_mean , fill = data_source, color = data_source)) + 
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) + 
  geom_errorbar(aes(x = reorder(genus, -pollen_quadrillion_mean ), y = pollen_quadrillion_mean, #negative error bar
                    ymax = pollen_quadrillion_mean + pollen_quadrillion_sd, 
                    ymin = pollen_quadrillion_mean - pollen_quadrillion_sd, color = data_source), 
                position = position_dodge2(preserve = "single", width = 0.2, padding = 0.5), color = "white", alpha = 0.3)+
  geom_errorbar(position = position_dodge2(preserve = "single", width = 0.2, padding = 0.5)) + #positive error bar
  ggthemes::theme_few() + xlab("genus") +
  scale_fill_manual(name = "data source", labels = c("city-wide", "street trees"), values = c("gray24", "gray78")) + 
  scale_color_manual(name = "data source", labels = c("city-wide", "street trees"), values = c("gray24", "gray78")) + 
  ylab(pollen~production~(pollen~grains/year)~" \u00D7"~10^15) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) #ylab(average~pollen~(grains/m^3))
#ggsave("C:/Users/dsk273/Box/writing/tree planting and pollen in NYC/fig1_pollen_prod_total_genus_st_it.jpeg", dpi = 300, width = 7, height = 5, units = "in")


### calculating future pollen production: i-Tree trees growth and mortality ###########################################################
#ran a forecast in i-Tree eco forecast module using standard parameter values
library("DBI")
sim_filepath <- "C:/Users/dsk273/Box/NYC projects/tree data/itree_2013_230629_simulation_planting200k_eachyear.db"
mydb <- dbConnect(RSQLite::SQLite(), sim_filepath)
#dbListTables(mydb)
it_forecast <- dbReadTable(mydb, "EcoForecastCohorts")
#names(it_forecast)
dbDisconnect(mydb)

sum(it_forecast$NumTrees[it_forecast$ForecastedYear == 50]) 

it_forecast_c <- it_forecast %>% 
  rowwise() %>% 
  mutate(cohortkey2 =  paste(as.character(unlist(CohortKey)), collapse = ''),
         parentkey2 =  paste(as.character(unlist(ParentKey)), collapse = ''))

###looping through to provide a unique key for each tree (the original key system is based on paired identifiers in the cohort key and parentkey columns)
#last year set up

it_forecast_c2 <- it_forecast_c %>% 
  mutate(tree_key = case_when(ForecastedYear == 50 ~ cohortkey2))

### going from the last forecasted year to the first
for(i in 50:1){ #takes an hour to run
  year_end <- it_forecast_c2 %>% 
    filter(ForecastedYear == i) %>% 
    select(tree_key_temp = tree_key, cohortkey2 = parentkey2)
  
  it_forecast_c2 <- left_join(it_forecast_c2, year_end) %>% 
    mutate(tree_key = case_when( is.na(tree_key) & !is.na(tree_key_temp) ~ tree_key_temp,
                                 TRUE ~ tree_key)) %>% 
    select(-tree_key_temp)
}

#just looking at a single taxon for easy checking to make sure the key value system worked
test <- it_forecast_c2 %>% 
  filter(SppCode == "LOJA") %>%  
  arrange(CrownLightExposure, ForecastedYear)

#denote which trees are part of the tree planting scenario
planted_trees_lookuptable <- it_forecast_c2 %>% 
  filter(ForecastedYear > 0 & DBH == 2) %>% 
  select(tree_key) %>% 
  distinct() %>% 
  mutate(origin = "new")
it_forecast_c2 <- left_join(it_forecast_c2, planted_trees_lookuptable) #it_forecast_c2

#convert species codes to binomials
it_sp_list <- read_csv("C:/Users/dsk273/Box/NYC projects/tree data/2013_itree_plots/itree_SpeciesList.csv", name_repair = "universal") %>% 
  mutate(scientificname = stringr::str_replace_all(Scientific.Name, "[^[:alnum:]///' ]", "")) %>% #remove strange characters
  select(-Scientific.Name) %>% 
  #mutate(sp = regmatches(scientificname, gregexpr("\\(.*?\\)", scientificname))) %>% 
  #mutate(sp = gsub("[\\(\\)]", "", regmatches(Scientific.Name, gregexpr("\\(.*?\\)", Scientific.Name))[[1]])) %>% 
  mutate(Genus = gsub( " .*$", "", scientificname ),
         Species = sub("^\\S+\\s+", '', scientificname),
         Species = case_when(Species == Genus ~ "sp.",
                             TRUE ~ Species)) %>% ungroup()

#select only genera with pollen production equations
pollen_prod_focal_genera <- c("Acer", "Betula", "Platanus", "Quercus", "Morus", "Populus", "Gleditsia", "Juglans", "Ulmus")
it_forecast2 <- left_join(it_forecast_c2, it_sp_list) %>% 
    filter(Genus %in% pollen_prod_focal_genera)


Sys.time()
for(i in 1:1000){

#drawing from the parameter distributions for each taxon
Acne_param_a <- rnorm(n = 1, mean = 253.71, sd = 47.75)
Acne_param_b <- rnorm(n = 1, mean = 0.38, sd = 3.26)
Acpl_param_a <- rnorm(n = 1, mean = 25.59, sd = 7.00)
Acpl_param_b <- rnorm(n = 1, mean = 1.22, sd = 0.46)
Acru_param_a <- rnorm(n = 1, mean = 62.32, sd = 13.50)
Acru_param_b <- rnorm(n = 1, mean = 1.27, sd = 0.44)
Acsa_param_a <- rnorm(n = 1, mean = 2.28, sd =0.49)
Acsa_param_b <- rnorm(n = 1, mean =21.98, sd =0.28)
Bepa_param_a <- rnorm(n = 1, mean = 561.16, sd = 228.86)
Bepa_param_b <- rnorm(n = 1, mean = 5.03, sd =4.42)
Gltr_param_a <- rnorm(n = 1, mean = 659.91, sd =103.36)
Gltr_param_b <- rnorm(n = 1, mean = -3.25, sd = 1.97)
Juni_param_a <- rnorm(n = 1, mean = 239.08, sd = 64.85)
Juni_param_b <- rnorm(n = 1, mean = 11.47, sd = 8.22)
Mosp_param_a <- rnorm(n = 1, mean = -67.95, sd = 1366.09)
Mosp_param_b <- rnorm(n = 1, mean = 254.06, sd = 93.26)*0.578
Mosp_param_c <- rnorm(n = 1, mean = 6021.57, sd =2011.79)
Plac_param_a <- rnorm(n = 1, mean = 1066.75, sd = 251.73)
Plac_param_b <- rnorm(n = 1, mean = 1.26, sd = 8.15)
Posp_param_a <- rnorm(n = 1, mean = 2.01, sd = 0.24)
Posp_param_b <- rnorm(n = 1, mean = 24.17, sd = 0.19)
Qusp_param_a <- rnorm(n = 1, mean = 423.56, sd = 85.45)
Qusp_param_b <- rnorm(n = 1, mean = 36.20, sd = 11.42)
Qupa_param_a <- rnorm(n = 1, mean = 327.2, sd =100.94)
Qupa_param_b <- rnorm(n = 1, mean = 14.9, sd = 7.41)
Ulsp_param_a <- rnorm(n = 1, mean = 546.56, sd = 89.86) #rnorm(n = 1, mean = 5.86, sd = 0.35)
Ulsp_param_b <- rnorm(n = 1, mean = 23.76, sd = 17.06) #rnorm(n = 1, mean = 23.11, sd = 0.15)

  
it_forecast3_i <- it_forecast2 %>%  
  dplyr::select(ForecastedYear, Genus, Species, DBH, NumTrees, origin, tree_key) %>% 
  mutate(tree_BA = 0.00007854 * (DBH * 2.54)^2) %>% #covert DBH from inches to cm and then to basal area
  mutate(per_tree_pollen_prod = case_when(
      Genus == "Acer" & Species == "negundo"  ~ ( Acne_param_a * tree_BA + Acne_param_b) *0.558, #.558 is the sex ratio
      Genus == "Acer" & Species == "platanoides"  ~ Acpl_param_a * tree_BA + Acpl_param_b,
      Genus == "Acer" & Species == "rubrum" ~ (Acru_param_a * tree_BA + Acru_param_b) * 0.106, #.106 is the sex ratio
      Genus == "Acer" & Species == "saccharinum" ~ (exp( Acsa_param_a * tree_BA + Acsa_param_b))/1000000000, #convert to billions
      Genus == "Betula"  ~ Bepa_param_a * tree_BA + Bepa_param_b,
      Genus == "Gleditsia" ~ Gltr_param_a * tree_BA + Gltr_param_b,
      Genus == "Juglans"  ~  Juni_param_a * tree_BA + Juni_param_b,
      Genus == "Morus"  ~  (Mosp_param_c* tree_BA^2 +  Mosp_param_a* tree_BA + Mosp_param_b)*0.58, #.58 adjusts for sex ratio
      Genus == "Platanus"  ~ Plac_param_a * tree_BA + Plac_param_b,
      Genus == "Populus"  ~ (exp( Posp_param_a * tree_BA + Posp_param_b) * 0.482)/1000000000, #convert to billions
      Genus == "Quercus"  ~ Qusp_param_a * tree_BA + Qusp_param_b, #red oaks and unknown oaks
      Genus == "Quercus" & Species == "palustris" ~ Qupa_param_a * tree_BA + Qupa_param_b, #pin oaks
      Genus == "Ulmus"  ~ Ulsp_param_a * tree_BA + Ulsp_param_b 
    ),
    iter = i)  %>% 
  filter(!is.na(per_tree_pollen_prod)) #remove non-focal Acer spp

ifelse(i == 1, 
       it_forecast3_all <- it_forecast3_i,
       it_forecast3_all <- bind_rows(it_forecast3_all, it_forecast3_i))
print(i)
}
Sys.time()

#calculate total pollen production for each taxon
it_forecast_citywide  <- it_forecast3_all %>% 
  mutate(cohort_tree_pollen_prod =per_tree_pollen_prod * NumTrees) %>% #scaling each tree by estimated number per city 
  group_by(Genus, ForecastedYear, origin, iter) %>% 
  summarize(total_p_bil = sum(cohort_tree_pollen_prod),
            n_trees = sum(NumTrees),
            ba_mean = mean(tree_BA )) %>% 
  #filter(!is.na(total_p_bil)) %>% 
  mutate(total_p = total_p_bil * 1000000000,
         total_p_tril = total_p / 10^12,
         total_p_quad = total_p / 10^15) %>% 
  group_by(Genus, ForecastedYear, origin) %>% 
  summarize(total_p_quad_mean = mean(total_p_quad),
            total_p_quad_sd = sd(total_p_quad)) %>% 
  mutate(origin = case_when(is.na(origin)~ "original", TRUE ~ origin)) 


#figure 2: pollen production over time for each species by existing trees vs new trees assuming approximately equal replacement
it_forecast_citywide %>% 
  ggplot(aes(x = ForecastedYear, y = total_p_quad_mean, color = origin, fill = origin)) + 
  geom_ribbon(aes(x = ForecastedYear, 
                  ymin = total_p_quad_mean - total_p_quad_sd, ymax =  total_p_quad_mean + total_p_quad_sd, fill = origin, 
                  alpha = 0.9), color = NA) + scale_alpha(guide = 'none')+
  ggthemes::theme_few() + xlab("year") + theme(legend.position = c(0.8, 0.2), strip.text = element_text(face = "italic")) + 
  geom_line(color = "black")+ 
  ylab(pollen~production~(pollen~grains/year)~" \u00D7"~10^15)  + facet_wrap(~Genus, scales = "free_y") + 
  scale_fill_manual(name = "tree origin", values = c("gold","gray40")) +
  scale_color_manual(name = "tree origin", values = c("gold","gray40"))
#ggsave("C:/Users/dsk273/Box/writing/tree planting and pollen in NYC/fig4_pollen_prod_time_treeorigin_genus.jpeg", dpi = 300, width = 6, height = 5, units = "in")


### Fig SI 6 ##########################################################
# this figure requires it_forecast3_i (only one iteration required)

#pollen production as a function of age for a tree cohort from each species, also providing survival
#specific trees to use
species_to_use <- c("Acer negundo", "Acer platanoides", "Acer rubrum", "Acer saccharinum", "Betula papyrifera", "Gleditsia triacanthos",
                    "Juglans","Morus alba", "Platanus x hybrida", "Quercus rubra", "Quercus palustris", "Ulmus americana")
individuals_to_use_in_fig <- it_forecast3_i %>% #dplyr::sample_n(10000) %>% 
  mutate(scientificname = paste(Genus, Species, sep = " ")) %>% 
  filter(scientificname %in% species_to_use) %>% 
  filter(ForecastedYear == 1 & DBH == 2) %>% #select a unique tree cohort from each taxon
  group_by(scientificname) %>% arrange(-NumTrees) %>%  slice(1) %>% ungroup() %>% 
  select(tree_key, initial_num_trees = NumTrees) %>% mutate(selected_indiv = "for_fig")


pol_age_to_use <- left_join(it_forecast3_i, individuals_to_use_in_fig) %>% 
  filter(selected_indiv == "for_fig") %>% 
  mutate(scientificname = paste(Genus, Species, sep = " ")) %>% 
  mutate(dbh_cm = 2.54 * DBH) %>%  
  mutate(tree_BA = 0.00007854 * (DBH * 2.54)^2) #covert DBH from inches to cm and then to basal area
  
#draw from distributions  
for(i in 189:1000){
  #drawing from the parameter distributions for each taxon
  Acne_param_a <- rnorm(n = 1, mean = 253.71, sd = 47.75)
  Acne_param_b <- rnorm(n = 1, mean = 0.38, sd = 3.26)
  Acpl_param_a <- rnorm(n = 1, mean = 25.59, sd = 7.00)
  Acpl_param_b <- rnorm(n = 1, mean = 1.22, sd = 0.46)
  Acru_param_a <- rnorm(n = 1, mean = 62.32, sd = 13.50)
  Acru_param_b <- rnorm(n = 1, mean = 1.27, sd = 0.44)
  Acsa_param_a <- rnorm(n = 1, mean = 2.28, sd =0.49)
  Acsa_param_b <- rnorm(n = 1, mean = 21.98, sd =0.28)
  Bepa_param_a <- rnorm(n = 1, mean = 561.16, sd = 228.86)
  Bepa_param_b <- rnorm(n = 1, mean = 5.03, sd =4.42)
  Gltr_param_a <- rnorm(n = 1, mean = 659.91, sd =103.36)
  Gltr_param_b <- rnorm(n = 1, mean = -3.25, sd = 1.97)
  Juni_param_a <- rnorm(n = 1, mean = 239.08, sd = 64.85)
  Juni_param_b <- rnorm(n = 1, mean = 11.47, sd = 8.22)
  Mosp_param_a <- rnorm(n = 1, mean = -67.95, sd = 1366.09)
  Mosp_param_b <- rnorm(n = 1, mean = 254.06, sd = 93.26)*0.578
  Mosp_param_c <- rnorm(n = 1, mean = 6021.57, sd =2011.79)
  Plac_param_a <- rnorm(n = 1, mean = 1066.75, sd = 251.73)
  Plac_param_b <- rnorm(n = 1, mean = 1.26, sd = 8.15)
  Posp_param_a <- rnorm(n = 1, mean = 2.01, sd = 0.24)
  Posp_param_b <- rnorm(n = 1, mean = 24.17, sd = 0.19)
  Qusp_param_a <- rnorm(n = 1, mean = 423.56, sd = 85.45)
  Qusp_param_b <- rnorm(n = 1, mean = 36.20, sd = 11.42)
  Qupa_param_a <- rnorm(n = 1, mean = 327.2, sd =100.94)
  Qupa_param_b <- rnorm(n = 1, mean = 14.9, sd = 7.41)
  Ulsp_param_a <- rnorm(n = 1, mean = 546.56, sd = 89.86) #rnorm(n = 1, mean = 5.86, sd = 0.35)
  Ulsp_param_b <- rnorm(n = 1, mean = 23.76, sd = 17.06) #rnorm(n = 1, mean = 23.11, sd = 0.15)
  
  pol_age_i <- pol_age_to_use %>% 
    mutate(per_tree_pollen_prod = case_when(
      Genus == "Acer" & Species == "negundo"  ~ ( Acne_param_a * tree_BA + Acne_param_b) *0.558, #.558 is the sex ratio
      Genus == "Acer" & Species == "platanoides"  ~ Acpl_param_a * tree_BA + Acpl_param_b,
      Genus == "Acer" & Species == "rubrum" ~ (Acru_param_a * tree_BA + Acru_param_b) * 0.106, #.106 is the sex ratio
      Genus == "Acer" & Species == "saccharinum" ~ (exp( Acsa_param_a * tree_BA + Acsa_param_b))/1000000000, #convert to billions
      Genus == "Betula"  ~ Bepa_param_a * tree_BA + Bepa_param_b,
      Genus == "Gleditsia" ~ Gltr_param_a * tree_BA + Gltr_param_b,
      Genus == "Juglans"  ~  Juni_param_a * tree_BA + Juni_param_b,
      Genus == "Morus"  ~  (Mosp_param_c* tree_BA^2 +  Mosp_param_a* tree_BA + Mosp_param_b)*0.58, #.58 adjusts for sex ratio
      Genus == "Platanus"  ~ Plac_param_a * tree_BA + Plac_param_b,
      Genus == "Populus"  ~ (exp( Posp_param_a * tree_BA + Posp_param_b) * 0.482)/1000000000, #convert to billions
      Genus == "Quercus"  ~ Qusp_param_a * tree_BA + Qusp_param_b, #red oaks and unknown oaks
      Genus == "Quercus" & Species == "palustris" ~ Qupa_param_a * tree_BA + Qupa_param_b, #pin oaks
      Genus == "Ulmus"  ~ Ulsp_param_a * tree_BA + Ulsp_param_b)) %>% 
    mutate(per_tree_pollen_prod = case_when(per_tree_pollen_prod < 0 ~ 0, TRUE ~ per_tree_pollen_prod)) %>% #removing any negative value artifacts
    mutate(proportion_surviving = NumTrees/initial_num_trees,
            iter = i) 
  
  ifelse(i == 1, 
         pol_age_all <- pol_age_i,
         pol_age_all <- bind_rows(pol_age_all, pol_age_i))
  print(i)
}

pol_age_all %>% 
  group_by(scientificname, ForecastedYear, dbh_cm) %>% 
  summarize(pol_prod_mean = mean(per_tree_pollen_prod),
            pol_prod_sd = sd(per_tree_pollen_prod)) %>% 
  ggplot(aes(x= ForecastedYear, y = pol_prod_mean,
             ymin = pol_prod_mean - pol_prod_sd, ymax =  pol_prod_mean + pol_prod_sd)) + geom_ribbon(fill = "gray") + 
  ggthemes::theme_few() + facet_wrap(~scientificname, scales = "free_y") + geom_line()+
  xlab("year") + ylab("pollen production per tree (billions of grains)") + 
  scale_fill_viridis_c(name = "DBH (cm)")+scale_color_viridis_c(name = "DBH (cm)")+
  theme(strip.text = element_text(face = "italic"))
  
#ggsave("pollen_prod_indiv_time_species.jpeg", dpi = 300, width = 10, height = 7, units = "in")

pol_age_all %>% 
  group_by(scientificname, ForecastedYear, dbh_cm) %>% 
  summarize(pol_prod_mean = mean(per_tree_pollen_prod),
            pol_prod_sd = sd(per_tree_pollen_prod)) %>% 
  ggplot(aes(x= ForecastedYear, y = dbh_cm)) + 
  ggthemes::theme_few() + facet_wrap(~scientificname, scales = "free_y") + geom_line()+
  xlab("year") + ylab("pollen production per tree (billions of grains)") + 
  theme(strip.text = element_text(face = "italic"))


# # a little check on growth rates
# left_join(it_forecast3, individuals_to_use_in_fig) %>% 
#   filter(selected_indiv == "for_fig") %>% 
#   filter(scientificname == "Morus alba") %>% 
#   mutate(dbh_cm = 2.54 * DBH) -> test 

# it_forecast_citywide %>% 
#   filter(planted == "planted") %>% 
#   group_by(Genus, ForecastedYear, planted) %>% 
#   summarize(total_p_quad_genus = sum(total_p_quad),
#             total_n_trees = sum(n_trees)) %>% 
#   ggplot(aes(x = ForecastedYear, y = total_p_quad_genus)) + geom_area()+ ggthemes::theme_few() + xlab("year") +
#   ylab("pollen produced (quadrillions)") + facet_wrap(~Genus, scales = "free_y")

# #change in planted or unplanted trees over time
# it_forecast_citywide %>% 
#   #filter(planted == "planted") %>% 
#   filter(is.na(planted)) %>% 
#   group_by(Genus, ForecastedYear, planted) %>% 
#   summarize(total_p_quad_genus = sum(total_p_quad),
#             total_n_trees = sum(n_trees),
#             mean_ba = mean(ba_mean)) %>% 
#   ggplot(aes(x = ForecastedYear, y = total_n_trees, color = total_p_quad_genus)) + geom_point()+ ggthemes::theme_few() + xlab("year") +
#   ylab("pollen produced (quadrillions)") + facet_wrap(~Genus, scales = "free_y") + scale_color_viridis_c()


#figure: pollen production over time for each species
it_forecast_citywide %>% 
  ggplot(aes(x = ForecastedYear, y = total_p_quad, color = Species, group = scientificname)) + geom_point()+ ggthemes::theme_few() + xlab("year") +
  ylab("pollen produced (quadrillions)") + facet_wrap(~Genus, scales = "free_y")
#ggsave("pollen_prod_time_genus.jpeg", dpi = 300, width = 7, height = 4, units = "in")


#histogram of iTree DBH over a few different years
names(it_dbh_genus_np)
years_to_include <- c(0, 15, 30, 45)
it_forecast3 %>% 
  filter(ForecastedYear %in% years_to_include ) %>% 
  mutate(dbh_cm = DBH * 2.54,
    dbh_0 = round(dbh_cm, -1)) %>% 
  group_by(Genus, ForecastedYear, dbh_0) %>% 
  summarize(n_trees_dbh = sum(NumTrees))  %>% 
  ggplot(aes(x = dbh_0, y = n_trees_dbh)) + geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(Genus ~ paste("year:", ForecastedYear), scales = "free_y") + 
  xlab("DBH (cm)")+ ylab("number of trees in NYC")+ #scale_fill_viridis_d() + 
  ggthemes::theme_few() + theme(strip.text = element_text(face = "italic"))
#ggsave("n_trees_nyc_per_year.jpg", dpi = 300, width = 7, height = 10, units = "in")



### a couple misc stats #######################
sum(citywide_pol_join$total_p_quad_gen)
length(st_trees$genus[st_trees$genus == "Morus"])/length(st_trees$genus)
length(st_trees$genus[st_trees$genus == "Betula"])/length(st_trees$genus[st_trees$genus])



