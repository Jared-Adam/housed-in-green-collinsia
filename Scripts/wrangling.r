# packages #####
library(tidyverse)
library(ggpubr)


# import ####
gh_1 <- X2025_7_21_GH_pilot1_JSA
gh_2 <- X2025_4_17_GH_pilot2_JSA

# looksy ####

gh_1_clean <- gh_1 %>% 
  mutate(across(everything(),(~replace(.,.== 'na', NA)))) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  mutate_at(vars(8:9), as.numeric) %>% 
  mutate(mass_g = rowSums(.[8:9], na.rm = T)) %>% 
  filter(diameter_mm != 'NA') %>% 
  mutate_at(vars(2:3), as.factor) %>% 
  mutate_at(vars(4:5), as.numeric) %>% 
  mutate(growth = as.factor(gs)) %>% 
  select(!c(g_full, g_spec, gs)) %>% 
  relocate(date, trt, plant, growth) %>% 
  print(n = 50)

gh_2_clean <- gh_2 %>% 
  mutate(across(everything(),(~replace(.,.== 'na', NA)))) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  relocate(date, dad, trt, stage, plant) %>% 
  mutate_at(vars(2:6), as.factor) %>% 
  mutate_at(vars(7:10), as.numeric) %>% 
  print(n = 50)

unique(gh_2_clean$date)

gh_2_mass <- gh_2_clean %>% 
  filter(date == '2025-05-19') %>% 
  select(!c(dad, stage, diameter_mm, leaf_count, fruit_count)) %>% 
  drop_na() %>% 
  print(n = Inf)

gh_2_fitness <- gh_2_clean %>% 
  filter(date != '2025-05-19') %>% 
  select(!mass_g) %>% 
  print(n = 10)
unique(gh_2_fitness$date)





