# data ####

# exp 1
gh_1_clean

# exp 2
gh_2_clean
gh_2_fitness
gh_2_mass



# exploration ####

gh_1_clean %>% 
  ggplot(aes(x = trt, y = mass_g))+
  geom_point()+
  stat_summary(fun = 'mean', color = 'red', size = 5, geom = 'point')
