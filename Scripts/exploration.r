# data ####

# exp 1
gh_1_clean

# exp 2
gh_2_clean
gh_2_fitness
gh_2_mass



# exploration ####

p1_exp <- names(gh_1_clean[2])
p1_exp <- set_names(p1_exp)

p1_resp <- names(gh_1_clean[5:8])
p1_resp <- set_names(p1_resp)


plot_fxn <- function(x,y){
  ggplot(gh_1_clean, aes(x = .data[[x]], y = .data[[y]]))+
  geom_point()+
  ylim(0,NA)+
  stat_summary(fun = 'mean', color = 'red', size = 5, geom = 'point')+
  theme_bw()+
  labs(title = 'X')
}

p1_plots <- map(p1_resp, 
                ~map(p1_exp, plot_fxn, y = .x))
p1_list <- map(p1_plots, ~cowplot::plot_grid(plotlist = .x))
ggarrange(plotlist = p1_list)
