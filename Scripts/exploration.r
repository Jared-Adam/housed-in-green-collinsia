# data ####

# exp 1
gh_1_clean

# exp 2
gh_2_clean
gh_2_fitness
gh_2_mass



# p1 exploration ####

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
  labs(title = 'Exp 1')
}

p1_plots <- map(p1_resp, 
                ~map(p1_exp, plot_fxn, y = .x))
p1_list <- map(p1_plots, ~cowplot::plot_grid(plotlist = .x))
ggarrange(plotlist = p1_list)

# p2 exploration ####

p2_exp <- names(gh_2_fitness[3])
p2_exp <- set_names(p2_exp)

p2_resp <- names(gh_2_fitness[7:9])
p2_resp <- set_names(p2_resp)

p2_plot_fxn <- function(x,y){
  ggplot(gh_2_fitness, aes(x = .data[[x]], y = .data[[y]]))+
    geom_point()+
    facet_wrap(~water_trt)+
    ylim(0,NA)+
    stat_summary(fun = 'mean', color = 'red', size = 5, geom = 'point')+
    theme_bw()+
    labs(title = 'Exp 2')
}

p2_plots <- map(p2_resp,
                ~map(p2_exp, p2_plot_fxn, y = .x))
p2_list <- map(p2_plots, ~cowplot::plot_grid(plotlist = .x))
ggarrange(plotlist = p2_list)

