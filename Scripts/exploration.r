# packages ####
library(RColorBrewer)
library(marginaleffects)
library(performance)

# data ####

# exp 1
gh_1_clean

# exp 2
gh_2_clean
gh_2_fitness
gh_2_mass
gh_2_long


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

gh_1_clean %>% 
  ggplot(aes(x = trt))+
  geom_bar(aes(fill = mass_g))


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


# GAM gh 2 long fitness ####

gh_2_long %>% 
  ggplot(aes(x = dad, y = value, color = trt, fill = trt))+
  geom_smooth(method = 'gam',
              formula = y ~ s(x,k =4))+
  facet_grid(~fitness)+
  theme_bw()


# GAM gh 2 x drought ####
gh_2_fitness %>% 
  mutate(trt = case_when(trt == 'C' ~ 'Control',
                         trt == 'E' ~ 'Edge',
                         trt == 'HP' ~ 'Hole Punch',
                         trt == 'MV' ~ 'Mid Vein',
                         trt == 'T' ~ 'Leaf Tip')) %>% 
  ggplot(aes(x = dad, y = fruit_count, color = trt, fill = trt))+
  geom_smooth(method = 'gam',
              formula = y ~ s(x, k =4),
              size = 2)+
  theme_bw()+
  theme(axis.title = element_text(size=24),
        panel.grid = element_blank(),
        plot.subtitle = element_text(size=20, hjust = 0.5),
        axis.text = element_text(size = 24),
        legend.text = element_text(size = 18),
        axis.ticks.length = unit(.25, 'cm'),
        legend.title = element_text(size = 20))+
  guides(color=guide_legend(title="Damage Treatment"), fill = FALSE)+
  scale_fill_brewer(palette = 'Dark2')+
  scale_color_brewer(palette = 'Dark2')+
  labs(x = "Days after damage",
       y = "Fruit count")


# gh2 damage treatment - plant totals
ggplot(data = gh_2_plant, aes(x=trt, y=fruit_count)) +
  geom_jitter() +
  scale_y_sqrt() +
  stat_summary(color='red')


# Focus on C, HP, and T for proposals ####

# Subset data and prep
gh_2_plant_CHPT = gh_2_plant %>%
  subset(trt == 'C' | trt == 'HP' | trt == 'T') %>%
  droplevels() %>%
  mutate(trt = fct_recode(trt,
                          "Control" = "C",
                          "Dispersed"   = "HP",
                          "Contiguous"       = "T"
  ))

# Fit models
m = glm(fruit_count ~ trt, data = gh_2_plant_CHPT, family='poisson')
summary(m)
r2_mcfadden(m)
m0 = glm(fruit_count ~ 1, data = gh_2_plant_CHPT, family='poisson')
anova(m0,m)

# Calc effect sizes
m.means = exp(c(coef(m)[1], coef(m)[1] + coef(m)[2], coef(m)[1] + coef(m)[3]))
m.means
(m.means[1] - m.means[3]) / (m.means[1] - m.means[2])

(m.means[1] - m.means[2]) / m.means[1]
(m.means[1] - m.means[3]) / m.means[1]

# Plot raw data
gh_2_plant_CHPT %>%
  ggplot(aes(x=trt, y=fruit_count)) +
  geom_jitter() +
  stat_summary(col='purple')

# Plot model predictions
mytheme =
  theme_bw() +
  theme(axis.title = element_text(size=12),
        legend.title = element_text(size=12),
        panel.grid = element_blank(),
        plot.subtitle = element_text(size=12, hjust = 0.5))

p = plot_predictions(
  m,
  condition = 'trt',
  newdata=datagrid()
) +
  mytheme +
  labs(x= 'Damage treatment', y = 'Fruit production')

p

ggsave('plots/Collinsia_damagetype_prelim.pdf',
       p,
       width = 2.75,
       height = 2.75,
       units='in',
       dpi=600)
