# 2026-02-17 class script

# ggplot intro
# check out the posit cheat sheet to reference for help

library(tidyverse)
library(palmerpenguins)

head(penguins)
find("filter") # says please find the filter function - returns "package:dplyr" "package:stats"
                # dplyr is listed first - so this is what R will use

# make a ggplot 

ggplot(data = penguins) +  # remember the + signs!
  geom_point( aes(x=flipper_length_mm, y=body_mass_g, 
    color= species, shape = sex)) + # in ggplot use 'color' not 'col' for specific color
  geom_smooth( aes(x=flipper_length_mm, y=body_mass_g), method = "lm") +
  xlab("Flipper length (mm)") +
  ylab("Body mass (g)") +
  ggtitle("yay penguins!") +
  theme_bw()
 
penguins_ts = penguins %>% # making a time series 
  group_by(species, year) %>%
  summarize(n_penguins = n())

ggplot(data = penguins_ts) +
    geom_line( aes(x = year, y = n_penguins, color = species) ) +
    theme_bw()


# univariate plotting - histrogram

ggplot(data = penguins) +
  geom_histogram(aes (x=flipper_length_mm, fill=species), 
                position = "identity", 
                alpha = 0.75, binwidth = 5) + # density of flipper length across the range
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4")) +
  theme_bw()

# box plots!
ggplot(data = penguins) +
  geom_boxplot(aes( y= flipper_length_mm, x = species)) +
  geom_jitter( aes(y = flipper_length_mm, x = species, color=species), width = 0.2) +
  xlab("") +
  theme_bw()


# bar plots
my_sex_plot = ggplot(data = penguins %>% filter(!is.na(sex))) +
  geom_bar(aes (x=sex, fill=species)) +
  facet_wrap(~species, nrow=3, scales="free") + # generate a panel for each variable that you sent
  coord_flip() + # flip x and y
  theme_bw()

my_sex_plot
ggsave(filename = "figures/my_sex_plot.png", # .png does not have loss or compress = better quality
      plot=my_sex_plot,
      width = 5,
      height = 4,
      units = "in",
      dpi = 30)   # by default save the last thing you plotted, 
# but can save based on an obj name like we did here
# if you want to save it somewhere else

# exercise 2.2
