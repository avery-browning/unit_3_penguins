# 2026-03-03 ; class script
# Linear models - linear regression 

library(palmerpenguins)
library(tidyverse)
install.packages("ggiraph")
library(ggiraph)
library(GGally)
install.packages("ggiraphExtra")
library(ggiraphExtra) # ggPredict()
library(broom)  # tidy() augment() #does NOT load with tidyverse
library(car) # vif()

head(penguins)
summary(penguins)

ggpairs(data = penguins %>% select(where(is.numeric)))
ggpairs(data = penguins %>% select(bill_depth_mm, bill_length_mm))

# build a BAD model >:D
lm_1 = lm(bill_depth_mm ~ bill_length_mm, data = penguins)
summary(lm_1)

str(lm_1) # structure
lm_1$coefficients
class(lm_1)

ggplot(data = penguins, aes(x=bill_length_mm, y=bill_depth_mm)) +
  geom_point() + 
  geom_smooth(method = "lm")

?plot.lm
plot(lm_1)

# try just modeling one spp

# come back here - something missing / error
gentoo = penguins %>%
  filter(species == "Gentoo") %>%
          (!is.na(bill_length_mm))
head(gentoo)
summary(gentoo)

ggpairs(data = gentoo %>% select(bill_length_mm, bill_depth_mm))

lm_2 = lm(bill_depth_mm ~ bill_length_mm, data = gentoo)
summary(lm_2)
plot(lm_2)

ggplot(data = gentoo, aes(y=bill_depth_mm, x=bill_length_mm)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

# plot lm for all 3 species
ggplot(data = penguins) +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color = species)) +
  geom_smooth(aes(x=bill_length_mm, y=bill_depth_mm, color = species), method="lm") +
  geom_smooth(aes(x=bill_length_mm, y=bill_depth_mm), method="lm", color = "black")


# Simpson's Paradox - 

# Exercise 5.1 
# Build a lm predicting Gentoo bill depth as a fxn of flipper length. Plot the predictors. 
# What explanatory variable (bill depth vs. flipper length) does a better job at predicting bill depth?
# Why? 
ggpairs(data = gentoo) %>% select((flipper_length_mm, bill_depth_mm))

lm_3 = lm(flipper_length_mm ~ bill_depth_mm, data=gentoo)
summary(lm_3)
summary(lm_2)


