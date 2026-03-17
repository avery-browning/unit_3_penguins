# 2026-03-17 class script - continuation of 2026-03-05 class script
# Multiple regression interactions and ANOVA

library(tidyverse)
library(palmerpenguins)

# summary(lm_3)

penguins_lm_3 = penguins %>%
  filter(!is.na(bill_depth_mm))
lm_4 = lm(bill_depth_mm ~ bill_length_mm + species + bill_length_mm * species, data = penguins_lm_3)
# shorthand: lm_4 = lm(bill_depth_mm ~ bill_length_mm * species, data = penguins_lm_3)
# see recording for explanation !!
# if you wanted just the interaction: lm_4 = lm(bill_depth_mm ~ bill_length_mm : species, data = penguins_lm_3)
# it's unusual not to include the indep variable -- -- - cont
summary(lm_4)

AIC(lm_3, lm_4) # error b/c lm_3 wasn't defined - see 3/5 class script
AIC(lm_3)
# if the difference in the AIC score is > the magnitude of 2, then ---------
# also compare r^2
# don't get caught up in the specific AIC value, focus on the DIFFERENCE between the values

# step fxn -- use for hw!
# use the step fxn on the most complex model
# takes the model and simplifies it 
best_model = step(lm_4)
best_model

# plot 
lm_4_predict = lm_4 %>%
  broom::augment(se_fit = T, interval = "confidence")
head(lm_4_predict)

ggplot(data = lm_4_predict) + 
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_line(aes(x = bill_length_mm, y = .fitted, color = species)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, x = bill_length_mm, fill = species), alpha = 0.3) +
  theme_classic()

# multiple regression w/ one than one continuous variable 

library(car) # vif() calculates variance in inflation factor ---------------

gentoo = penguins %>%
  filter(species == "Gentoo")
  summary(gentoo)

lm_gentoo_1 = lm(bill_depth_mm ~ bill_length_mm, data = gentoo)
lm_gentoo_2 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm, data = gentoo)
lm_gentoo_3 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g, data = gentoo)

AIC(lm_gentoo_1, lm_gentoo_2, lm_gentoo_3)
step(lm_gentoo_2)
summary(lm_gentoo_3)

lm_gentoo_4 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g - 1, data = gentoo)
# ^ removed intercept
summary(lm_gentoo_4)

# back to lm_gentoo_3
# -------------------

vif(lm_gentoo_3) # variance inflation factor
# if you have a vif>10 that's --------- 
# BIG healthy grain of salt
# in taxidermy hw - you might not be upset if things are perfectly co-linear
# traditionally we look at 2D plot, but we have 4 variables here, and will have a 4D plot
# -----------

# we are going to chooose a flipper length, and choose a body mass
# a good starting point is the MEDIAN (mean can get funky)
# holding 2 x's at median and explore the relationship between the y's 
# -----

newdata = gentoo %>%
  select(body_mass_g) 
  mutate(flipper_length_mm = median(gentoo$flipper_length_mm, na.rm=T))
  mutate(bill_length_mm = median(gentoo$bill_length_mm, na.rm=T))
  head(newdata)

# something off here ^ 

lm_gentoo_3_predict = lm_gentoo_3 %>%
  broom::augment(newdata = newdata, se_fit=T, interval = "confidence") 
head(lm_gentoo_3_predict)

ggplot(data = lm_gentoo_3_predict) +
  geom_point(aes(x = body_mass_g, y = bill_depth_mm), data = gentoo) +
  geom_line(aes(x = body_mass_g, y = .fitted)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, x = body_mass_g), alpha = 0.3) +
  annotate("text", x=4250, y=17, label = paste0("flipper length = ", median(gentoo$flipper_length_mm, na.rm=T))) +
  annotate("text", x=4250, y=16.5, label = paste0("bill length = ", median(gentoo$bill_length_mm, na.rm=T))) +
  theme_bw()

# more off ^ D:<

# exercise 5.3
# ------------ trying - incorrect, see example
ggplot(data = lm_gentoo_3_predict) + 
  geom_point(aes(x=bill_depth_mm, y=flipper_length_mm), data = gentoo) +
  geom_line(aes(x=bill_depth_mm, y= .fitted)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, x= bill_depth+mm), alpha = 0.3) +
  theme_bw()
# ---------------

# ANOVA
# mathematically equivalent to lm's 
# typically used where y continuous, x categorical 

penguin_lm = lm(body_mass_g ~ species + sex, data = penguins)
summary(penguin_lm)
anova(penguin_lm)

penguins %>%
  group_by(sex) %>%
  summarize(mean_body_mass_g = mean(body_mass_g, na.rm=T))

penguin_anova = aov(body_mass_g ~ sex + species, data = penguins)
summary(penguin_anova)

TukeyHSD(penguin_anova) # mind the capitalization for Tukey!

# see recording for more info 



