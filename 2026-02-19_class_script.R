# 2026-02-19 class script
# statistical diagnostics, t-tests, correlations

library(tidyverse)
library(palmerpenguins)
# install.packages("rstatix")
library(rstatix)

head(penguins)

gentoo = penguins %>% 
  filter(species == "Gentoo") %>%
  droplevels()
head(gentoo)
summary(gentoo)
dim(gentoo)

mean(gentoo$body_mass_g, na.rm=T)
sd(gentoo$body_mass_g, na.rm=T)

ggplot() +
  geom_histogram(data = gentoo, aes(x = body_mass_g))

# QQ plot - quantile quantile
ggplot() +
  stat_qq(aes(sample = body_mass_g), data = gentoo)

gentoo_body_mass_g_symonds = 5500 # from Symonds and Tattersall 2010, accessed via EOL

my_t_test = t.test(gentoo$body_mass_g, mu = gentoo_body_mass_g_symonds)
my_t_test
class(my_t_test) # htest -> hypothesis test
summary(my_t_test)
str(my_t_test) # str -> structure
my_t_test$p.value # returns p value from t-test

# try a pipe-friendly t-test from rstatix package

my_t_test2 = gentoo %>% 
  t_test(body_mass_g ~ 1, mu = gentoo_body_mass_g_symonds)
my_t_test2$p

# independent sample t-test

data_for_t_test = penguins %>%
  dplyr::filter(species %in% c("Gentoo", "Adelie"),
                !is.na(body_mass_g)) %>%
  droplevels()

summary(data_for_t_test)

ggplot(data = data_for_t_test) +
  geom_histogram(aes(x=body_mass_g)) +
  facet_wrap(~species, scales="free")


ggplot(data=data_for_t_test) +
  stat_qq(aes(sample = body_mass_g)) +
  facet_wrap(~species, scales="free")

# check equality of variance
# if true, can use student's t-test
# if false, use Welch's

data_for_t_test %>% levene_test(body_mass_g ~ species) # if p value is < 0.05, variances are NOT equal - use Welch's

my_indy_t_test = t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species,var.equal=T)
class(my_indy_t_test)
my_indy_t_test

# using rstatix
# using Welch's
data_for_t_test %>%
  t_test(body_mass_g ~ species)
# using Student's
data_for_t_test %>%
  t_test(body_mass_g ~ species, var.equal=T)



#################################
# 3.4 correlations
# bill depth vs. length
#################################

head(data = gentoo)
ggplot(data = gentoo) +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm))

ggplot(data = gentoo) +
  stat_qq(aes(sample = bill_depth_mm))

cor(x=gentoo$bill_depth_mm, y=gentoo$bill_length_mm, use="complete.obs")

cor.test(x=gentoo$bill_length_mm, y=gentoo$bill_depth_mm, use="complete.obs")

gentoo %>%
  cor_test(bill_depth_mm, bill_length_mm) # "_" is pipe-friendly, "." is NOT

install.packages("GGally")
library(GGally) # normally load libraries at TOP of script

gentoo %>%
  select(body_mass_g, bill_length_mm, bill_depth_mm, flipper_length_mm) %>%
  GGally::ggpairs()

head(gentoo)
gentoo = gentoo %>%
  filter(!is.na(body_mass_g)) # remove NAs
cor(gentoo[, seq(3,6)] ) # creates a cor matrix

penguins %>%
  select(species, where(is.numeric)) %>%
  GGally::ggpairs(aes(color = species))
