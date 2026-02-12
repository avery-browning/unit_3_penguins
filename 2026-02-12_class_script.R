# class script 2026-02-12

# Intro to dplyr

# install.packages("tidyverse")
# install.packages("palmerpenguins")

library(tidyverse)
library(palmerpenguins)

head(penguins)
summary(penguins)

# add pic of penguin in code for bonus points

glimpse(penguins) #turns the dataset on its side
dim(penguins)

# filter
# subset to look at just the gentoo penguins
gentoo = filter(penguins, species == "Gentoo")
summary(gentoo)

gentoo_ladies = filter(penguins, species == "Gentoo", sex == "female")
summary(gentoo_ladies)

# introduce the pipe - takes penguins and feeds it to the filter fxn
# in math, this would be like y = h(g(f(x))) -> first take x, then do g, then do h, and that equals y
gentoo_ladies = penguins %>%
  filter(species == "Gentoo") %>%
  filter(sex == "female") %>%

  
gentoo_ladies_body_mass_g = penguins %>%
  filter(species == "Gentoo") %>%
  filter(sex == "female") %>%
  summarize(gentoog_ladies_body_mass_g = mean(body_mass_g)) %>%
  print()  

gentoo_ladies_body_mass_g = mean(penguins$body_mass_g[penguins$sex == "female" & penguins$species == "Gentoo", na.rm=T])

# exercise 1.1
chinstrap = filter(penguins, species == "Chinstrap")
summary(chinstrap)
chinstrap_long_flippers = filter(penguins, species == "Chinstrap", flipper_length_mm > 200)
summary(chinstrap_long_flippers)

# sex ratio 34:34
chinstrap_females = filter(penguins, species == "Chinstrap", sex == "female")
summary(chinstrap_females)
chinstrap_males = filter(penguins, species == "Chinstrap", sex == "male")
summary(chinstrap_males)

# sex ratio long flippers
chinstrap_long_flippers_male = filter(penguins, species == "Chinstrap", flipper_length_mm > 200, sex == "male")
summary(chinstrap_long_flippers_male)
chinstrap_long_flippers_female = filter(penguins, species == "Chinstrap", flipper_length_mm > 200, sex == "female")
summary(chinstrap_long_flippers_female)

# use group_by()
species_mean_mass = penguins %>%
  filter(!is.na(sex)) %>%
  group_by(species, sex) %>%
  summarize(mean_body_mass_g = mean(body_mass_g, na.rm=T),
            sd_body_mass_g = sd(body_mass_g, na.rm=T),
          n_penguins = n())
species_mean_mass

# you can use ungroup() to make dplyr ignore a previous group

write_csv(species_mean_mass, file = "data/processed/my_sweet_sweet_body.csv")

sd_body_mass_g

# go back to add write.csv code

# mutate
penguins_for_america = penguins %>%
  mutate(body_mass_lb = body_mass_g * 0.0022) #0.0022 lb/g
head(penguins_for_america)

penguins %>% distinct(island)

temp = penguins %>%
  select(-body_mass_g) # - removes a column
head(temp)

temp = penguins %>%
  arrange(desc(body_mass_g)) # sorts descending
head(temp)
tail(temp)