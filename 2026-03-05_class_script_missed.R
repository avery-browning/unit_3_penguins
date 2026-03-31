# 2026-03-05 missed class script

library(palmerpenguins)
library(tidyverse)

# Multiple regression - when you are predicting y as a fxn of more than one x

# the additional assumption you are supposed to make if you are doing a multiple linear 
# regression is that your x's are not colinear

# So if you're modeling for prediction, instead of, like, to understand the mechanistic 
# relationships between your variables, go ahead and throw lots of collinear independent 
# variables, or X's, into your model.independent variables, or X's, into your model.

# For the purposes of this class, there are two major types of X's, of independent variables 
# that you can feed your model. And that's true for a linear model, or for a generalized 
# linear model, for all kinds of modeling work.
# And those two kinds of X's are a continuous variable, Or a categorical variable.
# A continuous variable is just a number, and there's an infinite number of numbers, right?
# So it's a continuous variable, there are infinite number of possibilities.
# And it's modeled like a continuous variable, like this consistent function.

# Categorical variable. That's when you have a fixed number of distinct groups, and 
# it's got a lot of names, which I think is one source of confusion. Categorical 
# variables, depending on who you're talking to, might be called discrete variables.
# Discrete means, like, it stands alone as a complete unit. 33.1 as a bill length 
# doesn't stand alone, because there could be a 33.0001 right after it. That's 
# continuous. But discrete is, like, it's by itself, and then there's a gap 
# before the next one.
# They're also called nominal variables, and in R, kind of lazily, we might call them factors.
# Factor is just the class that we use, to categorize a categorical variable.
# And so the example that we've been using of categorical variables is species. There's 3 
# species in our dataset. A daily, chinstrap, and Gentu.
# No more, no less. There's no, like, half a daily, half chin strap. There are three 
# distinct variables, so they're categorical. Sex.
# Male, female, and, you know, if you want to model it, NA.
# Those are… those are distinct categories in a categorical variable.
# And they look really different when we model them, and today we're going to focus on 
# modeling where our second variable is a categorical variable. And then, in the next 
# class, we're going to add a second X where the second X is continuous, which is 
# actually a little bit trickier.
# But that… that need for a categorical variable is where we left off in the last 
# class. We needed species to build a sane model for this bill length, bill depth comparison.
# Here, I talk about year.

summary(penguins)
unique(penguins$year)
?unique
penguins %>% distinct(year)

# I'm gonna start off by creating a dataset that drops my NAs. We ended off with, 
# like, in the tutorial with LM2, so we're gonna build this model and call it LM3.

penguins_lm_3 = penguins %>%
  filter(!is.na(bill_depth_mm))

# build a multi regression model :) :D !!!
lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data = penguins_lm_3)
summary(lm_3)

# Okay, so this looks different from last time. Last time, these rows were here, we 
# just had a y-intercept, and then a slope that was comparing buildups to bill 
# length. So that part is similar, and the estimates are reasonably similar, 
# but it's added two additional coefficients.
# So now, instead of calculating two coefficients. Like, this is essentially 
# a comparison between chinstrap and adelie. And then how will we offset 
# it for gentoo? This is a comparison between gentoo
# and adelie, which is buried in the intercept. If you don't know it's 
# buried in the intercept, you're probably not going to do a good job interpreting 
# this model. So do one of those functions I showed you above, distinct or unique.
# Figure out all the species, you know, all the categories in your categorical variable, 
# and which one is missing.
# What else do I see? All of these p-values are significant. Like, super, super significant.
# the multiple R squared, and then the adjusted R squared, which penalizes the 
# multiple R-squared for the 4 coefficients that it calculated, 1, 2, 3, 4.
# The adjusted R-squared is .7669. So that means that this model explains that 
# it's 77% of the variation in bill depth.

# The coefficient for bill length is positive. That feels really good. 
# As the bill gets longer, the bill gets deeper. That kind of hits that gut check, 
# that common sense check. And then, it's a little bit harder to interpret those, 
# adjustments. It looks like. The y-intercept for these two species is lower than 
# the y-intercept for the species buried in the intercept, which is the Adelie.

# ANOVAs
# ANOVAs are most frequently presented when all of your X's are categorical, 
# so this is probably not the best case for an ANOVA, because we have bill length 
# as one of our X's that's continuous. But you can present an ANOVA that includes 
# continuous variables in your independent variables.

anova(lm_3)
# So what I did here is I fed the model. This is the model class, LM class, LM3, 
# the output from the LM function. I fed it to the ANOVA function, and it said, 
# oh, I see you've already run this linear model, and you just want me to present 
# the results in an ANOVA table. And so did this.
# And it looks very different. I don't play with ANOVAs as much, so I'm like, 
# I feel like this is hiding a lot of important details.
# I think we'll talk about ANOVAs at the end of next class, but here it's got… 
# it says that bill length is statistically significant, it's an important predictor of buildup.
# And then it says species is also an important predictor, but it doesn't break down 
# anything about each species.
# If then you wanted to say, well, what is the difference between an Adelie and a 
# chin strap? You'd have to use a post-hop Tukey test, which I think we'll take a 
# minute and do in the next class.
# But that's one reason I don't like it, is I feel like it buries some of the 
# important information. Same math, same results. If you want to present it this 
# way, you can use the ANOVA function, and I'll show you another function in the 
# next class. The other thing I wanted to show you…
# and I think this is pretty great, is, In the Tidyverse, one of the packages is the 
# Broom package, and we use mostly dplyr and ggplot in this class. But Broom, the 
# point of the Broom package, is to make model results easy to play with. And we 
# have model results, and right now they're kind of ugly.
# So I'm going to show you how you can use the broom package to make them easier to play with.
# So… when you load the Tidyverse, it doesn't automatically load the broom package, 
# so you can either call it with Library Broom, it's already installed, it installed 
# with the Tidyverse, but it didn't load with the Tidyverse, which is confusing.
# Or if you just call it, it'll say, we see you didn't load the broom package, 
# but you're explicitly calling the broom package, so we're gonna look in the broom 
# package to find the function.
# So, broom, tidy. the tidy function will take the output of your model and turn 
# it into something that's easy and fun to play with. So all we have to do is feed it the model.
broom::tidy(lm_3, conf.int = T)

# And it spit out that model table that we've been talking about, but now it's in a tibbleL, 
# which is, like, the same as the data frame. So this is very easy to deal with. Now we 
# can call the column, dollar sign estimate, we could call the row number, we could 
# filter by intercept. here is, one way to make this even, provide even more information. 
# We can ask for the confidence intervals around the coefficient estimates. So we do that 
# with the parameter conf.int. equals true.

#  it provided confidence intervals around the coefficient. So the y-intercept 
# estimated was 10.6, and the confidence intervals around it are 9.25 to 11.9. If 
# you query the health page, I would query it by saying, tidy.lm.
# I want the version of the tidy function that uses an LM class object.

# By default, confident.interval is false. You can also set the confidence interval level, 
# which by default is set at .95. When I do this in my own scripts, I like to 
# remind myself that it's set at 0.05 by actually explicitly coding it in. 

lm_3_table = broom::tidy(lm_3, conf.int = T, conf.level = 0.95)
write.csv(lm_3_table, file = "figures/lm_3_table.csv")
# So that's unnecessary and redundant, but it reminds me what the confidence interval is set at.
# Like, these don't cross zero.
# 9 to 11 doesn't cross 0, which is another gut check that that y-intercept is statistically 
# significant. That's why that p-value is so small.