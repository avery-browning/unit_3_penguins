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
write_csv(lm_3_table, file = "figures/lm_3_table.csv")
# So that's unnecessary and redundant, but it reminds me what the confidence interval is set at.
# Like, these don't cross zero.
# 9 to 11 doesn't cross 0, which is another gut check that that y-intercept is statistically 
# significant. That's why that p-value is so small.

# If we did write CSV in the past, we might have done write.csv, which is in Besar, 
# write underscore CSV is in Dplyre, doesn't really matter.
# write.csv also writes row names, so there'd be, like, this extra jump junk column 
# that just said 1, 2, 3, 4, and that irritates me, so if I'm thinking about it, 
# I use write underscore CSV.

# I'm going to show you one more sneaky, fast and loose way to plot a model, and 
# then we're going to dig into the weeds of how, very carefully, to generate 
# your own model predictions, and then plot them. So the last sneaky way is 
# in these two libraries.

library(ggiraph)
library(ggiraphExtra)

# But this has this really slick function called ggpredict. And so we're gonna 
# use it really quick. GG PREDICT, with a capital P, And we can just pass it the model.
# And it's relying on ggplot functionality, and it's looking at what kind of model 
# it is. Of course, it's a linear model.

ggPredict(lm_3, se = T, interactive = T)

# There are 3 lines, because depending on which species you have, there are 3
# intercepts, or one intercept, which is the… adelie intercept, and then 
# two adjustments to that intercept. So that's the role of the categorical 
# variable. It makes it look like these three lines, because there are these 
# adjustments to intercepts with different categories of species.
# because I have GDPredict the model, and the background of the model, when 
# R built that model, it used the original data, it remembers the original data, 
# and it went ahead and plotted it for me.

# Now I'm gonna make this a little bit fancier and add a parameter, which is SE, 
# standard error, equals true. 
# se = T
# And what it did was it added this standard error ribbon behind the plot.
# That's pretty nice. Usually, if you submit a model without standard errors to it, 
# reviewer number 2 is going to say, where are your errors? So, that's there for you.
# And we're gonna go through, for the rest of the class, how to do this yourself, 
# not using a slick function.

# One more parameter... interactive = T
# But remove the theme if you use this
# Okay, I don't really recommend this for generating plots that you're 
# going to put in a peer-reviewed paper, but if you're trying to get to 
# know your data, and you're gonna play with 5 different models today as 
# you, like, figure out where your dissertation is going, this is really 
# fun. My mouse now, if I select a point.
# Like, maybe I'm suspicious of this point, or this point. This is a very 
# deep bill. So I could hover over it, and it's gonna say that bill is in row 
# 19 of the data I fed, which was, remember, Penguins LM3, not penguins.
# Row 19, and that's the bill length and the bill depth. So it kind of just 
# makes it really easy for each individual data point to pop out at you. And 
# then if I hover over… a model line, a model prediction, it shows me the… 
# Equation for that model. So in this case, why the bill depth is… 0.2, I mean,
#  it's rounded.2 times the bill length, plus 10.59. I remember 10.59 was the intercept.
# But if I hover over the chin strap model, this is actually the intercept with 
# the correction for the chin strap, which was 10.5 minus something like 2.
# And so you can see the exact models underlying the lines that you see plotted here.
# It's like a trick, like a little fun bell and whistle that you can do. 
# I honestly only ever use this in class to show you guys that I can do it.

# Now we're going to go through 3 different methods for how to generate your own predictions.
# You've got your data, How would you predict that with a bill length of 40 in a 
# Gentoo penguin, how do you predict what the buildup is? You hover over 
# that line, you can see the math behind it, but now we're going to generate 
# predictions for every point that we want to.

# We're gonna start off using the predict function in BASAR. It's actually 
# probably in the stats package, but that's part of BASAR. And I'm going to 
# store my predictions in a new variable called LM3Predictions.
# Using the predict function from BASAR, I'm going to feed it the model that I've already built.

lm_3_predictions = predict(lm_3)
head(lm_3_predictions)

# I fed up the model, and it gave me a vector of numbers. 18.4, 18.4, 18.6. 
# What it did was it took all the underlying bill length and species data in
# Penguins LM3 that we fed to LM3, It's taking all those, and it's generating Y 
# predictions for each row from Penguins LM3.
# This is essentially what all those plots that we just made did in the background.
# So that's nice, now we have predictions, we could draw the line.
# We can, amplify it one step by also generating those standard error 
# ribbons underneath. And we do that with interval equals confidence.

lm_3_predictions = predict(lm_3, interval = "confidence")
head(lm_3_predictions)

# So we're gonna ask it to generate confidence intervals around each Y prediction 
# that it created, and we're going to… just like I did before, I'm gonna set the 
# level, even though I'm agreeing with the defaults, I do this for my own personal 
# well-being.
# So run that again and take a look

lm_3_predictions = predict(lm_3, interval = "confidence", level = 0.95)
head(lm_3_predictions)

# Now instead of a single vector, it 
# created 3 vectors and put it in a table for me, a data frame, probably, because 
# we're still in base R, where fit is the predicted Y value, the predicted bill depth.
# And lower and upper is the lower and upper 95% confidence interval for this exact prediction.

# And worth noting… If I can… I'm gonna get rid of this interactive thing.


ggPredict(lm_3, se = T)

# Pull up this plot real fast.
# It's not super fun to see.
# But the 95% confidence interval is not consistent across the line. 
# It's wider here at the edge, and then in the middle, where there's more 
# data to guide the model, it's thinner.
# It's not super obvious in this example, but this is not just two straight lines 
# underneath the main line. There's a shape to it.
# And that's gonna come up.

# This is another moment that drives people, like, they just get confused. 
# I just have these predictions, and I don't know which X was used to generate them.
# And it's the X's that were pulled from Penguin's LM3 that we fed to the LM3, 
# to the linear model function that we used to create LM3.
# So, to make it just less confusing for me to work with, I'm gonna take these predictions.
# And join them back up with the original data.
# Using CBind, which is column bind. Take one set of columns, and another 
# set of columns, and squish them together this way. It's just adding new 
# columns to your data set.
# And it should be the same length.
# Like, let's do DIM LM3 predictions.
# And note that they're both the same number of rows.

dim(lm_3_predictions)
dim(penguins_lm_3)

# Should probably name these something more clear, but we're gonna call it Penguins 
# LM3, which is what we started with, predict, meaning we've added predictions to 
# this table. Equals C bind. Penguins LM3. with our new predictions, which we 
# called LM3 predictions.

penguins_lm_3_predict = cbind(penguins_lm_3, lm_3_predictions)
head(penguins_lm_3_predict)
dim(penguins_lm_3_predict)

# If they have different rows, number of rows, then that means that this 
# Penguins LM3 data is not the data that PREDICT used.
# Which, that's what we're gonna do really soon.
# So, we're gonna see the sense of that soon. But for now, it just used the 
# underlying data that built the model, and that's why the number of rows equals.
# And checking out the number of rows in our new C-binded data frame, it's the same 
# number of rows, and it's the two sets of columns added together, so that feels good. 
# We see the data that we're comfortable with.
# Starting with the adelies, and then it added 3 additional rows. The Y prediction.
# And the upper and lower 95% confidence interval for that prediction. So things 
# are going well, this is feeling reasonably sane.

# Now, I want to plot them. I made my own predictions. I don't want to rely on these 
# cute little… the smoother with method equals lm, or the ggpredict, that flashy function 
# in the giraffe package. 
# I want to build my own figure, using predictions that I generated, because I have a lot of control over that.
# So we're gonna use ggplot.

# Our data is all gonna come from this new table we created, Penguins LM3 Predict, 
# because it has the original data, and it has the predictions, it has everything we need.
# I'm gonna try and put some aesthetics in this original call, so I don't have to repeat myself 
# too much.

ggplot(data = penguins_lm_3_predict, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point()

# Let's add our predicted line.
# GEOM line… And… This time, we're gonna overwrite… the bill… Depth aesthetic to be our fit.
# Which was called… fits. So, by default, for all the geometries, it's gonna try and
# It's gonna try and use the X, Y, and color that I gave in the aesthetics for the GG plot call.
# But in the case of G Online, I overwrote Y. Said, use everything just like in the 
# original call, but for Y, use FIT.

ggplot(data = penguins_lm_3_predict, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  geom_line(aes(y = fit))

# Now I want to add my confidence interval ribbons. We're going to use a new aesthetic called GM Ribbon.
# And this time, we're still gonna use X equals… Bill Length.
# But we've got to provide it an upper and a lower Y value for that ribbon.
# which we've calculated already very nicely, it's not gonna be that hard. And the 
# parameters for the upper and lower are Ymin and Ymax. So we're gonna say Ymin
# equals… I scroll up and see that I called it, or the predict function called it lower, 
# LWR. Ymin equals LWR, Y max equals UPR for upper.
# Let's just try that and see what happens. And change the theme.

ggplot(data = penguins_lm_3_predict, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr)) +
  theme_bw()

# Whoa! We've got our ribbons, looking awesome, except for not awesome. It filled it in with black.
# So let's change the fill. We're gonna say…
# Remember, color is, like, the outline of an object, and fill is when you color it in. 
# Like, fill it in with a crayon. So fill equals species, this is gonna improve it a little bit.
# Okay, that looks better, but now what we want to do is we want to change the transparency 
# of the confidence interval ribbon so that we can see the stuff underneath, which that's the 
# alpha parameter, alpha for transparency.
# alpha equals… I don't know, let's try… 0.7.
# And if you forget what the alpha does, remind yourself.
# Alpha is the transparency parameter.

ggplot(data = penguins_lm_3_predict, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = species), alpha = 0.7) +
  theme_bw()

# Too much. Let's do .5.

ggplot(data = penguins_lm_3_predict, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = species), alpha = 0.5) +
  theme_bw()

#Okay… That's looking good. I can see the underlying data.
# Color is species, still, because of the original…
# aesthetics call up here in ggplot, and that's why our ribbons are outlined in blue. 
# If we want to emulate the plots we've already seen, we don't want that.

ggplot(data = penguins_lm_3_predict, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = species, color = NULL), alpha = 0.5) +
  theme_bw()

# Compared to the other plots that we've generated, this plot only generated predictions 
# across the range of data for a given category. So for the gentoo category, it's only 
# generated predictions from the smallest Gen 2 beak length to the largest gentoo length.
# In general, actually, I'm pretty happy with that, because we know we're not supposed 
# to extrapolate beyond the range of our data, although as scientists, we do that all the time.
# But this really kind of reins you in there. We can't make necessarily good predictions 
# for Genju penguins with a beak length of 35 millimeters, because we've never seen what 
# that looks like in the wild.
# So that's great. But there are reasons where you do need to extrapolate beyond the 
# bounds of your original data, so now what we're going to do is we're going to feed the prediction
# function new data.

# We're gonna say, what if you had a gentoo with a bill length of 35? 
# What would the prediction be, given this model that's already been fit?
# And we're slowly venturing into the reason that we want more control over 
# generating our own predictions.

# -Generate new data so we can extrapolate beyond the data feeding the model-

# The first thing that we need to do is generate a fake data set of bill length that 
# we're going to use for our predictions. Like, what if you've got a Gen 2 with a 
# bill length of 35? We have to generate that bill length of 35 before we can create 
# our prediction.
# So we're going to generate this fake data set, we're going to call it new data, and 
# the reason I use the word new data is because in the predict.lm fxn, the second 
# parameter is called new data.

newdata_bill_length_mm = seq(from = min(penguins_lm_3$bill_length_mm), 
                        to = max(penguins_lm_3$bill_length_mm), by = 0.1)

#  So I'm calling it new data to remind you that this is what we're going to call the parameter.
# It's gonna assume that you want to use the data that are inherent within the object 
# unless you pass it new data, and then it will generate its predictions off of the new data.
# So, our new data for bill length.
# in millimeters, we're going to make it up. We're faking it here. If it was me, I'd call 
# it fake data. So we're gonna start with a sequence which we're comfortable with, the sequence 
# function. The minimum… Penguins, dollar sign, bill, length, In millimeter.

newdata_bill_length_mm

# Max and min probably have an NA. remove equals true parameter, which I could set, 
# but instead I'm going to just use the data that I pulled the NAs out of.
# So, starting with the very smallest bill lengths in my penguins data, and going to the 
# very largest bill length, regardless of species, and moving with increments of 0.1, give me 
# a whole list of bill lengths.
# And it goes… the smallest bill length in the data is 32.1, and then it says 32.2, 32.3, 32.4, 
# all the way to the largest bill length of 59.6 millimeters.
# So I generated a fake data set.

# So now I'm gonna make a new dataset. This is just new data in general, not just my bill length stuff.
# And I'm gonna say, let's try and use this expand.grid function. I'm gonna feed it
# I need new data to have three… two columns in it. It has to have bill length, and it 
# has to have species. If it doesn't have those two columns, my LM3 cannot generate predictions, 
# because it needs those two X's. And I'm going to very carefully name those columns exactly 
# what LM3 is looking for.
# Bill length, millimeters, and species.
# So my bill length in millimeters equals this new data that I just created. 
# New data underscore bill length millimeters.
# Now, species equals… We could say unique, Penguins… LM3, dollar sign species. 

newdata = expand.grid(bill_length_mm = newdata_bill_length_mm, species = unique(penguins_lm_3$species))
head(newdata)
tail(newdata)

# Alright, so what is expand.grid gonna do?
# It took that list of… I don't know how many build lengths that we generated in our fake data set.
# And it's set next to each potential species.
# And maybe I'll even open it up here. In my right-hand toolbar, and click on Session.
# I'm gonna click on New Data. And I see that table.
# With 32.1 next to adelie, 32.2 next to adelie, 32.3 next to adelie. Scroll, scroll, scroll…
# That's a lot of scrolling. All the way to 59.6 next to adelie, every possible bill 
# length in my fake data set has been paired with adelie.
# And then, every possible bill length in my fake data set was paired with Gentoo, starting 
# with 32.1, that's a Gen 2 penguin. 32.2, that's a gentoo penguin. I could scroll down, and 
# then see that same list is applied to Chinstrap.

# So expand.grid takes every possible version of one column.
# And makes a row so that every unique combination with another column exists in the new dataset.
# Now, I can generate predictions here where Gentoo are predicted outside the bounds of 
# my Gentoo data, but within the bounds of all my Penguin data. I could just go from 0 to 100. 
# I could have made my sequence 0 to 100 by 1.
# But I didn't. I still wanted to stay within the bounds of my general Penguin data. 
# Okay, so I created some new data.

# Now I'm going to generate predictions using that predict function.
# We'll call it New Data. Predict LM3.
# It's gonna come from my predict function, just like before. I'm gonna feed it LM3, just like before.
# And this time… I'm gonna pass it the new data argument.
# We called it New Data.
# We just called it new data. That's kind of confusing. And I want those confidence intervals

newdata_predict_lm_3 = predict(lm_3, newdata = newdata, interval = "confidence")
head(newdata_predict_lm_3)

# now for that new data set, it gave me a fit, a lower, and an upper.
# I want to C-bind that with the original data that I… the fake data that I used,
#  that way I'm not lost into what these predictions are being generated from, like, 
# which X's. So now I'm going to go ahead and C-bind it.
# And, I'm gonna maybe… I'm just gonna C-bind it right here.
# C bind new data, Comma, my predictions.

newdata_predict_lm_3 = cbind(newdata, predict(lm_3, newdata = newdata, interval = "confidence"))

# So, for the fake bill length and fake species, here are the predictions.
# Now I'm gonna plot them. My points are gonna be from the original data.

# So the original data was from… Penguins LM3.
# The aesthetics are X equals bill length, Y equals build depth.
# Color equals species.
# GM line, this is gonna come from my new data predictions.
# which we called New Data.
# Predict.
# LM3.
# Aesthetics are X equals bill length, Y equals fit.
# Color equals… species.

ggplot() +
  geom_point(data = penguins_lm_3, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_line(data = newdata_predict_lm_3, aes(x = bill_length_mm, y = fit, color = species))
  geom_ribbon(data = newdata_predict_lm_3, aes(x = bill_length_mm, ymin = lwr, ymax = upr, fill = species),
              alpha = 0.5 )

# That looks good, and note that I have now predicted outside of the bounds of the range 
# for each species, so we've accomplished our goal here.

# So we've got our error ribbons, and this time we've generated predictions outside of the 
# bounds as needed.
# Had we… if you remember when we created the sequence.
# From the smallest bill length in our dataset to the largest for our fake bill length data, 
# I then said buy equals 0.1.
# if I… I could just generate ribbons with just the smallest and largest bill lengths.
# And that would create… A consistently wide box behind these points.
# But because I gave it routine areas to check on the size of the confidence intervals 
# across the full range of X's.
# that, that model had enough information to make these confidence intervals small, 
# where there's a lot of data to inform the model, and then wide again at the edges of 
# the range. 
# Otherwise, like, you could do a line with two points.
# This line would have looked the same if we just built it off of the smallest and largest beak length.
# But we fed it a lot of beak lengths so that we could see the shape in the confidence intervals underneath.

# Awesomeness with Basar. Now we're gonna do it one more time
# And this time we're gonna use some Tidyverse tools, which I like a little bit better.
# The tool that we're going to use is, once again, in the broom package that had the 
# tidy function that created that nice table of parameters from our model. Now we're 
# going to use the augment function. Augment is like the same thing as predict. 
# It's gonna generate predictions from our model, so we have to feed it a model.
# So, we're going to… Create a new dataset.
# I kind of ran out of names for these things.

# But we're gonna overwrite LM3Predict if we've already created it.
# And this time, we're gonna start with the model. Just like with the predict function, 
# the first parameter is the model. But Tidyverse, the dplyer, is all like, take that 
# first parameter that you'd feed a function, and pull it outside and pipe from it.

# Because it makes it feel more readable. So taking this model.
# that we've already created from BASAR, we're going to augment it. And augment 
# is in the broom package, so we've got to call it out specifically, because we haven't loaded the broom library.
# The data come from the data we created for the model.
# And now the parameters, of course, have to change a little to torture us.
# Now, if we want to generate confidence intervals, we've got to set SE underscore fit equals true.
# Interval equals confidence.

lm_3_predict = lm_3 %>%
  broom::augment(data = penguins_lm_3, se_fit = T, interval = "confidence", conf.level = 0.95)
head(lm_3_predict)
glimpse(lm_3_predict)

# Okay, this is pretty nice, because before, we generated our predictions in a table,
# and then we carefully used CBind to combine it with the data that created those predictions.
# So that's, like, one a little bit confusing step, and you… if you botch that, it's bad news. 
# If you… if you tell that the X's that use… were used to predict the Y's are the wrong X's, 
# you're going to get very confused in the next step.
# Here, Augment does that for you. It remembers the data, we sent it data as Penguins on 3,
# we sent it the model, and it goes ahead and it combines the predictions


# for a daily penguin with a bill length of 39.1, here is the predicted bill depth, and here 
# are the confidence intervals around that prediction. So, Augments did a lot of the work for us, 
# that feels pretty good.
# We could predict… we could plot it,
# I think it's really obvious how to plot it, so I might skip it, just the Y…
# is going to be dot fitted for your line, and the ribbon, the upper and lower parts of 
# the ribbon, are dot lower and dot upper, so the names change just a bit, because Augment 
# likes to use dots in front of their names.

# we're going to use another function in the Tidyverse called tidyR, which it 
# has a new version of expand.grid called expand.


# So, one more time, we're gonna generate some new data.
# We're gonna call it new data, and we're going to start with the original data, so this 
# is a different workflow compared to when we did it before. Start with the original data, 
# which knows all of the species and all of the bill lengths, and we're gonna say Tidy R,
# From that package, let's use expand, which purposefully sounds a lot like expand grid. That's on purpose.
# And we want bill length in millimeters. And species. Those are the two variables we 
# fed our model, so these are the ones where we want every possible combination of one to be 
# situated with every possible combination of the other.

newdata = penguins_lm_3 %>%
  tidyr::expand(bill_length_mm, species)
head(newdata)

# Now we can generate our predictions from this new data using the augment function from 
# the broom package. So, we're going to overwrite our LM3 predictions that we used for… 
# from base R, and this time, we're going to start with the model.

lm_3_predict = lm_3 %>% 
  broom::augment(newdata = newdata, se_fit = T, interval = "confidence", conf.int = 0.95)

# And we're gonna pipe to… the augment function in the broom package, and just like before.
# But this time, we're gonna send it new data. That new data we just created right up here.
# New data equals new data. To be clear.
# The first new data is the name of the parameter, the second new data is the name of this
# Objects that we just created in the last line of code.
# I thought it was helpful to make them the same, but it also could be confusing, too.
# And then, just like before, I want those confidence intervals, so I'm gonna take advantage 
# of those parameters that will calculate them for me.
# Is it conf.int equals 0.95?
# And let's see what happened. Head LM3 predict.

head(lm_3_predict)

# And now it took the new data that we created with the expand function in the broom package…
# And it augmented it with the predictions and the confidence intervals.
# And… Because we're 2 minutes past, I don't think we need to plot it, but it's exactly like before.
# Where now the full range of bill length is represented, instead of just the subset 
# relevant for each of the three species.
# So now we've built a model and generated our predictions, our own predictions, and 
# plotted them, and the reason for all this pain will become very clear in the next class when we 
# have more than one continuous variable in our model. 