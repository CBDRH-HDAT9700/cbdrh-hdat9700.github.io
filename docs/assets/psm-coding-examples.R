###########################################################
# MSc in Health Data Science
# HDAT9700 - Ch 2. Causal Inference from Observational Data
# Tutorial solutions
# Mark Hanly
# 6 Jun 2022
###########################################################


###################
# Practical 1: EDA
###################

library(MatchIt)
library(dplyr)
library(ggplot2)

data("lalonde")
View(lalonde)

lalonde %>% 
  group_by(treat) %>% 
  summarise(age = mean(age), educ = mean(educ))


library(compareGroups)
comp1 <- compareGroups::compareGroups(treat ~ age + educ + race + married + nodegree, data=lalonde)
comp1

##############################
# Practical 2: Match the data
##############################

m1 <- MatchIt::matchit(treat ~ age + educ + race + nodegree, 
                 method = 'nearest',
                 distance = 'glm', 
                 link = 'logit',
                 data = lalonde)


summary(m1)


###############################
# Practical 3: Compare balance
###############################

library(cobalt)

cobalt::bal.tab(m1)
cobalt::bal.plot(m1, var.name = 'race')
cobalt::love.plot(m1)

################################################ 
# Practical 4: Get estimate in the matched data
################################################ 

df1 <- MatchIt::match.data(m1)

lm(re78 ~ treat, data = df1, weights = weights)

lm(re78 ~ treat, data = lalonde)

