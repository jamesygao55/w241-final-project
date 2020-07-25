# setwd("/Users/jamesgao/MIDS/w241/final-project")


library(data.table)
library(dplyr)
library(ggplot2)
library(stargazer)
library(lmtest)
library(sandwich)
library(foreign)
library(knitr)



d <- fread("data_as_of_july_25.csv")

setnames(d, "Timestamp", "timestamp")
setnames(d, "Name", "name")
setnames(d, "Gender", "gender")
setnames(d, "Age", "age")
setnames(d, "Music Training", "music_training")
setnames(d, "Pre-treatment Score", "pre_treatment_score")
setnames(d, "Post-treatment Score", "post_treatment_score")
setnames(d, "Treatment", "treatment")
setnames(d, "Email", "email")
d[ , score_difference := post_treatment_score - pre_treatment_score]


model_post_treatment_score <- lm(post_treatment_score ~ treatment, data = d)
model_score_difference <- lm(score_difference ~ treatment, data = d)

summary(model_post_treatment_score)
summary(model_score_difference)




## Gender Covariate
# post-treatment score only
model_post_treatment_score_gender <- lm(post_treatment_score ~ treatment + gender + treatment*gender, data = d)
summary(model_post_treatment_score_gender)


# difference in scores
model_score_difference_gender <- lm(score_difference ~ treatment + gender + treatment*gender, data = d)
summary(model_score_difference_gender)



