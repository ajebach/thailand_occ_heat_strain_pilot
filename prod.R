library(dplyr)
library(emmeans)
library(janitor)
library(ggplot2)
library(lmerTest)
library(car)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

d <- read.csv('./data/prod.csv') %>%
  clean_names()

#ABSOLUTE PRODUCTIVITY##########################################################
d %>%
  ggplot()+
  geom_histogram(aes(x = piles))+
  facet_grid(~condition)

fit <- lmer(piles ~ condition + wbgt_average + (1|subject),
            data = d)

qqPlot(residuals(fit))

anova(fit)
summary(fit)
confint(fit, level = 0.95)
confint(fit, level = 0.9)

emm <- emmeans(fit, pairwise ~ condition)
emm
confint(emm)

#PER HOUR#######################################################################
d %>%
  group_by(condition) %>%
  summarise(mean = mean(piles_per_hour_of_work),
            sd = sd(piles_per_hour_of_work))

fit <- lmer(piles_per_hour_of_work ~ condition + wbgt_average + (1|subject),
            data = d)

qqPlot(residuals(fit))

summary(fit)
confint(fit, level = 0.95)
confint(fit, level = 0.9)

emm <- emmeans(fit, pairwise ~ condition)
emm
confint(emm)

