library(car)
library(dplyr)
library(emmeans)
library(janitor)
library(ggplot2)
library(lmerTest)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

d <- read.csv("./data/hssq.csv") %>%
  clean_names() %>%
  rename(subject = participant_id)

d %>%
  ggplot()+
  geom_histogram(aes(x = symptoms))+
  facet_grid(~condition)

d %>%
  ggplot()+
  geom_histogram(aes(x = symptoms))

d %>%
  rename(y = symptoms) %>%
  group_by(condition) %>%
  summarise(mu = mean(y),
            sd = sd(y),
            med = median(y),
            q1 = quantile(y, prob = 0.25),
            q3 = quantile(y, prob = 0.75))

fit <- glmer(symptoms ~ condition + wbgt_avg + (1|subject),
          data = d,
          family = poisson(link = 'log'))

plot(fit)

anova(fit)
summary(fit)

emm <- emmeans(fit, pairwise ~ condition)
emm_observed <- regrid(emm, transform = "response")
emm_observed