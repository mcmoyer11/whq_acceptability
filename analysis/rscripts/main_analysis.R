#Boxes follow-ups
#last edit: 7/6/17

library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(bootstrap)
library(lme4)

#Set the working directory to whereever you have your raw data and the "helpers.R" file
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
source("../../helpers.R")
################################################################
################################################################
# this is where it starts

tot <- read.csv("../data/tot_nat.csv")
tot <- droplevels(tot)
#View(tot)

#b <- subset(tot, tot$response=="")
# drop all the empty values
total = tot %>%
  drop_na()

View(total)

whooo = subset



# Running mixed effects models

m = glm(response ~ context+tense, data = total, family=binomial)
summary(m)

confint(m)

predict(m, type="response")

m2 = glm(response~context+tense+wh+mat_verb, data = tot, family=binomial)
summary(m2)

# the smaller AIC/BIC the better
anova(m)
AIC(m) #6158.916
BIC(m) #6193.091
anova(m2)
AIC(m2) #6040.557
BIC(m2) #6088.402

anova(m,m2)

# non-parametric pairwise comparisons.

kruskal.test(response ~ list, data = total)
# Kruskal-Wallis chi-squared = 53.657, df = 7, p-value = 2.749e-09
kruskal.test(response ~ subject, data = total)
# Kruskal-Wallis chi-squared = 471.86, df = 214, p-value < 2.2e-16


interAB = interaction(total$mat_verb, total$tense)
kruskal.test(response ~ interAB, data = total)
# Kruskal-Wallis chi-squared = 118.66, df = 3, p-value < 2.2e-16***

# Testing for effects of particular trials
# across all contexts
kruskal.test(response ~ emb_verb, data = total)
# Kruskal-Wallis chi-squared = 22.034, df = 15, p-value = 0.1069
# across mst alone
mst = subset(total, total$context=="MST")
kruskal.test(response ~ emb_verb, data = mst)
# Kruskal-Wallis chi-squared = 29.609, df = 15, p-value = 0.01341*
# TOTAL
amst = mst %>%
  group_by(emb_verb,tense,wh) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

#plot the overall data
ggplot(amst, aes(emb_verb,y=Proportion_yes,fill=tense)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~wh) +
  # scale_fill_grey() +
  ggtitle(label = "Who data") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

View(mst)
view = mst %>%
  filter(emb_verb %in% c("view")) 
  # filter(tense %in% c("finite"))
kruskal.test(response ~ tense, data = view)

# between the two verbs
# Kruskal-Wallis chi-squared = 0.11422, df = 1, p-value = 0.7354
# between tense
# Kruskal-Wallis chi-squared = 6.4996, df = 1, p-value = 0.01079
hide = mst %>%
  filter(emb_verb %in% c("hide")) %>%
  filter(tense %in% c("nonfinite"))
kruskal.test(response ~ mat_verb, data = view)
length(unique(view$subject)) # 107
wilcox.test(response ~ tense, data=view)
# mat_verb across tense
# Kruskal-Wallis chi-squared = 0.03492, df = 1, p-value = 0.8518
# tense across `predict`
# Kruskal-Wallis chi-squared = 6.4996, df = 1, p-value = 0.01079
# mat_verb across nonfinit
# Kruskal-Wallis chi-squared = 0.03492, df = 1, p-value = 0.8518

ask = mst %>%
  filter(emb_verb %in% c("ask")) %>%
  filter(tense %in% c("nonfinite"))
kruskal.test(response ~ mat_verb, data = ask)
# between tense in predict
# Kruskal-Wallis chi-squared = 1.3489, df = 1, p-value = 0.2455
# between mat_verb in finite
# Kruskal-Wallis chi-squared = 6.8303, df = 1, p-value = 0.008962
# mat_verb in nonfinite
# Kruskal-Wallis chi-squared = 1, df = 1, p-value = 0.3173

call = mst %>%
  filter(emb_verb %in% c("call")) %>%
  filter(mat_verb %in% c("predict"))
kruskal.test(response ~ tense, data = call)
# Kruskal-Wallis chi-squared = 1.4312, df = 1, p-value = 0.2316

hire = mst %>%
  filter(emb_verb %in% c("hire")) %>%
  filter(tense %in% c("finite"))
kruskal.test(response ~ mat_verb, data = hire)
# in nonfinite
# Kruskal-Wallis chi-squared = 3.1826, df = 1, p-value = 0.07443
# in finite
# Kruskal-Wallis chi-squared = 1.9726, df = 1, p-value = 0.1602

interview = mst %>%
  filter(emb_verb %in% c("interview")) %>%
  filter(tense %in% c("nonfinite"))
kruskal.test(response ~ mat_verb, data = interview)
# mat_verb in nonfinite
# Kruskal-Wallis chi-squared = 9.4623, df = 1, p-value = 0.002097


sell = mst %>%
  filter(emb_verb %in% c("sell")) %>%
  filter(tense %in% c("finite"))
kruskal.test(response ~ mat_verb, data = sell)
# Kruskal-Wallis chi-squared = 2.0671, df = 1, p-value = 0.1505
length(unique(sell$subject)) # 55 subjects
# testing difference from chance
prop.test(23, 33, p = 0.5, alternative = "two.sided", correct = FALSE)
# none of the values are significantly different from each other OR from chance

sell_y = sell %>%
  filter(response %in% c("1"))
summary(sell_y)
# know_prop_yes = .3
# predict_prop_yes = .48

tott_sell = total %>%
  filter(emb_verb %in% c("sell"))
# Question: how did sell do in the 
tot_sell = total %>%
  filter(emb_verb %in% c("sell")) %>%
  filter(tense %in% c("nonfinite")) %>%
  group_by(context,tense, mat_verb) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

View(tott_sell)

ggplot(tot_sell, aes(context,y=Proportion_yes,fill=mat_verb)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  # facet_wrap(~mat_verb) +
  # scale_fill_grey() +
  ggtitle(label = "Responses for non-finite 'V-where-sell'") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

# are the MST and MAT bars significantly different from each other?
# Answer: no
nonf_pred_sell = tott_sell %>%
  filter(mat_verb %in% c("know")) %>%
  filter(tense %in% c("nonfinite")) %>%
  filter(context %in% c("MAT", "MST"))
kruskal.test(response ~ context, data = nonf_pred_sell)
# Kruskal-Wallis chi-squared = 2.1176, df = 1, p-value = 0.1456 (predict)
# Kruskal-Wallis chi-squared = 0.80645, df = 1, p-value = 0.3692 (know)

# kruskal.test(response ~ emb_verb, data = f)
# Kruskal-Wallis chi-squared = 23.987, df = 15, p-value = 0.06531
kruskal.test(response ~ emb_verb, data = nf)
# Kruskal-Wallis chi-squared = 27.564, df = 15, p-value = 0.02446

interWHEV = interaction(mst$emb_verb, mst$wh)
kruskal.test(response ~ interWHEV, data = mst)
# ruskal-Wallis chi-squared = 29.609, df = 15, p-value = 0.01341

who = subset(mst, mst$wh=="who")
kruskal.test(response ~ emb_verb, data = who)
# Across TOTAL: Kruskal-Wallis chi-squared = 8.0181, df = 7, p-value = 0.331
# Across MST: Kruskal-Wallis chi-squared = 8.5137, df = 7, p-value = 0.2895

interMFwho = interaction(who$tense, who$mat_verb)
kruskal.test(response ~ interMFwho, data = who)
# Kruskal-Wallis chi-squared = 140.82, df = 3, p-value < 2.2e-16

kruskal.test(resp)

where = subset(mst, mst$wh=="where")
kruskal.test(response ~ emb_verb, data = where)
# Across TOTAL: Kruskal-Wallis chi-squared = 4.2908, df = 7, p-value = 0.7457
# Across MST: Kruskal-Wallis chi-squared = 15.09, df = 7, p-value = 0.03486

interMFwhere = interaction(where$tense, where$mat_verb)
kruskal.test(response ~ interMFwhere, data = where)
# Kruskal-Wallis chi-squared = 106.01, df = 3, p-value < 2.2e-16

# TOTAL
awho = who %>%
  group_by(context,emb_verb,tense) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

#plot the overall data
ggplot(awho, aes(context,y=Proportion_yes,fill=tense)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~emb_verb) +
  scale_fill_grey() +
  ggtitle(label = "Who data") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)


# MST
amstwho = who %>%
  group_by(emb_verb,tense, mat_verb) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

#plot the overall data
ggplot(amstwho, aes(tense,y=Proportion_yes,fill=mat_verb)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~emb_verb) +
  # scale_fill_grey() +
  ggtitle(label = "Who-questions") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

pairwise.wilcox.test(who$response, who$emb_verb, p.adjust.method = "BH")
interAB = interaction(total$context, total$tense)
kruskal.test(response ~ interAB, data = total)
# Kruskal-Wallis chi-squared = 225.4, df = 31, p-value < 2.2e-16

interAB = interaction(total$emb_verb, total$tense)
kruskal.test(response ~ interAB, data = total)
# Kruskal-Wallis chi-squared = 94.754, df = 31, p-value = 2.232e-08

##########################################
##########################################
# TOTAL
amstwhere = where %>%
  group_by(emb_verb,tense, mat_verb) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

#plot the overall data
ggplot(amstwhere, aes(tense,y=Proportion_yes,fill=mat_verb)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~emb_verb) +
  # scale_fill_grey() +
  ggtitle(label = "Where-questions") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

# MST
awhere = where %>%
  group_by(context,emb_verb,tense) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

#plot the overall data
ggplot(awhere, aes(context,y=Proportion_yes,fill=tense)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~emb_verb) +
  scale_fill_grey() +
  ggtitle(label = "Where data") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)




##########################################



####################################
#for the TOTAL data
####################################
agr = total %>%
  group_by(context) %>%
  summarize(prop_yes = mean(as.numeric(response)))

# for looking at tense
#aggregating the data 
aggr = total %>%
  group_by(context,tense) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

#plot the overall data
ggplot(aggr, aes(context,y=Proportion_yes,fill=tense)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  #facet_wrap(~mat_verb) +
  scale_fill_grey() +
  ggtitle(label = "Total data") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

####################################################
################ mat_verb ##########################
#aggregating the data 
a = total %>%
  group_by(context,mat_verb) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

#plot the overall data
ggplot(a, aes(context,y=Proportion_yes,fill=mat_verb)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  #facet_wrap(~mat_verb) +
  scale_fill_grey() +
  ggtitle(label = "Total data") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

####################################################
# ################ wh ##############################
#aggregating the data 
w = total %>%
  group_by(context,wh) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

#plot the overall data
ggplot(w, aes(context,y=Proportion_yes,fill=wh)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  #facet_wrap(~mat_verb) +
  scale_fill_grey() +
  ggtitle(label = "Total data") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

####################################################
################ emb_verb ##########################
#aggregating the data 
ev = total %>%
  group_by(emb_verb, context) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

head(ev)

#plot the overall data
ggplot(ev, aes(context,y=Proportion_yes)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~emb_verb) +
  scale_fill_grey() +
  ggtitle(label = "Total data emb_verb") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

kruskal.test(response ~ emb_verb, data = total)
# data:  response by emb_verb
# Kruskal-Wallis chi-squared = 22.034, df = 15, p-value = 0.1069

pairwise.wilcox.test(total$response, total$emb_verb, p.adjust.method = "BH")
# None of them are significant.

####################################################
################ MST ###############################
total_mst <- subset(total, total$context=="MST")

#aggregating the data 
tmst = total_mst %>%
  group_by(mat_verb,wh,tense) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

#plot the overall data
ggplot(tmst, aes(tense,y=Proportion_yes,fill=wh)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~mat_verb) +
  scale_fill_grey() +
  ggtitle(label = "Mention-some context") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)


kruskal.test(response ~ mat_verb, data = total_mst)
# Kruskal-Wallis chi-squared = 156.7, df = 1, p-value < 2.2e-16

pairwise.wilcox.test(total_mst$response, total_mst$mat_verb, p.adjust.method = "BH")

# Interaction?
interCSC <- interaction(total_mst$mat_verb, total_mst$tense)
kruskal.test(response~interCSC, data = total_mst)
# Kruskal-Wallis chi-squared = 244.78, df = 3, p-value < 2.2e-16

####################################
# Looking at emb_verb in MST

tmstev = total_mst %>%
  group_by(emb_verb,mat_verb,tense) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

#plot the overall data
ggplot(tmstev, aes(mat_verb,y=Proportion_yes,fill=tense)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~emb_verb) +
  scale_fill_grey() +
  ggtitle(label = "Embedded Verbs in Mention-some context") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

kruskal.test(response ~ emb_verb, data = total_mst)
# Kruskal-Wallis chi-squared = 29.609, df = 15, p-value = 0.01341

pairwise.wilcox.test(total_mst$response, total_mst$emb_verb, p.adjust.method = "BH")

####################################
# Participant distributions in MST
kruskal.test(response ~ subject, data = total)


####################################
####################################

# for looking at mst context and embedded verbs
#aggregating the data 
tmst = total_mst %>%
  group_by(mat_verb,tense, emb_verb) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

#plot the overall data
ggplot(tmst, aes(tense,y=Proportion_yes,fill=mat_verb)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~emb_verb) +
  scale_fill_grey() +
  ggtitle(label = "Mention-some context") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

####################################

# for looking at matf context
total_matf <- subset(total, total$context=="MATF")
#aggregating the data 
tmatf = total_matf %>%
  group_by(mat_verb,wh,tense) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

#plot the overall data
ggplot(tmatf, aes(tense,y=Proportion_yes,fill=wh)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~mat_verb) +
  scale_fill_grey() +
  ggtitle(label = "Mention-All T+F context") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

####################################

# for looking at matf and embedded verb
total_matf = droplevels(total_matf)
#aggregating the data 
agr_matf = total_matf %>%
  group_by(mat_verb,tense, emb_verb) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

#plot the overall data
ggplot(agr_matf, aes(tense,y=Proportion_yes,fill=mat_verb)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~emb_verb) +
  scale_fill_grey() +
  ggtitle(label = "Mention-Al T+F context") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

####################################
#for the TOTAL factors in MS only
####################################

#subset to MS context
ms_all_facts <- subset(total, total$context=="MST")

# for looking at tense
#aggregating the data 
agr = ms_all_facts %>%
  group_by(wh,tense,mat_verb) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

#View(aggr)

#plot the overall data
ggplot(agr, aes(tense,y=Proportion_yes, fill=wh)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~mat_verb) +
  scale_fill_grey() +
  ggtitle(label = "Mention-Some Context") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

summary(total)


###################################################
###### Nonparametric t-tests for whole data
###################################################
kruskal.test(tense ~ response, data = total) #SIGNIFICANT
#Kruskal-Wallis chi-squared = 43.567, df = 1, p-value = 4.097e-11
kruskal.test(context ~ response, data = total) #SIGNIFICANT
#Kruskal-Wallis chi-squared = 823.42, df = 1, p-value < 2.2e-16
kruskal.test(mat_verb ~ response, data = total) #significant
#Kruskal-Wallis chi-squared = 53.714, df = 1, p-value = 2.319e-13
kruskal.test(wh ~ response, data = total) #significant
#Kruskal-Wallis chi-squared = 9.7095, df = 1, p-value = 0.001833

pred <- subset(total, total$mat_verb=="predict")
kruskal.test(tense ~ response, data = pred)
# chi-squared = 2.0009, df = 1, p-value = 0.1572

know <- subset(total, total$mat_verb=="know")
kruskal.test(tense ~ response, data = know)
#chi-squared = 62.176, df = 1, p-value = 3.142e-15

###################################################
###### Nonparametric t-tests for MS whole data
###################################################
total_mst <- subset(total, total$context=="MST")
kruskal.test(tense ~ response, data = total_mst) #***significant
#Kruskal-Wallis chi-squared = 156.7, df = 1, p-value < 2.2e-16
kruskal.test(mat_verb ~ response, data = total_mst) #SIG
#Kruskal-Wallis chi-squared = 18.892, df = 1, p-value = 1.383e-05
kruskal.test(wh ~ response, data = total_mst) #SIG
#Kruskal-Wallis chi-squared = 6.6105, df = 1, p-value = 0.01014

mst_know <- subset(total_mst, total_mst$mat_verb=="know")
kruskal.test(tense ~ response, data = mst_know)
#chi-squared = 192.86, df = 1, p-value < 2.2e-16

mst_predict <- subset(total_mst, total_mst$mat_verb=="predict")
kruskal.test(tense ~ response, data = mst_predict)
#chi-squared = 10.385, df = 1, p-value = 0.001271

###################################################
###### Nonparametric t-tests for MATF whole data
###################################################

total_matf <- subset(total, total$context=="MATF")
kruskal.test(tense ~ response, data = total_matf) # significant
#Kruskal-Wallis chi-squared = 9.513, df = 1, p-value = 0.00204
kruskal.test(mat_verb ~ response, data = total_matf) #totally significant
#Kruskal-Wallis chi-squared = 51.731, df = 1, p-value = 6.364e-13
kruskal.test(emb_verb ~ response, data = total_matf) #NOT significant across verbs
#Kruskal-Wallis chi-squared = 0.007113, df = 1, p-value = 0.9328
kruskal.test(wh ~ response, data = total_matf) # significant
#Kruskal-Wallis chi-squared = 30.219, df = 1, p-value = 3.859e-08


# t-tests for WH in MATF_know
total_matf_know = subset(total_matf, total_matf$mat_verb=="know")
kruskal.test(tense ~ response, data = total_matf_know) # NOT SIGNIFICANT
#2.0113, df = 1, p-value = 0.1561
kruskal.test(emb_verb ~ response, data = total_matf_know) #NOT Significant
#1.6626, df = 1, p-value = 0.1972

total_matf_know_finite = subset(total_matf_know, total_matf_know$tense=="finite")
kruskal.test(wh ~ response, data = total_matf_know_finite) # significant!!
#12.908, df = 1, p-value = 0.0003271

total_matf_know_nonfinite = subset(total_matf_know, total_matf_know$tense=="nonfinite")
kruskal.test(wh ~ response, data = total_matf_know_nonfinite) #SIGNIFICANT
#7.3741, df = 1, p-value = 0.006617

# TESTS FOR WH in MATF_PREDICT
total_matf_predict = subset(total_matf, total_matf$mat_verb=="predict")

kruskal.test(tense ~ response, data = total_matf_predict) # NOT significant
#8.798, df = 1, p-value = 0.003016

kruskal.test(wh ~ response, data = total_matf_predict) #SIGNIFICANT
#Kruskal-Wallis chi-squared = 12.15, df = 1, p-value = 0.0004909
total_matf_predict_finite = subset(total_matf_predict, total_matf_predict$tense=="finite")
kruskal.test(wh ~ response, data = total_matf_know_finite) # significant!!
#Kruskal-Wallis chi-squared = 12.908, df = 1, p-value = 0.0003271

total_matf_predict_nonfinite = subset(total_matf_predict, total_matf_predict$tense=="nonfinite")
kruskal.test(wh ~ response, data = total_matf_predict_nonfinite) #SIGNIFICANT
#Kruskal-Wallis chi-squared = 6.2815, df = 1, p-value = 0.0122


# TESTS FOR TENSE IN MATF
kruskal.test(tense ~ response, data = total_matf) # SIGNIFICANT
#chi-squared = 9.513, df = 1, p-value = 0.00204
kruskal.test(mat_verb ~ response, data = total_matf) #SIGNIFIFCANT
#chi-squared = 51.731, df = 1, p-value = 6.364e-13



# for looking at matf and embedded verb for predict_nonfinte

#aggregating the data 
agr_matf_p = total_matf_predict %>%
  group_by(mat_verb,tense,wh,emb_verb) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

#plot the overall data
ggplot(agr_matf_p, aes(tense,y=Proportion_yes,fill=tense)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~emb_verb) +
  scale_fill_grey() +
  ggtitle(label = "Mention-All T+F predict") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)




####################################
#for the non-finite condition
####################################

#aggregating the data 
agr = d %>%
  group_by(context,mat_verb,wh) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

#plot the overall data
ggplot(agr, aes(context,y=Proportion_yes,fill=wh)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~mat_verb) +
  scale_fill_grey() +
  ggtitle(label = "Non-Finite Condition") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)


####################################
#for the FINITE condition
####################################

#aggregating the data 
ag = nm %>%
  group_by(context,mat_verb,wh) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

ag = droplevels(ag)
#plot the overall data
ggplot(ag, aes(context,y=Proportion_yes,fill=wh)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~mat_verb) +
  scale_fill_grey() +
  ggtitle(label = "Finite Condition") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)



###################################################
###### T-tests for FINITE condition
###################################################


#getting the data into subset to run some pairwise comparisons
f_mst <- subset(nm,nm$context=="mst")
head(f_mst)
f_mst = droplevels(f_mst)

know_f_mst <- subset(f_mst,f_mst$mat_verb=="know")
predict_f_mst <- subset(f_mst,f_mst$mat_verb=="predict")

kruskal.test(data = f_mst)

#run a non-parametric test
kruskal.test(wh ~ response, data = f_mst) #not significant
kruskal.test(wh ~ response, data = know_f_mst) #not significant
kruskal.test(wh ~ response, data = predict_f_mst) #not significant

#in MST, know vs. predict
kruskal.test(mat_verb ~ response, data = f_mst) #SIGNIFICANT

#checking out MATF context
f_matf <- subset(nm,nm$context=="matf")
know_f_matf <- subset(f_matf,f_matf$mat_verb=="know")
predict_f_matf <- subset(f_matf,f_matf$mat_verb=="predict")

kruskal.test(wh ~ response, data = f_matf) #SIGNIFICANT
kruskal.test(mat_verb ~ response, data = f_matf) #SIGNIFICANT 
kruskal.test(wh ~ response, data = know_f_matf) #SIGNIFICANT
kruskal.test(wh ~ response, data = predict_f_matf) #NOT significant








###################################################
###### Mixed effects Model
###################################################


#here's how to run the lmer model, but there is not enough data yet (as of 7/17).
m = glmer(resp ~ context*wh + (1|subject),family="binomial",data=d)
summary(m)



###########################################
# take a look at the data just in the MST
###########################################

mst <- subset(d,d$context=="mst") 

#look at who
mst_who <- subset(mst,mst$wh=="who")
mst_who = droplevels(mst_who)
View(mst_who)

mst_agrwho = mst_who %>%
  group_by(emb_verb,mat_verb) %>%
  summarise(Proportion = mean(resp), CILow = ci.low(resp), CIHigh = ci.high(resp)) %>%
  mutate(YMin = Proportion - CILow, YMax = Proportion + CIHigh)
dodge = position_dodge(.9)

who = ggplot(mst_agrwho, aes(emb_verb,y=Proportion,fill=mat_verb)) +
  geom_bar(position=dodge,stat="identity") + 
  scale_fill_grey()

#look at where
mst_where <- subset(mst,mst$wh=="where")
mst_where = droplevels(mst_where)

mst_agrwhere = mst_where %>%
  group_by(emb_verb,mat_verb) %>%
  summarise(Proportion = mean(resp), CILow = ci.low(resp), CIHigh = ci.high(resp)) %>%
  mutate(YMin = Proportion - CILow, YMax = Proportion + CIHigh)
dodge = position_dodge(.9)

where = ggplot(mst_agrwhere, aes(emb_verb,y=Proportion,fill=mat_verb)) +
  geom_bar(position=dodge,stat="identity") + 
  scale_fill_grey()

