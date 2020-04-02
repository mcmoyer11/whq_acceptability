#Boxes follow-ups
#last edit: 7/6/17

library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(bootstrap)
#library(lme4)

#Set the working directory to whereever you have your raw data and the "helpers.R" file
setwd("/Users/morganmoyer/Dropbox/Moyer_research/Follow-ups/Data/")
source("helpers.R")

#Import the data into R (make sure it's a CSV for easy reading), and assign it to a variable so you can refer to it later in the file.
d = read.csv("non-finite_rawr.csv")
unique(d$subject)  # 67 participants without removing contols 

d = read.csv("non-finite_rawr.csv")
#how many subjects
unique(d$subject)


m = read.csv("finite_rawr.csv")
unique(m$subject) # 52 without removing controls
head(m)

#add a new column to say which tense condition
d <- cbind(d, "tense" = c("nonfinite"))
m <- cbind(m, "tense" = c("finite"))

#remove non-natives speakers for finite 
nm = subset(m,m$l1=="EN")
unique(nm$subject) # now only 28

#combine the two together for later comparison
total <- rbind(nm, d)
head(total)

#take a look
head(new)
tail(new)
nrow(new)

####################################
#for the TOTAL data
####################################

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

# for looking at mat_verb
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

# for looking at wh
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


# for looking at mst context
total_mst <- subset(total, total$context=="mst")
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


# for looking at matf context
total_matf <- subset(total, total$context=="matf")
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

# for looking at matf context by mat_verb
total_matf <- subset(total, total$context=="matf")
#aggregating the data 
tmatfmv = total_matf %>%
  group_by(mat_verb,wh,tense) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

#plot the overall data
ggplot(tmatfmv, aes(tense,y=Proportion_yes,fill=wh)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~mat_verb) +
  scale_fill_grey() +
  ggtitle(label = "Mention-All T+F context") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)


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




###################################################
###### Nonparametric t-tests for whole data
###################################################
kruskal.test(tense ~ response, data = total) #SIGNIFICANT
kruskal.test(context ~ response, data = total) #SIGNIFICANT
kruskal.test(mat_verb ~ response, data = total) #significant
kruskal.test(wh ~ response, data = total) #significant

total_mst <- subset(total, total$context=="mst")
kruskal.test(tense ~ response, data = total_mst)
tmk <- subset(total_mst, total_mst$mat_verb=="know")
tmkf <- subset(tmk, tmk$tense=="finite")

kruskal.test(wh ~ response, data = tmkf)


kruskal.test(tense ~ response, data = total) #SIGNIFICANT
kruskal.test(context ~ response, data = total) #SIGNIFICANT


###################################################
###### Nonparametric t-tests for MATF whole data
###################################################

total_matf <- subset(total, total$context=="matf")
kruskal.test(tense ~ response, data = total_matf)
kruskal.test(emb_verb ~ response, data = total_matf) #NOT significant across verbs

# TTESTS FOR WH in MATF
total_matf = subset(total, total$context=="matf")
kruskal.test(wh ~ response, data = total_matf) # significant

# t-tests for WH in MATF_know
total_matf_know = subset(total_matf, total_matf$mat_verb=="know")
kruskal.test(tense ~ response, data = total_matf_know) # NOT SIGNIFICANT
kruskal.test(emb_verb ~ response, data = total_matf_know) #NOT Significant

total_matf_know_finite = subset(total_matf_know, total_matf_know$tense=="finite")
kruskal.test(wh ~ response, data = total_matf_know_finite) # significant!!

total_matf_know_nonfinite = subset(total_matf_know, total_matf_know$tense=="nonfinite")
kruskal.test(wh ~ response, data = total_matf_know_nonfinite) #SIGNIFICANT

# TESTS FOR WH in MATF_PREDICT
total_matf_predict = subset(total_matf, total_matf$mat_verb=="predict")

kruskal.test(tense ~ response, data = total_matf_predict) # NOT significant

kruskal.test(wh ~ response, data = total_matf_predict) #SIGNIFICANT
total_matf_predict_finite = subset(total_matf_predict, total_matf_predict$tense=="finite")
kruskal.test(wh ~ response, data = total_matf_know_finite) # significant!!

total_matf_predict_nonfinite = subset(total_matf_predict, total_matf_predict$tense=="nonfinite")
kruskal.test(wh ~ response, data = total_matf_predict_nonfinite) #SIGNIFICANT


# TESTS FOR TENSE IN MATF
kruskal.test(tense ~ response, data = total_matf) # MARGINALLY SIGNIFICANT
kruskal.test(mat_verb ~ response, data = total_matf) #SIGNIFIFCANT


#testing EMB_VERB for trial differences
kruskal.test(emb_verb ~ response, data = total_matf_know) #NOT SIGNIFICANT
kruskal.test(emb_verb ~ response, data = total_matf_know_finite) #NOT SIGNIFICANT
kruskal.test(emb_verb ~ response, data = total_matf_know_nonfinite) #NOT SIGNIFICANT
kruskal.test(emb_verb ~ response, data = total_matf_predict) #***SIGNIFICANT
kruskal.test(emb_verb ~ response, data = total_matf_predict_finite) #NOT SIGNIFICANT
kruskal.test(emb_verb ~ response, data = total_matf_predict_nonfinite) #***SIGNIFICANT



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

###################################################
###### Nonparametric t-tests for whole data
###################################################
total_mst
kruskal.test(tense ~ response, data = total) #SIGNIFICANT
kruskal.test(context ~ response, data = total) #SIGNIFICANT

# TTESTS FOR WH in MATF
total_matf = subset(total, total$context=="matf")
kruskal.test(wh ~ response, data = total_matf) # significant

# t-tests for WH in MATF_know
total_matf_know = subset(total_matf, total_matf$mat_verb=="know")
kruskal.test(tense ~ response, data = total_matf_know) # NOT SIGNIFICANT

total_matf_know_finite = subset(total_matf_know, total_matf_know$tense=="finite")
kruskal.test(wh ~ response, data = total_matf_know_finite) # significant!!

total_matf_know_nonfinite = subset(total_matf_know, total_matf_know$tense=="nonfinite")
kruskal.test(wh ~ response, data = total_matf_know_nonfinite) #SIGNIFICANT

# TESTS FOR WH in MATF_PREDICT
total_matf_predict = subset(total_matf, total_matf$mat_verb=="predict")

kruskal.test(tense ~ response, data = total_matf_predict) # NOT significant

kruskal.test(wh ~ response, data = total_matf_predict) #SIGNIFICANT
total_matf_predict_finite = subset(total_matf_predict, total_matf_predict$tense=="finite")
kruskal.test(wh ~ response, data = total_matf_know_finite) # significant!!

total_matf_predict_nonfinite = subset(total_matf_predict, total_matf_predict$tense=="nonfinite")
kruskal.test(wh ~ response, data = total_matf_predict_nonfinite) #SIGNIFICANT


# TESTS FOR TENSE IN MATF
kruskal.test(tense ~ response, data = total_matf) # MARGINALLY SIGNIFICANT
kruskal.test(mat_verb ~ response, data = total_matf) #SIGNIFIFCANT

###################################################
###### Nonparametric t-tests for non-finite
###################################################
head(d)
str(d)
kruskal.test(context ~ response, data = d)

#getting the data into subset to run some pairwise comparisons
mst <- subset(d,d$context=="mst")
#View(mst)
mst = droplevels(mst)

know_mst <- subset(mst,mst$mat_verb=="know")
predict_mst <- subset(mst,mst$mat_verb=="predict")

#check out what each type of object the factors are
#str(pwc_mst)

#run a non-parametric test
kruskal.test(wh ~ response, data = mst)
kruskal.test(mat_verb ~ response, data = mst)
kruskal.test(wh ~ response, data = know_mst)
kruskal.test(wh ~ response, data = predict_mst)

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

