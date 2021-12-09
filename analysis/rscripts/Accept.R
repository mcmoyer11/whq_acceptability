library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(bootstrap)
source("code_sheets/helpers.R")

#Set the working directory to whereever you have your raw data and the "helpers.R" file
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

#Import the data into R (make sure it's a CSV for easy reading), and assign it to a variable so you can refer to it later in the file.
d = read.csv("data/accep_raw.csv")
#view the first 6 rows of the data file, so you know everything is OK
head(d)
# View the whole data file
View(d)

#this is for getting the age into the right kind of form
age = read.table("data/accept_age.txt",sep="\t",header=T)
head(age)
table(age$age1)
age$NewAge = age$age1
age$NewAge = as.character(age$NewAge)
age[age$age1 == "22-24",]$NewAge = 23
age[age$age1 == "60+",]$NewAge = 60
age[age$age1 == "over 24",]$NewAge = 24
age$NewAge = as.numeric(age$NewAge)
summary(age)

#merging age data with the larger data set
age = age[,c("subject","NewAge")]
nage = age %>%
  filter(as.character(subject) %in% levels(d$subject))
age = unique(droplevels(age)) 
d = merge(d,age,by="subject",all.x=T)
nrow(d)
head(d)
summary(d)

#aggregating the data into how 
agr = d %>%
  group_by(verb,report,wh) %>%
  summarise(Proportion = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion - CILow, YMax = Proportion + CIHigh)
dodge = position_dodge(.9)

ggplot(agr, aes(report,y=Proportion,fill=wh)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~verb)

new = d %>%
  filter(report %in% c("ms","t+f") & verb == "know")
new = droplevels(new)
nrow(new)
str(new) #this gives you all the factors and tells you what kind of object they are.
#we need to change the response into a factor and not an integer
new$fresponse = as.factor(as.character(new$response))



m = glmer(fresponse ~ report*wh + (1|subject),family="binomial",data=new)
summary(m)

#we have to center the factors
centered = cbind(new,myCenter(new[,c("report","wh", "NewAge")]))
head(centered)
summary(centered)
str(centered)

#report and wh interaction, and random by-subject effects
m = glmer(fresponse ~ creport*cwh + (1|subject),family="binomial",data=centered)
summary(m)

#simple effect 
m.simple = glmer(fresponse ~ report*wh - wh + (1|subject),family="binomial",data=centered)
summary(m.simple)

#age effect
m = glmer(fresponse ~ creport*cwh*cNewAge + (1|subject),family="binomial",data=centered)
summary(m)

m = glmer(fresponse ~ creport*cwh + cNewAge + cwh:cNewAge + (1|subject),family="binomial",data=centered)
summary(m)

new$BinnedAge = cut(new$NewAge,2)
summary(new)

agr = new %>%
  group_by(BinnedAge,report,wh) %>%
  summarise(Proportion = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion - CILow, YMax = Proportion + CIHigh)
dodge = position_dodge(.9)

ggplot(agr, aes(report,y=Proportion,fill=wh)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~BinnedAge)
