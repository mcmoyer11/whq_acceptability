#TVJT

library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(bootstrap)

#Set the working directory to whereever you have your raw data and the "helpers.R" file
setwd("/Users/morganmoyer/Dropbox/mem_tutorial-master/")
source("code_sheets/helpers.R")

#Import the data into R (make sure it's a CSV for easy reading), and assign it to a variable so you can refer to it later in the file.
d = read.csv("data/tvjtraw2.csv")
#view the first 6 rows of the data file, so you know everything is OK
tail(d)
# View the whole data file
View(d)


d=droplevels(d)
# head(d)
# View(d)

# d_straw <- subset(d,d$trial=="strawberries")
# View(d_straw)
# View(d)

d = d %>%
  mutate(noun = case_when(
    noun == "bp" ~ "BP",
    noun == "dd" ~ "PDD"  
  )) %>%
  mutate(report = case_when(
    report == "fc" ~ "cont_n",
    report == "ms" ~ "target1",
    report == "t+f" ~ "target2",
    report == "tc" ~ "cont_y"
  ))


d_new <- subset(d, d$report!="target2")
# View(d_new)
#d_broc <- subset(d_new, d$trial=="strawberries")
#View(d_broc)


#STATS

d_tar2 <- subset(d, d$report=="target2")
View(d_tar2)
#aggregating the data into how 

agr = d_new %>%
  #filter(age %in% c("child")) %>%
  drop_na() %>%
  group_by(report,noun,age) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)
# View(agr)

ggplot(agr, aes(report,y=Proportion_yes,fill=noun)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_fill_grey() +
  facet_wrap(~age) +
  # theme(text = element_text(size=16)) +
  ylim(0,1) +
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=12),
        legend.text=element_text(size=11))

# View(agr)


####looking at adults distribution of subjects in MS
ads <- subset(d_new, d_new$age=="adult")
ads_ms <- subset(ads, ads$report=="ms")
agr = ads_ms %>%
  group_by(subject,noun) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

ads = ads_ms %>%
  drop_na() %>%
  select(subject,trial) %>%
  gather(response,Seen,-subject,-,-trial) %>%
  select(-response)


ggplot(agr, aes(subject,y=Proportion_yes,fill=noun)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_fill_grey() +
  #facet_wrap(~age) +
  theme(text = element_text(size=16)) +
  ylim(0,1)


##############t-tests
#main effects
kruskal.test(response ~ age, data = d_new)
#Kruskal-Wallis chi-squared = 15.628, df = 1, p-value = 7.711e-05
d_new$noun = as.factor(d_new$noun)
kruskal.test(response ~ noun, data = d_new)
#Kruskal-Wallis chi-squared = 0.087244, df = 1, p-value = 0.7677
d_new$report = as.factor(d_new$report)
kruskal.test(response ~ report, data = d_new)
#Kruskal-Wallis chi-squared = 297.99, df = 3, p-value < 2.2e-16

View(d_new)

ms <- subset(d_new, d_new$report=="ms")
kruskal.test(response ~ knowledge, data = ms)
#Kruskal-Wallis chi-squared = 2.663, df = 1, p-value = 0.1027
kruskal.test(response ~ noun, data = ms)
#Kruskal-Wallis chi-squared = 1.6291, df = 1, p-value = 0.2018
kruskal.test(response ~ age, data = ms)
# Kruskal-Wallis chi-squared = 10.892, df = 1, p-value = 0.0009659 ******
kruskal.test(response ~ subject, data = ms)

#adults
View(d_new)
ads <- subset(d_new, d_new$age=="adult")
ads_ms <- subset(ads, ads$report=="ms")
kruskal.test(noun ~ response, data = ads_ms)
#Kruskal-Wallis chi-squared = 0.61104, df = 1,p-value = 0.4344
kruskal.test(response ~ subject, data = ads_ms)
#Kruskal-Wallis chi-squared = 103.72, df = 39, p-value = 8.78e-08

#kids
kds <- subset(d_new, d_new$age=="child")
kds_ms <- subset(kds, ads$report=="ms")
kruskal.test(noun ~ response, data = kds_ms)
#Kruskal-Wallis chi-squared = 0.027113, df = 1,p-value = 0.8692

#adults vs. kids
ms <- subset(d_new, d_new$report=="ms")

ms_bp <- subset(ms, ms$noun=="bp")
kruskal.test(age ~ response, data = ms_bp)
#Kruskal-Wallis chi-squared = 6.6578, df = 2, p-value= 0.03583

ms_dd <- subset(ms, ms$noun=="dd")
kruskal.test(age ~ response, data = ms_dd)
#Kruskal-Wallis chi-squared = 4.872, df = 1, p-value= 0.0273

# This code isn't working so figure out what's going on here.

# axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
# 
# new = d %>%
#   filter(report %in% c("ms","t+f") & verb == "know")
# new = droplevels(new)

#getting the data into subset to run some pairwise comparisons
ms <- subset(d,d$report=="ms")
View(ms)
ms = droplevels(ms)
nlevels(ms$age)

#run a non-parametric test
kruskal.test(age ~ response, data = ms)
