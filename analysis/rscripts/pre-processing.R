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
# this is all preprocessing now. jut go down to the read.csv at line 85

# SKIP THIS PART AND JUST LOAD THE FINAL CSV

b <- read.csv("../data/blue_long.csv")
y <- read.csv("../data/yellow_long.csv")
r <- read.csv("../data/red_long.csv")
g <- read.csv("../data/green_long.csv")
c <- read.csv("../data/cyan_long.csv")
m <- read.csv("../data/magenta_long.csv")
go <- read.csv("../data/goldenrod_long.csv")
o <- read.csv("../data/olive_long.csv")

str(b)

bl = b %>%
  gather(response,factors,MST.predict.where.find:MAT.k0w.who.select,factor_key=TRUE) %>%
  mutate(list="blue") %>%
  mutate(tense="nonfinite")
yl = y %>%
  gather(response,factors,MAF.predict.where.find:MATF.k0w.who.select,factor_key=TRUE) %>%
  mutate(list="yellow") %>%
  mutate(tense="nonfinite")
rl = r %>%
  gather(response,factors,MATF.predict.where.find:MAF.k0w.who.select,factor_key=TRUE) %>%
  mutate(list="red") %>%
  mutate(tense="nonfinite")
gl = g %>%
  gather(response,factors,MAT.predict.who.recruit:MST.k0w.who.select,factor_key=TRUE) %>%
  mutate(list="green") %>%
  mutate(tense="nonfinite")
cl = c %>%
  gather(response,factors,MST.predict.where.find:MAT.know.who.select,factor_key=TRUE) %>%
  mutate(list="cyan") %>%
  select(subject,native_lang,response,factors,list) %>%
  mutate(tense="finite")
ml = m %>%
  gather(response,factors,MATF.predict.where.find:MAF.k0w.who.select,factor_key=TRUE) %>%
  mutate(list="magenta") %>%
  mutate(tense="finite")
gol = go %>%
  gather(response,factors,MAF.predict.where.find:MATF.k0w.who.select,factor_key=TRUE) %>%
  mutate(list="goldenrod") %>%
  mutate(tense="finite")
ol = o %>%
  gather(response,factors,MAT.predict.where.find:MST.k0w.who.select,factor_key=TRUE) %>%
  mutate(list="olive") %>%
  mutate(tense="finite")
  
nrow(bl) #1120
nrow(yl) #960
nrow(rl)#896
nrow(gl) #896
nrow(cl)#800
nrow(ml)#832
nrow(gol)#1088
nrow(ol)#832
#total:7424

total <- rbind(bl,yl,rl,gl,cl,ml,gol,ol)
# nrow(total)
# #remove non-native speakers
tot_nat = subset(total, total$native_lang=="EN")
# View(tot_nat)
# write.csv(tot_nat,"tot_nat.csv")

# nrow(tot_nat) #6880
