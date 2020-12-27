library(readxl)
library(kedd)
library(KernSmooth)
library(ggplot2)
library(tidyverse)
library(splines)
library(dplyr)
data<-read_xlsx("D:\\gun\\gunwithoutna.xlsx")
data<- as.data.frame(data)
y<- data$latitude
x<- data$longitude
dat<-cbind(x,y)
dat<-as.data.frame(dat)
plot(dat)

ggplot(data = dat, mapping = aes(x = x, y = y)) +
  geom_density2d_filled() +
  xlim(-130,-60)+
  ylim(20,50)
  


ggplot(dat, aes(x=x, y=y)) +
  geom_hex(bins = 1000) +
  xlim(-130,-60)+
  ylim(25,50)+
  scale_fill_gradient(low="red", high="gray") #调整颜色

library("maps")
library("mapdata")
map("state", col = "black", ylim = c(25, 50),xlim = c(-130,-60), panel.first = grid())
title("usa")
par(new=TRUE)
ggplot(dat, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+
  xlim(-130,-60)+
  ylim(25,50)+
  scale_fill_gradient(low = 'gray',high = 'red')
