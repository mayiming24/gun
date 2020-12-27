library(readxl)
library(tseries)
library(forecast)
library(kedd)
library(KernSmooth)
library(ggplot2)
library(tidyverse)
library(splines)
library(dplyr)

data<-read_xlsx("D:\\gun\\gun.xlsx")
data<- as.data.frame(data)

date<-table(data$date)
date<- as.data.frame(date)
plot(date$Var1,date$Freq)
plot(smooth(date$Freq),type = 'l')
dat<- cbind(1:length(date$Var1),date$Freq)
colnames(dat)=c('a','b')
dat<- as.data.frame(dat)
ggplot (data = dat)+
  geom_point (mapping = aes(x = a, y = b),colour='gray')+
  geom_smooth(mapping = aes(x = a, y = b))

plot(dat$a,dat$b,col='gray',ylim = c(50,250))
lines(smooth.spline(dat$a,dat$b),col=2)
smooth.spline(dat$a,dat$b)

m.bsp <- lm(dat$b ~ bs(dat$a, df = 28 ))
lines(dat$a, fitted(m.bsp), col ='blue')

m.nsp<- lm(dat$b ~ ns(dat$a, df = 25 ))
lines(dat$a, fitted(m.nsp), col ='red')




plot(dat$a,dat$b,col='gray',ylim = c(50,250))
abline(lm(dat$b ~dat$a ))
d<- ts(data = dat$b,frequency = 1)
m1<-stats::filter(d,rep(1/20,20),sides = 2)
lines(dat$a,m1,col=3)
m2<-stats::filter(m1,rep(1/20,20),sides = 2)
lines(dat$a,m2,col=3)
m3<-stats::filter(m2,rep(1/20,20),sides = 2)
lines(dat$a,m3,col=4)

month<-read_xlsx("D:\\gun\\month.xlsx")
month<- as.data.frame(month)
colnames(month)=c('mon','tol')
m<- ts(data = month$tol,start = c(2014,01),frequency = 12)
plot(decompose(m))


lines(locpoly(dat$a,dat$b,bandwidth = dpill(dat$a,dat$b),gridsize = 1000),col=7)

