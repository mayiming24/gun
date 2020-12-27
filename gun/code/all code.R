library(readxl)
library(kedd)
library(KernSmooth)
library(ggplot2)
library(tidyverse)
library(splines)
library(dplyr)
#数据读取初步分析
data<-read_xlsx("D:\\gun\\gun.xlsx")
data<- as.data.frame(data)
date<-table(data$date)
date<- as.data.frame(date)
plot(date$Var1,date$Freq)
hist(date$Freq,breaks = 5)

x<- date$Freq
y<- seq(from=75,to=400,by=1)

h<- 3.5* sd(x)*(length(x))^-0.333
b<-seq(from=50,to=400,by=7.8)
b<-append(b,400)
rb<- rep(0,45)
for (i in 1:45) {
  for (j in 1:length(x)) {
    if(x[j]>b[i]){
      if(x[j]<= b[i+1]){rb[i]<-rb[i]+1}
    }
  }
}
p<- rb/length(x)
f<- function(x){
  for (i in 1:46) {
    if (x> b[i]){
      if (x <= b [i+1]){
        return(p[i]/h)
      }
    }
  }
}
fy<- 1:length(y)
for (i in 1:length(y)) {
  fy[i]<- f(y[i])
}
plot(y,fy,type = 'l',ylab = 'density',xlab = 'Number of shootings')
#histogram

xbar<- mean(x)
xsd<- sd(x)
nf<-function(x){dnorm(x,mean = xbar,sd=xsd)}
ke<- dkde(x,y,deriv.order = 0,kernel = 'gaussian')
ke2<-dkde(x,y,deriv.order = 0,kernel = 'uniform')
plot(y,fy,xlim = c(50,300), type = 'l',ylab = 'density',xlab = 'Number of shootings')
lines(ke$eval.points, ke$est.fx,xlim = c(50,400),col=2)
lines(ke2$eval.points, ke2$est.fx,col=3)
lines(y,nf(y),col=4)
legend("topright",                                    #图例位置为右上角
       legend=c("Histogram","Parametric method","gaussian KDE","uniform KDE"), 
       col=c(1,4,2,3),lty=1,lwd=2,bty='n',cex = 0.7)
#KDE

shapiro.test(x)
qqnorm((x-xbar)/sd(x),xlim=c(-2,2),ylim = c(-2,2))
#正态检验

dat<- cbind(1:length(date$Var1),date$Freq)
colnames(dat)=c('a','b')
dat<- as.data.frame(dat)
ggplot (data = dat)+
  geom_point (mapping = aes(x = a, y = b),colour='gray')+
  geom_smooth(mapping = aes(x = a, y = b))
#gam模型

plot(dat$a,dat$b,col='gray',ylim = c(50,250))
lines(smooth(date$Freq),col=7)
abline(lm(dat$b ~dat$a ),col=1)
d<- ts(data = dat$b,frequency = 1)
m1<-stats::filter(d,rep(1/20,20),sides = 2)
lines(dat$a,m1,col=4)
m2<-stats::filter(m1,rep(1/20,20),sides = 2)
lines(dat$a,m2,col=3)
m3<-stats::filter(m2,rep(1/20,20),sides = 2)
lines(dat$a,m3,col=2)
#moving average

legend("topleft",legend=c("linear","Moving median","1th moving average","2th moving average","3th moving average"), 
       col=c(1,7,4,3,2),lty=1,lwd=2,bty='n',cex = 0.6)
fit <-loess(b~a,dat,span= 1,degree= 1)
lines(fit)

plot(dat$a,dat$b,col='gray',ylim = c(50,250))
m.bsp <- lm(dat$b ~ bs(dat$a, df = 28 ))
lines(dat$a, fitted(m.bsp), col ='2')
m.nsp<- lm(dat$b ~ ns(dat$a, df = 25 ))
lines(dat$a, fitted(m.nsp), col ='7')
lines(locpoly(dat$a,dat$b,bandwidth = dpill(dat$a,dat$b),gridsize = 1000),col=3)
lines(dat$a,m3,col=4)
legend("topleft",legend=c("b-spline","natural spline","kenerl regression","3th moving average"), 
       col=c(2,7,3,4),lty=1,lwd=2,bty='n',cex = 0.6)
#spline

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


library('FNN')
cat(range(dat$x), range(dat$y)) 
xrange <- seq(from = -130, to = -60, by = 1)
yrange <- seq(from = 25, to = 50, by = 0.5)
knnde <- function(x, k, xrange, yrange){
  p <- ncol(x)
  n <- nrow(x)
  est_pt <- expand.grid(xrange, yrange)
  distance <- knnx.dist(x, est_pt, k)
  est_de <- matrix(k / (2 * n * distance[,k]), nrow = length(xrange))
  return(est_de)
}
k <- 16
fit_knnde <- knnde(dat, k, xrange, yrange)
persp(xrange, yrange, fit_knnde, phi = 10, theta = 30, col = 4, border=0,
      xlab = 'longtitude',ylab = 'latitude',zlab = 'density')
which(fit_knnde==max(fit_knnde),arr.ind=T)