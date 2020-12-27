library(readxl)
library(tseries)
library(forecast)
library(kedd)
library(KernSmooth)
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
shapiro.test(x)
xbar<- mean(x)
xsd<- sd(x)
nf<-function(x){dnorm(x,mean = xbar,sd=xsd)}

ke<- dkde(x,y,deriv.order = 0,kernel = 'gaussian')
ke2<-dkde(x,y,deriv.order = 0,kernel = 'uniform')
plot(y,fy,xlim = c(50,300), type = 'l',ylab = 'density',xlab = 'Number of shootings')
lines(ke$eval.points, ke$est.fx,xlim = c(50,400),col=2)
lines(ke2$eval.points, ke2$est.fx,col=3)
lines(y,nf(y),col=4)
legend("topright",                                    #Í¼ÀýÎ»ÖÃÎªÓÒÉÏ½Ç
      legend=c("Histogram","Parametric method","gaussian KDE","uniform KDE"), 
      col=c(1,4,2,3),lty=1,lwd=2,bty='n',cex = 0.7)

shapiro.test(x)
qqnorm((x-xbar)/sd(x),xlim=c(-2,2),ylim = c(-2,2))


