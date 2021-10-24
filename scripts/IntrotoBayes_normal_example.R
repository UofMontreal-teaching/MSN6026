####################################
#
#  Estimation AM taille des américains
#   
#
#####################################


#####librairies

library(ggplot2)
library(ggthemes)
library(writexl)
library(scales)


######## prior

mu0 <- 175

tau0 <- 1/5^2

mydata <-data.frame(x=seq(150,200,length=500))
mydata$y<- dnorm(mydata$x,mean=mu0,sd=1/sqrt(tau0))


ICinf <- mu0+qnorm(0.025)*1/sqrt(tau0) 

ICisup <- mu0+qnorm(0.975)*1/sqrt(tau0) 

####### false likelihood

mu <- 190

sd <- 8

n <- 5

tau <- n * (1/sd^2) 


mydata$y2 <- dnorm(mydata$x,mean=mu,sd=1/sqrt(tau))

###### posterior


mydata$y3 <- dnorm(mydata$x, mean= (tau0*mu0 + n*tau*mu)/(tau0+n*tau),sd=1/sqrt(tau0+n*tau))


ggplot(data=mydata, aes(x=x))+  
  geom_line(aes(y=y),colour='black',size=1.5)+
  
  #ylim(0,0.008)+
  xlab('Height (cm)')+
  ylab('Density')+
  theme(legend.position = "none") +
  theme(aspect.ratio = 2/(1+sqrt(5)))	+
  theme(axis.title.x=element_text(vjust=0,size=16))+
  theme(axis.title.y=element_text(size=16,angle=90))+
  theme(axis.text.x=element_text(size=14))+
  theme(axis.text.y=element_text(size=10,angle=90))

ggplot(data=mydata, aes(x=x))+  
  geom_line(aes(y=y2),colour='black',size=1.5)+
  
  #ylim(0,0.008)+
  xlab('Height (cm)')+
  ylab('Density')+
  theme(legend.position = "none") +
  theme(aspect.ratio = 2/(1+sqrt(5)))	+
  theme(axis.title.x=element_text(vjust=0,size=16))+
  theme(axis.title.y=element_text(size=16,angle=90))+
  theme(axis.text.x=element_text(size=14))+
  theme(axis.text.y=element_text(size=10,angle=90))

ggplot(data=mydata, aes(x=x))+  
  geom_line(aes(y=y3),colour='black',size=1.5)+
  
  #ylim(0,0.008)+
  xlab('Height (cm)')+
  ylab('Density')+
  theme(legend.position = "none") +
  theme(aspect.ratio = 2/(1+sqrt(5)))	+
  theme(axis.title.x=element_text(vjust=0,size=16))+
  theme(axis.title.y=element_text(size=16,angle=90))+
  theme(axis.text.x=element_text(size=14))+
  theme(axis.text.y=element_text(size=10,angle=90))
