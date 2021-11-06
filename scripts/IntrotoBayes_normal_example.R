#' ---
#' title: "IntrotoBayes_normal_example"
#' author: "Jérôme Lavoué"
#' date: "October 24, 2021"
#' output: github_document
#' ---
#' 
#' 

#+ r setup, include=FALSE, cache = FALSE
require("knitr")
## setting working directory
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

#+ libraries, include = FALSE

library(ggplot2)
library(ggthemes)
library(writexl)
library(scales)


#+ initial calculations , include = FALSE

mu0 <- 175

tau0 <- 1/5^2

mydata <-data.frame(x=seq(150,200,length=500))

mydata$y<- dnorm(mydata$x,mean=mu0,sd=1/sqrt(tau0))


####### false likelihood

mu <- 190

sd <- 8

n <- 5

tau <- n * (1/sd^2) 

mydata$y2 <- dnorm(mydata$x,mean=mu,sd=1/sqrt(tau))

###### posterior

mydata$y3 <- dnorm(mydata$x, mean= (tau0*mu0 + n*tau*mu)/(tau0+n*tau),sd=1/sqrt(tau0+n*tau))


#' A look at the prior - figure 1

#+ prior graph , echo = FALSE

p <- ggplot(data=mydata, aes(x=x))+ geom_line(aes(y=y),colour='red',size=1.5)
  
p <- p + theme_solarized()

p <- p +  xlab('Univers des hypothèse pour la taille moyenne (cm)')+
  ylab('Densité')+
  theme(axis.title.x=element_text(size=14))+
  theme(axis.title.y=element_text(size=14))+
  theme(axis.text.x=element_text(size=12 , hjust = 0.5))+
  theme(axis.text.y=element_text(size=12 ,  hjust = 0.5))

p <- p + scale_y_continuous(expand = c(0 , 0.01) , 
                            limits = c(0,0.12),
                            labels = scales::number_format(accuracy = 0.01,
                                                           decimal.mark = ',') )

p  


#' A look at the likelihood - figure 2

#+ likelihood graph , echo = FALSE

p <- ggplot(data=mydata, aes(x=x))+ geom_line(aes(y=y2),colour='black',size=1.5)

p <- p + theme_solarized()

p <- p +  xlab('Univers des hypothèse pour la taille moyenne (cm)')+
  ylab('Densité')+
  theme(axis.title.x=element_text(size=14))+
  theme(axis.title.y=element_text(size=14))+
  theme(axis.text.x=element_text(size=12 , hjust = 0.5))+
  theme(axis.text.y=element_text(size=12 ,  hjust = 0.5))

p <- p + scale_y_continuous(expand = c(0 , 0.01) , 
                            limits = c(0,0.12),
                            labels = scales::number_format(accuracy = 0.01,
                                                           decimal.mark = ',') )

p <- p + scale_x_continuous(limits = c(150,200),
                            labels = scales::number_format(accuracy = 0.01,
                                                           decimal.mark = ',') )

p  

#' Distribution postérieure simulée par échantillonnage (n=1000)

#+ posterior simulation , echo = FALSE , message = FALSE , warning = FALSE

mysample <- data.frame( x = rnorm( 1000 , mean= (tau0*mu0 + n*tau*mu)/(tau0+n*tau),sd=1/sqrt(tau0+n*tau)))

p <- ggplot(data=mydata, aes(x=x))+ geom_line(aes(y=y),colour='red',size=1.0)

p <- p + geom_line( aes(y=y2),colour='black',size=1.0)

p <- p + geom_histogram( data= mysample ,aes(x=x,y=..density..),size=1.0, alpha = 0.5 , bins=50 , fill = "green" )

p <- p + theme_solarized()

p <- p +  xlab('Univers des hypothèse pour la taille moyenne (cm)')+
  ylab('Densité')+
  theme(axis.title.x=element_text(size=14))+
  theme(axis.title.y=element_text(size=14))+
  theme(axis.text.x=element_text(size=12 , hjust = 0.5))+
  theme(axis.text.y=element_text(size=12 ,  hjust = 0.5))

p <- p + scale_y_continuous(expand = c(0 , 0.01) , 
                            limits = c(0,0.30),
                            labels = scales::number_format(accuracy = 0.01,
                                                           decimal.mark = ',') )

p <- p + scale_x_continuous(limits = c(150,200),
                            labels = scales::number_format(accuracy = 0.01,
                                                           decimal.mark = ',') )

p


#write_xlsx( mysample , "data/myposteriorsample.xlsx" )


#' Distribution postérieure théorique 

#+ posterior theo , echo = FALSE , message = FALSE , warning = FALSE


p <- ggplot(data=mydata, aes(x=x))+ geom_line(aes(y=y),colour='red',size=1.0)

p <- p + geom_line( aes(y=y2),colour='black',size=1.0)

p <- p + geom_line( aes(y=y3),colour='green',size=1.0 )

p <- p + theme_solarized()

p <- p +  xlab('Univers des hypothèse pour la taille moyenne (cm)')+
  ylab('Densité')+
  theme(axis.title.x=element_text(size=14))+
  theme(axis.title.y=element_text(size=14))+
  theme(axis.text.x=element_text(size=12 , hjust = 0.5))+
  theme(axis.text.y=element_text(size=12 ,  hjust = 0.5))

p <- p + scale_y_continuous(expand = c(0 , 0.01) , 
                            limits = c(0,0.30),
                            labels = scales::number_format(accuracy = 0.01,
                                                           decimal.mark = ',') )

p <- p + scale_x_continuous(limits = c(150,200),
                            labels = scales::number_format(accuracy = 0.01,
                                                           decimal.mark = ',') )

p



#' Distribution postérieure simulée par échantillonnage (n=1000) (uniquement la postérieure)

#+ posterior simulation2 , echo = FALSE , message = FALSE , warning = FALSE


p <- ggplot(data=mysample)

p <- p + geom_histogram( aes(x=x),size=1.0, alpha = 0.5 , bins=50 )

p <- p + theme_solarized()

p <- p +  xlab('Univers des hypothèse pour la taille moyenne (cm)')+
  ylab('fréquence')+
  theme(axis.title.x=element_text(size=14))+
  theme(axis.title.y=element_text(size=14))+
  theme(axis.text.x=element_text(size=12 , hjust = 0.5))+
  theme(axis.text.y=element_text(size=12 ,  hjust = 0.5))

p <- p + scale_y_continuous(expand = c(0 , 0.01) , 
                            #limits = c(0,0.30),
                            labels = scales::number_format(accuracy = 0.01,
                                                           decimal.mark = ',') )

p <- p + scale_x_continuous(limits = c(180,200),
                            labels = scales::number_format(accuracy = 0.01,
                                                           decimal.mark = ',') )

p


