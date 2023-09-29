#' ---
#' title: "InferenceII_Power illustration"
#' author: "Jérôme Lavoué"
#' date: "October 17, 2022"
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
library(dplyr)


#+ initial calculations , include = FALSE , echo = FALSE

muH1.1 <- 100
muH1.2 <- 95

sigmaH1.1 <- 5
sigmaH1.2 <- 2

muH0 <- 93

sigmaH0 <- 5

alpha <- 0.05
alpha.1 <- 0.10

n <- 5
n.1 <- 10

n.sim <-100000

#+ rejection region , include = TRUE , echo = FALSE

mydata <-data.frame(x=seq(-4,15,length=1000))
mydata$y<- dt(mydata$x,(n-1))

p <- ggplot(data=mydata, aes(x=x)) 

p <- p +   geom_line(aes(y=y),colour='black',size=1.5)	
p <- p +   geom_ribbon(data=subset(mydata,x<=qt( (1-alpha) ,(n-1) )),aes(ymax=y),ymin=0,
              fill="steelblue1",colour=NA,alpha=I(1/3))
p <- p +   geom_ribbon(data=subset(mydata,x>qt( (1-alpha) ,(n-1) )),aes(ymax=y),ymin=0,
              fill="red",colour=NA,alpha=I(1/3))
p <- p +   geom_segment(aes(x = qt( (1-alpha) ,(n-1) ), y = 0, xend = qt( (1-alpha) ,(n-1) ), yend = dt(qt( (1-alpha) ,(n-1) ),(n-1))),size=1)                                                                                        

p <- p + xlab('') +  ylab('Densité')

p <- p +  theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18,vjust=+0.55))+
          theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=18,vjust=+0.55)) 

p <- p +  scale_x_continuous( limits=c(-4,15) )
p <- p +  scale_y_continuous( limits=c(0,0.4) )

p  

#    ggsave("C:/jerome/Dropbox/temp/bla.jpeg", width = 19, height = 10, units = "cm",dpi=600) 



#+ calculations , include = FALSE , echo = FALSE

#main calculation
mysample1.1 <- rnorm( n.sim*n , mean= muH1.1 , sd=sigmaH1.1 )

mysample1.1 <- matrix(mysample1.1, nrow = n.sim , ncol = n)

#different mean
mysample1.2 <- rnorm( n.sim*n , mean= muH1.2 , sd=sigmaH1.1 )

mysample1.2 <- matrix(mysample1.2, nrow = n.sim , ncol = n)

#different variability
mysample1.3 <- rnorm( n.sim*n , mean= muH1.2 , sd=sigmaH1.2 )

mysample1.3 <- matrix(mysample1.3, nrow = n.sim , ncol = n)

#different sample size
mysample1.4 <- rnorm( n.sim*n.1 , mean= muH1.2 , sd=sigmaH1.2 )

mysample1.4 <- matrix(mysample1.4, nrow = n.sim , ncol = n.1)

my.st1.1 <- apply( mysample1.1 , 1 , function(x) { (mean(x) - muH0)/(sd(x)/sqrt(n))})
my.st1.2 <- apply( mysample1.2 , 1 , function(x) { (mean(x) - muH0)/(sd(x)/sqrt(n))})
my.st1.3 <- apply( mysample1.3 , 1 , function(x) { (mean(x) - muH0)/(sd(x)/sqrt(n))})
my.st1.4 <- apply( mysample1.4 , 1 , function(x) { (mean(x) - muH0)/(sd(x)/sqrt(n))})


my.emp.st <- data.frame(x1.1=my.st1.1)
my.emp.st$x1.2 <-my.st1.2
my.emp.st$x1.3 <-my.st1.3
my.emp.st$x1.4 <-my.st1.4

delta <-0.1

my.emp.st <-
  mutate(my.emp.st,
         grp1.1 = cut(x1.1, seq(-4, 15, by = delta), labels = FALSE, include.lowest = TRUE),
         flag1.1 = between(grp1.1, 0 , round((qt( (1-alpha) ,(n-1) )+4)/delta ) )
  )

my.emp.st <-
  mutate(my.emp.st,
         grp1.2 = cut(x1.2, seq(-4, 15, by = delta), labels = FALSE, include.lowest = TRUE),
         flag1.2 = between(grp1.2, 0 , round((qt( (1-alpha) ,(n-1) )+4)/delta ) )
  )



my.emp.st <-
  mutate(my.emp.st,
         grp1.3 = cut(x1.3, seq(-4, 15, by = delta), labels = FALSE, include.lowest = TRUE),
         flag1.3 = between(grp1.3, 0 , round((qt( (1-alpha) ,(n-1) )+4)/delta ) )
  )


my.emp.st <-
  mutate(my.emp.st,
         grp1.4 = cut(x1.4, seq(-4, 15, by = delta), labels = FALSE, include.lowest = TRUE),
         flag1.4 = between(grp1.4, 0 , round((qt( (1-alpha) ,(n.1-1) )+4)/delta ) )
  )

my.emp.st <-
  mutate(my.emp.st,
         grp1.5 = cut(x1.4, seq(-4, 15, by = delta), labels = FALSE, include.lowest = TRUE),
         flag1.5 = between(grp1.5, 0 , round((qt( (1-alpha.1) ,(n.1-1) )+4)/delta ) )
  )
#+ power 1 MAIN POWER ANALYSIS, include = TRUE , echo = FALSE

# empirical distributionb of st

p <- ggplot( my.emp.st ,aes(x1.1, fill = flag1.1))

p <- p  +  geom_histogram(binwidth = delta, boundary = 0, closed = "left", 
                 alpha = 0.5, size = 0.1)

#beauty

p <- p + xlab('') +  ylab("nombre d'expériences")

p <- p +  theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=18,vjust=+0.55)) 

p <- p +  scale_x_continuous( limits=c(-4,15) )
#p <- p +  scale_y_continuous( limits=c(0,0.8) )

p <- p + scale_fill_manual(values=c("yellow4", "wheat3"))

p <- p + theme( legend.position = "none")

p 

#    ggsave("C:/jerome/Dropbox/temp/bla.jpeg", width = 19, height = 10, units = "cm",dpi=300) 


#######################################################################

#+ power 2 different mean, include = TRUE , echo = FALSE

# empirical distributionb of st

p <- ggplot( my.emp.st ,aes(x1.2, fill = flag1.2))

p <- p  +  geom_histogram(binwidth = delta, boundary = 0, closed = "left", 
                          alpha = 0.5, size = 0.1)

#beauty

p <- p + xlab('') +  ylab("nombre d'expériences")

p <- p +  theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=18,vjust=+0.55)) 

p <- p +  scale_x_continuous( limits=c(-4,15) )
#p <- p +  scale_y_continuous( limits=c(0,0.8) )

p <- p + scale_fill_manual(values=c("yellow4", "wheat3"))

p <- p + theme( legend.position = "none")

p 

#    ggsave("C:/jerome/Dropbox/temp/bla.jpeg", width = 19, height = 10, units = "cm",dpi=300) 


#######################################################################

#+ power 3 different var, include = TRUE , echo = FALSE

# empirical distributionb of st

p <- ggplot( my.emp.st ,aes(x1.3, fill = flag1.3))

p <- p  +  geom_histogram(binwidth = delta, boundary = 0, closed = "left", 
                          alpha = 0.5, size = 0.1)

#beauty

p <- p + xlab('') +  ylab("nombre d'expériences")

p <- p +  theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=18,vjust=+0.55)) 

p <- p +  scale_x_continuous( limits=c(-4,15) )
#p <- p +  scale_y_continuous( limits=c(0,0.8) )

p <- p + scale_fill_manual(values=c("yellow4", "wheat3"))

p <- p + theme( legend.position = "none")

p 

#    ggsave("C:/jerome/Dropbox/temp/bla.jpeg", width = 19, height = 10, units = "cm",dpi=300) 


#######################################################################

#+ power 4 different n, include = TRUE , echo = FALSE


mydata <-data.frame(x=seq(-4,15,length=1000))
mydata$y<- dt(mydata$x,(n.1-1))

p <- ggplot(data=mydata, aes(x=x)) 

p <- p +   geom_line(aes(y=y),colour='black',size=1.5)	
p <- p +   geom_ribbon(data=subset(mydata,x<=qt( (1-alpha) ,(n.1-1) )),aes(ymax=y),ymin=0,
                       fill="steelblue1",colour=NA,alpha=I(1/3))
p <- p +   geom_ribbon(data=subset(mydata,x>qt( (1-alpha) ,(n.1-1) )),aes(ymax=y),ymin=0,
                       fill="red",colour=NA,alpha=I(1/3))
p <- p +   geom_segment(aes(x = qt( (1-alpha) ,(n.1-1) ), y = 0, xend = qt( (1-alpha) ,(n.1-1) ), yend = dt(qt( (1-alpha) ,(n.1-1) ),(n.1-1))),size=1)                                                                                        

p <- p + xlab('') +  ylab('Densité')

p <- p +  theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=18,vjust=+0.55)) 

p <- p +  scale_x_continuous( limits=c(-4,15) )
p <- p +  scale_y_continuous( limits=c(0,0.4) )

p  

#    ggsave("C:/jerome/Dropbox/temp/bla.jpeg", width = 19, height = 10, units = "cm",dpi=600) 

# empirical distributionb of st

p <- ggplot( my.emp.st ,aes(x1.4, fill = flag1.4))

p <- p  +  geom_histogram(binwidth = delta, boundary = 0, closed = "left", 
                          alpha = 0.5, size = 0.1)

#beauty

p <- p + xlab('') +  ylab("nombre d'expériences")

p <- p +  theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=18,vjust=+0.55)) 

p <- p +  scale_x_continuous( limits=c(-4,15) )
#p <- p +  scale_y_continuous( limits=c(0,0.8) )

p <- p + scale_fill_manual(values=c("yellow4", "wheat3"))

p <- p + theme( legend.position = "none")

p 

#    ggsave("C:/jerome/Dropbox/temp/bla.jpeg", width = 19, height = 10, units = "cm",dpi=300) 


#######################################################################

#+ power 5 different alpha, include = TRUE , echo = FALSE


mydata <-data.frame(x=seq(-4,15,length=1000))
mydata$y<- dt(mydata$x,(n.1-1))

p <- ggplot(data=mydata, aes(x=x)) 

p <- p +   geom_line(aes(y=y),colour='black',size=1.5)	
p <- p +   geom_ribbon(data=subset(mydata,x<=qt( (1-alpha.1) ,(n.1-1) )),aes(ymax=y),ymin=0,
                       fill="steelblue1",colour=NA,alpha=I(1/3))
p <- p +   geom_ribbon(data=subset(mydata,x>qt( (1-alpha.1) ,(n.1-1) )),aes(ymax=y),ymin=0,
                       fill="red",colour=NA,alpha=I(1/3))
p <- p +   geom_segment(aes(x = qt( (1-alpha.1) ,(n.1-1) ), y = 0, xend = qt( (1-alpha.1) ,(n.1-1) ), yend = dt(qt( (1-alpha.1) ,(n.1-1) ),(n.1-1))),size=1)                                                                                        

p <- p + xlab('') +  ylab('Densité')

p <- p +  theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=18,vjust=+0.55)) 

p <- p +  scale_x_continuous( limits=c(-4,15) )
p <- p +  scale_y_continuous( limits=c(0,0.4) )

p  

#    ggsave("C:/jerome/Dropbox/temp/bla.jpeg", width = 19, height = 10, units = "cm",dpi=600) 

# empirical distributionb of st

p <- ggplot( my.emp.st ,aes(x1.4, fill = flag1.5))

p <- p  +  geom_histogram(binwidth = delta, boundary = 0, closed = "left", 
                          alpha = 0.5, size = 0.1)

#beauty

p <- p + xlab('') +  ylab("nombre d'expériences")

p <- p +  theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=18,vjust=+0.55)) 

p <- p +  scale_x_continuous( limits=c(-4,15) )
#p <- p +  scale_y_continuous( limits=c(0,0.8) )

p <- p + scale_fill_manual(values=c("yellow4", "wheat3"))

p <- p + theme( legend.position = "none")

p 

#    ggsave("C:/jerome/Dropbox/temp/bla.jpeg", width = 19, height = 10, units = "cm",dpi=300) 

