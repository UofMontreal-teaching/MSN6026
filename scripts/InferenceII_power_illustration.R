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

sigmaH1.1 <- 5
sigmaH1.2 <- 2

muH0 <- 93

sigmaH0 <- 5

alpha <- 0.05

n <- 5

n.sim <-10000

#+ rejection region , include = FALSE , echo = FALSE

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



#+ calculations , include = FALSE , echo = FALSE


mysample1.1 <- rnorm( n.sim*n , mean= muH1.1 , sd=sigmaH1.1 )

mysample1.1 <- matrix(mysample1.1, nrow = n.sim , ncol = n)

mysample1.2 <- rnorm( n.sim*n , mean= muH1.1 , sd=sigmaH1.2 )

mysample1.2 <- matrix(mysample1.2, nrow = n.sim , ncol = n)



my.st1.1 <- apply( mysample1.1 , 1 , function(x) { (mean(x) - muH0)/(sigmaH1.1/sqrt(n))})
my.st1.2 <- apply( mysample1.2 , 1 , function(x) { (mean(x) - muH0)/(sigmaH1.2/sqrt(n))})
#my.st1.3 <- apply( mysample , 1 , function(x) { (mean(x) - muH0)/(sigmaH1.1/sqrt(n))})
#my.st1.4 <- apply( mysample , 1 , function(x) { (mean(x) - muH0)/(sigmaH1.1/sqrt(n))})


my.emp.st <- data.frame(x1.1=my.st1.1)
my.emp.st$x1.2 <-my.st1.2

my.emp.st$breaks1.1 <- cut(my.emp.st$x1.1, breaks = qt( (1-alpha) ,(n-1) ) )

#+ power 1 diff variab, include = FALSE , echo = FALSE


# color change for x

my.emp.st <-
  mutate(my.emp.st,
    grp = cut(x1.1, seq(-4, 15, by = 0.1), labels = FALSE, include.lowest = TRUE),
    is6 = between(grp, 60, 90)
  ) 

  ggplot(aes(x, fill = is6)) +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "left", 
                 col = "darkgreen", alpha = 0.5, size = 0.1) +
  scale_x_continuous(breaks = seq(0, 1, 0.1))

# empirical distributionb of st

p <- ggplot(data=my.emp.st )

#empirical st
p <- p + geom_histogram( data= my.emp.st ,aes(x=x1.1,  col = is6, fill = is6), binwidth=0.01 ,size=1.0 )

#critical value
p <- p +  geom_segment(aes(x = qt( (1-alpha) ,(n-1) ), y = 0, xend = qt( (1-alpha) ,(n-1) ), yend = dt( qt(  1-alpha , (n-1) )- (muH1.1-muH0)/(sigmaH1.1/sqrt(n)) ,(n-1))),size=1)                                                        

#beauty

p <- p + xlab('') +  ylab('Densité')

p <- p +  theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=18,vjust=+0.55)) 

p <- p +  scale_x_continuous( limits=c(-4,15) )
p <- p +  scale_y_continuous( limits=c(0,0.8) )

p 




### TO BE CONTINUED

p <- p + theme_solarized()

p <- p +  xlab('Univers des hypothèses pour la taille moyenne (cm)')+
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


