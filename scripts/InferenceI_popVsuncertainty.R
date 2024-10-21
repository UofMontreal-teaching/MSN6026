#' ---
#' title: "population vs incertitude"
#' author: "Jérôme Lavoué"
#' date: "October 21, 2024"
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

gm1 <- 30
gm2 <- 100

gsd1 <- 2
gsd2 <- 1.3

perc <- 0.95

n.sim <-100000

#+ upper limit population , include = TRUE , echo = FALSE

mydata <-data.frame(x=seq(0,200,length=1000))
mydata$y<- dlnorm(mydata$x, meanlog = log(gm1), sdlog = log(gsd1))

p <- ggplot(data=mydata, aes(x=x)) 

p <- p +   geom_line(aes(y=y),colour='black',size=1.5)	
p <- p +   geom_ribbon(data=subset(mydata,x<=qlnorm( perc ,meanlog = log(gm1), sdlog = log(gsd1))),aes(ymax=y),ymin=0,
              fill="chartreuse4",colour=NA,alpha=I(1/3))

p <- p +   annotate("segment", x = qlnorm( perc ,meanlog = log(gm1), sdlog = log(gsd1)), 
                               y = 0, 
                               xend = qlnorm( perc ,meanlog = log(gm1), sdlog = log(gsd1)), 
                               yend = dlnorm(qlnorm( perc ,meanlog = log(gm1), sdlog = log(gsd1)),
                                          meanlog = log(gm1), sdlog = log(gsd1)),size=1)                                                                                        

p <- p + xlab('') +  ylab('Densité')

p <- p +  theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18,vjust=+0.55))+
          theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=18,vjust=+0.55)) 

p <- p +  scale_x_continuous( limits=c(0,150) )
p <- p +  scale_y_continuous( limits=c(0,0.025) )

p <- p + theme_solarized()

p  

#    ggsave("C:/jerome/Dropbox/temp/bla.jpeg", width = 11, height = 6, units = "cm",dpi=600) 

#+ upper limit uncertainty , include = TRUE , echo = FALSE

mydata <-data.frame(x=seq(25,250,length=1000))
mydata$y<- dlnorm(mydata$x, meanlog = log(gm2), sdlog = log(gsd2))

p <- ggplot(data=mydata, aes(x=x)) 

p <- p +   geom_line(aes(y=y),colour='black',size=1.5)	
p <- p +   geom_ribbon(data=subset(mydata,x<=qlnorm( perc ,meanlog = log(gm2), sdlog = log(gsd2))),aes(ymax=y),ymin=0,
                       fill="steelblue1",colour=NA,alpha=I(1/3))

p <- p +   annotate("segment", x = qlnorm( perc ,meanlog = log(gm2), sdlog = log(gsd2)), 
                    y = 0, 
                    xend = qlnorm( perc ,meanlog = log(gm2), sdlog = log(gsd2)), 
                    yend = dlnorm(qlnorm( perc ,meanlog = log(gm2), sdlog = log(gsd2)),
                                  meanlog = log(gm2), sdlog = log(gsd2)),size=1)                                                                                        

p <- p + xlab('') +  ylab('Densité')

p <- p +  theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=18,vjust=+0.55)) 

p <- p +  scale_x_continuous( limits=c(25,250) )
p <- p +  scale_y_continuous( limits=c(0,0.017) )

p <- p + theme_solarized()

p  

#    ggsave("C:/jerome/Dropbox/temp/bla.jpeg", width = 11, height = 6, units = "cm",dpi=600) 


#+ calculations , include = FALSE , echo = FALSE

#main calculation

data1 <- rlnorm( n.sim , meanlog = log(gm1), sdlog = log(gsd1) )
data2 <- rlnorm( n.sim , meanlog = log(gm2), sdlog = log(gsd2) )


mydata <- data.frame(x1 = data1 , x2 = data2)


delta <-5

mydata <-
  mutate(mydata,
         grp1 = cut(x1, seq(0, 150, by = delta), labels = FALSE, include.lowest = TRUE),
         flag1 = between(grp1, 0 , round((qlnorm( perc ,meanlog = log(gm1), sdlog = log(gsd1)))/delta ) )
  )

mydata <-
  mutate(mydata,
         grp2 = cut(x2, seq(0, 250, by = delta), labels = FALSE, include.lowest = TRUE),
         flag2 = between(grp2, 0 , round((qlnorm( perc ,meanlog = log(gm2), sdlog = log(gsd2)))/delta ) )
  )


#+ upper limit population histogram, include = TRUE , echo = FALSE

p <- ggplot( mydata ,aes(x1, fill = flag1))

p <- p  +  geom_histogram(binwidth = delta, boundary = 0, closed = "left", 
                 alpha = 0.5, size = 0.1)

#beauty

p <- p + xlab('') +  ylab("Fréquence")

p <- p +  theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=18,vjust=+0.55)) 

p <- p +  scale_x_continuous( limits=c(0,150) )
#p <- p +  scale_y_continuous( limits=c(0,0.8) )

p <- p + scale_fill_manual(values=c("cornsilk3", "chartreuse4"))

p <- p+ theme_solarized()

p <- p + theme( legend.position = "none")

p 

#    ggsave("C:/jerome/Dropbox/temp/bla.jpeg", width = 11, height = 6, units = "cm",dpi=300) 

#+ upper limit uncertainty histogram, include = TRUE , echo = FALSE

p <- ggplot( mydata ,aes(x2, fill = flag2))

p <- p  +  geom_histogram(binwidth = delta, boundary = 0, closed = "left", 
                          alpha = 0.5, size = 0.1)

#beauty

p <- p + xlab('') +  ylab("Fréquence")

p <- p +  theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=18,vjust=+0.55)) 

p <- p +  scale_x_continuous( limits=c(25,250) )
#p <- p +  scale_y_continuous( limits=c(0,0.8) )

p <- p + scale_fill_manual(values=c("cornsilk3", "steelblue1"))

p <- p+ theme_solarized()

p <- p + theme( legend.position = "none")

p 

#    ggsave("C:/jerome/Dropbox/temp/bla.jpeg", width = 11, height = 6, units = "cm",dpi=300) 
