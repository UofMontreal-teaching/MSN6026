#' ---
#' title: "Tests d'hypothèse"
#' author: "Jérôme Lavoué"
#' date: "sept 2024"
#' output: github_document
#' ---
#' 
#' 



#+ r setup, include=FALSE, cache = FALSE,warning=FALSE, message = FALSE

require("knitr")

## setting working directory

opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

#+ libraries, echo = FALSE,warning=FALSE, message = FALSE

suppressMessages(library(ggplot2))
suppressMessages(library(ggthemes))
suppressMessages(library(writexl))
suppressMessages(library(scales))

#+ graph,echo = FALSE,warning=FALSE, message = FALSE

alpha <- 0.10

n <- 25


n.sim <-100000

#+ unilateral rejection region , include = TRUE , echo = FALSE

mydata <-data.frame(x=seq(-3,3,length=1000))
mydata$y<- dt(mydata$x,(n-1))

p <- ggplot(data=mydata, aes(x=x)) 

p <- p +   geom_line(aes(y=y),colour='black',size=1.5)	
p <- p +   geom_ribbon(data=subset(mydata,x<=qt( (1-alpha) ,(n-1) )),aes(ymax=y),ymin=0,
                       fill="steelblue1",colour=NA,alpha=I(1/3))
p <- p +   geom_ribbon(data=subset(mydata,x>qt( (1-alpha) ,(n-1) )),aes(ymax=y),ymin=0,
                       fill="red",colour=NA,alpha=I(1/3))
p <- p +   annotate( "segment" , x = qt( (1-alpha) ,(n-1) ), y = 0, xend = qt( (1-alpha) ,(n-1) ), yend = dt(qt( (1-alpha) ,(n-1) ),(n-1)),size=1)                                                                                        

p <- p + xlab('') +  ylab('Densité') + xlab("Statistique de test")

p <- p +  theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=18,vjust=+0.55))

p <- p + annotate("text", x = 1.7, y = 0.03, label = "alpha", parse = TRUE, size = 12)

p <- p +  scale_x_continuous( limits=c(-3,3) )
p <- p +  scale_y_continuous( limits=c(0,0.4) )

p  


#ggsave("F:/Dropbox/bureau/cours université de Montréal/figures-images/MSN6026/Inference1/unilat_rejection_zone.png",width = 12, height = 9, units = "cm",dpi=300)

#+ bilateral rejection region , include = TRUE , echo = FALSE


mydata <-data.frame(x=seq(-3,3,length=1000))
mydata$y<- dt(mydata$x,(n-1))

p <- ggplot(data=mydata, aes(x=x)) 

p <- p +   geom_line(aes(y=y),colour='black',size=1.5)	
p <- p +   geom_ribbon(data=subset(mydata,x<=qt( (1-alpha/2) ,(n-1) )),aes(ymax=y),ymin=0,
                       fill="steelblue1",colour=NA,alpha=I(1/3))

p <- p +   geom_ribbon(data=subset(mydata,x>=qt( alpha/2 ,(n-1) )),aes(ymax=y),ymin=0,
                       fill="steelblue1",colour=NA,alpha=I(1/3))

p <- p +   geom_ribbon(data=subset(mydata,x>qt( (1-alpha/2) ,(n-1) )),aes(ymax=y),ymin=0,
                       fill="red",colour=NA,alpha=I(1/3))

p <- p +   geom_ribbon(data=subset(mydata,x<qt( alpha/2 ,(n-1) )),aes(ymax=y),ymin=0,
                       fill="red",colour=NA,alpha=I(1/3))

p <- p +   annotate( "segment" , x = qt( (1-alpha/2) ,(n-1) ), y = 0, xend = qt( (1-alpha/2) ,(n-1) ), yend = dt(qt( (1-alpha/2) ,(n-1) ),(n-1)),size=1)                                                                                        

p <- p +   annotate( "segment" , x = qt( alpha/2 ,(n-1) ), y = 0, xend = qt( alpha/2 ,(n-1) ), yend = dt(qt( alpha/2 ,(n-1) ),(n-1)),size=1)                                                                                        


p <- p + xlab('') +  ylab('Densité') + xlab("Statistique de test")

p <- p +  theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=18,vjust=+0.55))

p <- p + annotate("text", x = 2.05, y = 0.02, label = "alpha/2", parse = TRUE, size = 6)
p <- p + annotate("text", x = -2.1, y = 0.02, label = "alpha/2", parse = TRUE, size = 6)


p <- p +  scale_x_continuous( limits=c(-3,3) )
p <- p +  scale_y_continuous( limits=c(0,0.4) )

p  


#ggsave("F:/Dropbox/bureau/cours université de Montréal/figures-images/MSN6026/Inference1/bilat_rejection_zone.png",width = 12, height = 9, units = "cm",dpi=300)


#+ unilateral rejection region other side , include = TRUE , echo = FALSE

mydata <-data.frame(x=seq(-3,3,length=1000))
mydata$y<- dt(mydata$x,(n-1))

p <- ggplot(data=mydata, aes(x=x)) 

p <- p +   geom_line(aes(y=y),colour='black',size=1.5)	
p <- p +   geom_ribbon(data=subset(mydata,x<=qt( (alpha) ,(n-1) )),aes(ymax=y),ymin=0,
                       fill="red",colour=NA,alpha=I(1/3))
p <- p +   geom_ribbon(data=subset(mydata,x>qt( (alpha) ,(n-1) )),aes(ymax=y),ymin=0,
                       fill="steelblue1",colour=NA,alpha=I(1/3))
p <- p +   annotate( "segment" , x = qt( (alpha) ,(n-1) ), y = 0, xend = qt( (alpha) ,(n-1) ), yend = dt(qt( (alpha) ,(n-1) ),(n-1)),size=1)                                                                                        

p <- p + xlab('') +  ylab('Densité') + xlab("Statistique de test")

p <- p +  theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=18,vjust=+0.55))

p <- p + annotate("text", x = -1.7, y = 0.03, label = "alpha", parse = TRUE, size = 12)

p <- p +  scale_x_continuous( limits=c(-3,3) )
p <- p +  scale_y_continuous( limits=c(0,0.4) )

p  

#ggsave("C:/jerome/Dropbox/maison/Nouveau dossier/unilat_rejection_zone.png",width = 12, height = 9, units = "cm",dpi=300)