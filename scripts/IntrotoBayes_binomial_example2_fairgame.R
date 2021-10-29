#' ---
#' title: "IntrotoBayes_binomial_example2_fair game"
#' author: "Jérôme Lavoué"
#' date: "October 29, 2021"
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


#' binomial example, the fair game - figure 1
#' apriori : p centered on 0.4 using a beta distrib
#' 
#' Information à priori


#+ binomial1 , echo  = FALSE

        mu <- 0.4
        
        sigma <- 0.12
        
        beta0 <- mu - 1 + (mu*(1-mu)^2/sigma^2)
        
        alpha0 <- (beta0*mu)/(1-mu)
        
        myseq <- seq( from = 0.05 , to = 0.95 , by = 0.1 ) 
        
        mydata <- data.frame( x = myseq , y.prior =  dbeta(  shape1 = alpha0 , shape2 = beta0 , x = myseq ) )
        
        mydata$y.prior <- mydata$y.prior/sum(mydata$y.prior) 
        
        p <- ggplot( data = mydata, aes( x = x , y = y.prior ))     
        
        p <- p + geom_segment( aes( x = x , xend = x , y = 0 , yend = y.prior))
        
        p <- p + theme_solarized()
        
        p <- p +  xlab('Hypothèses pour p, probabilité de gagner')+
          ylab('Probabilité à priori - P(Hi)')+
          theme(axis.title.x=element_text(size=14))+
          theme(axis.title.y=element_text(size=14,angle=90))+
          theme(axis.text.x=element_text(size=12 , hjust = 0.5))+
          theme(axis.text.y=element_text(size=12 , angle=90 ,  hjust = 0.5))
        
        p <- p + scale_y_continuous(expand = c(0 , 0.01) , 
                                    limits = c(0,0.5),
                                    labels = scales::number_format(accuracy = 0.01,
                                                                   decimal.mark = ',') )
        p <- p + scale_x_continuous(breaks = seq( from = 0 , to = 1 , by = 0.1 ) )
        
        p


#' binomial example, the fair game - figure 2
#' 
#' vraisemblance
        
#+ binomial2 , echo  = FALSE
                
mydata$y.lik <- dbinom( x = 14 , size = 41 , prob = mydata$x )       
        
p <- ggplot( data = mydata, aes( x = x , y = y.lik ))     

p <- p + geom_segment( aes( x = x , xend = x , y = 0 , yend = y.lik))

p <- p + theme_solarized()

p <- p +  xlab('Hypothèses pour p, probabilité de gagner')+
  ylab('P(O|Hi) - vraisemblance')+
  theme(axis.title.x=element_text(size=14))+
  theme(axis.title.y=element_text(size=14,angle=90))+
  theme(axis.text.x=element_text(size=12 , hjust = 0.5))+
  theme(axis.text.y=element_text(size=12 , angle=90 ,  hjust = 0.5))

p <- p + scale_y_continuous(expand = c(0 , 0.01) , 
                            limits = c(0,0.5),
                            labels = scales::number_format(accuracy = 0.01,
                                                           decimal.mark = ',') )
p <- p + scale_x_continuous(breaks = seq( from = 0 , to = 1 , by = 0.1 ) )

p        
        

#' binomial example, the fair game - figure 3
#' 
#' posterior

#+ binomial3 , echo  = FALSE

      beta <- beta0 + 41 - 14
      
      alpha <- alpha0 + 14
      
      mydata$y.post <-  dbeta(  shape1 = alpha , shape2 = beta , x = mydata$x ) 
      
      mydata$y.post <- mydata$y.post/sum(mydata$y.post) 
      
      p <- ggplot( data = mydata, aes( x = x , y = y.post ))     
      
      p <- p + geom_segment( aes( x = x , xend = x , y = 0 , yend = y.post))
      
      p <- p + theme_solarized()
      
      p <- p +  xlab('Hypothèses pour p, probabilité de gagner')+
        ylab('P(Hi|O) - probabilité à posteriori')+
        theme(axis.title.x=element_text(size=14))+
        theme(axis.title.y=element_text(size=14,angle=90))+
        theme(axis.text.x=element_text(size=12 , hjust = 0.5))+
        theme(axis.text.y=element_text(size=12 , angle=90 ,  hjust = 0.5))
      
      p <- p + scale_y_continuous(expand = c(0 , 0.01) , 
                                  limits = c(0,0.70),
                                  labels = scales::number_format(accuracy = 0.01,
                                                                 decimal.mark = ',') )
      p <- p + scale_x_continuous(breaks = seq( from = 0 , to = 1 , by = 0.1 ) )
      
      p


    ## exporting mydata
    
        
     write_xlsx( mydata , "data/binomialExampleFairGame.xlsx")



#' binomial example, the fair game - figure 3
#' 
#' Prior pour un univers continu
     
#+ binomial4 , echo  = FALSE     
     
     p <- ggplot()  +  xlim( 0 , 1 ) + geom_function(fun = dbeta, args = list(shape1 = alpha0 , shape2 = beta0))
     
     p <- p + theme_solarized()
     
     p <- p +  xlab('Hypothèses pour p, probabilité de gagner')+
       ylab('Densité de probabilité')+
       theme(axis.title.x=element_text(size=14))+
       theme(axis.title.y=element_text(size=14,angle=90))+
       theme(axis.text.x=element_text(size=12 , hjust = 0.5))+
       theme(axis.text.y=element_text(size=12 , angle=90 ,  hjust = 0.5))
     
     p <- p + scale_y_continuous(expand = c(0 , 0.01) , 
                                 limits = c(0,6.5),
                                 labels = scales::number_format(accuracy = 0.01,
                                                                decimal.mark = ',') )
     
     p  
     
     
#' binomial example, the fair game - figure 3
#' 
#' vraisemblance pour un univers continu
     
#+ binomial5 , echo  = FALSE       
     
     
     
     
     p <- ggplot()  +  xlim( 0 , 1 ) + geom_function(fun = dbinom, args = list(x = 14, size = 41))
     
     p <- p + theme_solarized()
     
     p <- p +  xlab('Hypothèses pour p, probabilité de gagner')+
       ylab('Densité')+
       theme(axis.title.x=element_text(size=14))+
       theme(axis.title.y=element_text(size=14,angle=90))+
       theme(axis.text.x=element_text(size=12 , hjust = 0.5))+
       theme(axis.text.y=element_text(size=12 , angle=90 ,  hjust = 0.5))
     
     p <- p + scale_y_continuous(expand = c(0 , 0.01) , 
                                 limits = c(0,0.15),
                                 labels = scales::number_format(accuracy = 0.01,
                                                                decimal.mark = ',') )
     
     p  
     
     
     
     
#' binomial example, the fair game - figure 3
#' 
#' Pposterior pour un univers continu
     
#+ binomial6 , echo  = FALSE     
     
     p <- ggplot()  +  xlim( 0 , 1 ) + geom_function(fun = dbeta, args = list(shape1 = alpha , shape2 = beta))
     
     p <- p + theme_solarized()
     
     p <- p +  xlab('Hypothèses pour p, probabilité de gagner')+
       ylab('Densité de probabilité')+
       theme(axis.title.x=element_text(size=14))+
       theme(axis.title.y=element_text(size=14,angle=90))+
       theme(axis.text.x=element_text(size=12 , hjust = 0.5))+
       theme(axis.text.y=element_text(size=12 , angle=90 ,  hjust = 0.5))
     
     p <- p + scale_y_continuous(expand = c(0 , 0.01) , 
                                 limits = c(0,6.5),
                                 labels = scales::number_format(accuracy = 0.01,
                                                                decimal.mark = ',') )
     
     p  
     
     
     
