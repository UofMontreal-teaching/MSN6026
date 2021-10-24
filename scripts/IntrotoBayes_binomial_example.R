#' ---
#' title: "IntrotoBayes_binomial_example"
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


#' binomial example, the false die - figure 1


#+ binomial1 , echo  = FALSE

    
    myseq <- c( 0.1 , 0.2 , 0.34 , 0.5 , 0.6 , 0.8) 
    
    mydata <- data.frame( x = myseq , y =  dbinom( x = 14 , size = 41 , prob = myseq ) )
    
    p <- ggplot( data = mydata, aes( x = x , y = y )) 
    
    p <- p +  geom_point( colour='black' , size=2 )
    
    #p <- p + theme_calc()
    
    p <- p + theme_solarized()
    
    p <- p +  xlab('Probabilité de succès')+
              ylab('Probabilité')+
              theme(axis.title.x=element_text(size=14))+
              theme(axis.title.y=element_text(size=14,angle=90))+
              theme(axis.text.x=element_text(size=12 , hjust = 0.5))+
              theme(axis.text.y=element_text(size=12 , angle=90 ,  hjust = 0.5))
    
    p <- p + scale_y_continuous(expand = c(0 , 0.01) , 
                                limits = c(0,0.15),
                                labels = scales::number_format(accuracy = 0.01,
                                                               decimal.mark = ',') )
    
    p          


#' binomial example, the false die - figure 2
    
    
#+ binomial2 , echo  = FALSE
    
    
    myseq <- seq( from = 0 , to = 1 , by = 0.025 ) 
    
    mydata <- data.frame( x = myseq , y =  dbinom( x = 14 , size = 41 , prob = myseq ) )
    
    p <- ggplot( data = mydata, aes( x = x , y = y )) 
    
    p <- p +  geom_point( colour='black' , size=2 )
    
    #p <- p + theme_calc()
    
    p <- p + theme_solarized()
    
    p <- p +  xlab('Probabilité de succès')+
      ylab('Probabilité')+
      theme(axis.title.x=element_text(size=14))+
      theme(axis.title.y=element_text(size=14,angle=90))+
      theme(axis.text.x=element_text(size=12 , hjust = 0.5))+
      theme(axis.text.y=element_text(size=12 , angle=90 ,  hjust = 0.5))
    
    p <- p + scale_y_continuous(expand = c(0 , 0.01) , 
                                limits = c(0,0.15),
                                labels = scales::number_format(accuracy = 0.01,
                                                               decimal.mark = ',') )
    
    p          
    
#' binomial example, the false die - figure 3
    
    
#+ binomial3 , echo  = FALSE

    p <- ggplot()  +  xlim( 0 , 1 ) + geom_function(fun = dbinom, args = list(x = 14, size = 41))
    
    p <- p + theme_solarized()
    
    p <- p +  xlab('Probabilité de succès')+
      ylab('Probabilité')+
      theme(axis.title.x=element_text(size=14))+
      theme(axis.title.y=element_text(size=14,angle=90))+
      theme(axis.text.x=element_text(size=12 , hjust = 0.5))+
      theme(axis.text.y=element_text(size=12 , angle=90 ,  hjust = 0.5))
    
    p <- p + scale_y_continuous(expand = c(0 , 0.01) , 
                                limits = c(0,0.15),
                                labels = scales::number_format(accuracy = 0.01,
                                                               decimal.mark = ',') )
      
    p  
      
#' binomial example, hygienist height - figure 1 ( prior )
#' application du modèle conjugué beta-binomial    
#'
#' Information à priori : proba moyenne 0.3, sd = 0.1

     
#+ binomial4 , echo  = FALSE
    
    mu <- 0.3
    
    sigma <- 0.12
    
    beta0 <- mu - 1 + (mu*(1-mu)^2/sigma^2)
    
    alpha0 <- (beta0*mu)/(1-mu)
    
    
    myseq <- seq( from = 0 , to = 1 , by = 0.1 ) 
    
    mydata <- data.frame( x = myseq , y =  dbeta(  shape1 = alpha0 , shape2 = beta0 , x = myseq ) )
    
    mydata$y <- mydata$y/sum(mydata$y) 
    
    p <- ggplot( data = mydata, aes( x = x , y = y ))     
    
    p <- p + geom_segment( aes( x = x , xend = x , y = 0 , yend = y))
    
    p <- p + theme_solarized()
    
    p <- p +  xlab('Proportion des hygiénistes')+
      ylab('Probabilité')+
      theme(axis.title.x=element_text(size=14))+
      theme(axis.title.y=element_text(size=14,angle=90))+
      theme(axis.text.x=element_text(size=12 , hjust = 0.5))+
      theme(axis.text.y=element_text(size=12 , angle=90 ,  hjust = 0.5))
    
    p <- p + scale_y_continuous(expand = c(0 , 0.01) , 
                                limits = c(0,0.50),
                                labels = scales::number_format(accuracy = 0.01,
                                                               decimal.mark = ',') )
    p <- p + scale_x_continuous(breaks = seq( from = 0 , to = 1 , by = 0.1 ) )
    
    p
    
    
#' vraisemblance : 11 succès sur 27 tirages
    
    
#+ binomial5 , echo  = FALSE
    

    myseq <- seq( from = 0 , to = 1 , by = 0.1 ) 
    
    mydata <- data.frame( x = myseq , y =  dbinom( x = 11 , size = 27 , prob = myseq ) )
    
    mydata$y <- mydata$y/sum(mydata$y) 
    
    p <- ggplot( data = mydata, aes( x = x , y = y ))     
    
    p <- p + geom_segment( aes( x = x , xend = x , y = 0 , yend = y))
    
    p <- p + theme_solarized()
    
    p <- p +  xlab('Proportion des hygiénistes')+
      ylab('Probabilité')+
      theme(axis.title.x=element_text(size=14))+
      theme(axis.title.y=element_text(size=14,angle=90))+
      theme(axis.text.x=element_text(size=12 , hjust = 0.5))+
      theme(axis.text.y=element_text(size=12 , angle=90 ,  hjust = 0.5))
    
    p <- p + scale_y_continuous(expand = c(0 , 0.01) , 
                                limits = c(0,0.50),
                                breaks = seq( from = 0 , to = 1 , by = 0.1 ),
                                labels = scales::number_format(accuracy = 0.01,
                                                               decimal.mark = ',') )
    
    p <- p + scale_x_continuous(breaks = seq( from = 0 , to = 1 , by = 0.1 ) )
    
    
    p 
    
    
#' Information à posteriori
    
    
#+ binomial6 , echo  = FALSE
    
    mu <- 0.3
    
    sigma <- 0.12
    
    beta <- beta0 + 27 - 11
    
    alpha <- alpha0 + 11
    
    myseq <- seq( from = 0 , to = 1 , by = 0.1 ) 
    
    mydata <- data.frame( x = myseq , y =  dbeta(  shape1 = alpha , shape2 = beta , x = myseq ) )
    
    mydata$y <- mydata$y/sum(mydata$y) 
    
    p <- ggplot( data = mydata, aes( x = x , y = y ))     
    
    p <- p + geom_segment( aes( x = x , xend = x , y = 0 , yend = y))
    
    p <- p + theme_solarized()
    
    p <- p +  xlab('Proportion des hygiénistes')+
      ylab('Probabilité')+
      theme(axis.title.x=element_text(size=14))+
      theme(axis.title.y=element_text(size=14,angle=90))+
      theme(axis.text.x=element_text(size=12 , hjust = 0.5))+
      theme(axis.text.y=element_text(size=12 , angle=90 ,  hjust = 0.5))
    
    p <- p + scale_y_continuous(expand = c(0 , 0.01) , 
                                limits = c(0,0.50),
                                labels = scales::number_format(accuracy = 0.01,
                                                               decimal.mark = ',') )
    p <- p + scale_x_continuous(breaks = seq( from = 0 , to = 1 , by = 0.1 ) )
    
    p
    
    
#' vérification numérique entre le calcul empirique et théorique

#+ binomial7 , echo  = FALSE
    
    myseq <- seq( from = 0 , to = 1 , by = 0.1 ) 
    
    mydata <- data.frame( x = seq( from = 0 , to = 1 , by = 0.1 ) , 
                          
                          y.prior  =  dbeta(  shape1 = alpha0 , shape2 = beta0 , x = myseq ),
                          
                          y.lik = dbinom( x = 11 , size = 27 , prob = myseq ),
                          
                          y.post.theo  =  dbeta(  shape1 = alpha , shape2 = beta , x = myseq ))
    
    mydata$y.post.theo.s <- mydata$y.post.theo/sum(mydata$y.post.theo)
    
    mydata$y.post.emp <- mydata$y.prior * mydata$y.lik 
    
    mydata$y.post.emp <- mydata$y.post.emp/sum(mydata$y.post.emp)
    
    knitr::kable(mydata)