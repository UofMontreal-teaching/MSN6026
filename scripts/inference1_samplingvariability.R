#' ---
#' title: "Illustration de la variabilité échantillonnale"
#' author: "Jérôme Lavoué"
#' date: "sept 2023"
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

mydata <- data.frame( x = seq( from = 40 , to = 100 , length.out = 1000  ) )

mydata$dens <- dnorm( mydata$x , mean = 70 , sd = sqrt(70) )

p <- ggplot(data = mydata )

p <- p + geom_line( aes( x = x , y = dens ) , linewidth = 1.5)

p <- p + theme_calc()

p <- p +  theme(axis.text.x=element_text(size=14),axis.title.x=element_text(size=16,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=16,vjust=+0.55))+
  theme(axis.ticks = element_line(linewidth = 1) , axis.ticks.length.x = unit(.25, "cm"), axis.ticks.length.y = unit(.25, "cm"))+
  labs(x=expression ( "") , y="Densité")+
    theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())

p <- p + scale_y_continuous(expand = c(0, 0) , 
                            limits = c(0,0.06),
                            labels = scales::number_format(accuracy = 0.001,
                                                           decimal.mark = ',') ) 

p <- p + scale_x_continuous( limits = c(40,100), )  

p

#ggsave("specific graphs/figures/april2022_lognormwith80percCI.png",width = 20, height = 12, units = "cm",dpi=300)



#+ graphmeann10,echo = FALSE,warning=FALSE, message = FALSE

n.sim <- 5000

n <- 10

mydata <- rnorm( n.sim*n , mean = 70 , sd= sqrt(70) )

mydata <- matrix( mydata , nrow = n.sim)

mean_vector <- apply( mydata , 1 , mean)

var_vector <- apply( mydata , 1 , var)

mydata <- data.frame( mean = mean_vector , var = var_vector )


p <- ggplot(data = mydata )

p <- p + geom_histogram( aes( x=mean ) , color = "white" , fill = "blue")

p <- p + theme_solarized()

p <- p +  theme(axis.text.x=element_text(size=14),axis.title.x=element_text(size=16,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=16,vjust=+0.55))+
  theme(axis.ticks = element_line(linewidth = 1) , axis.ticks.length.x = unit(.25, "cm"), axis.ticks.length.y = unit(.25, "cm"))+
  labs(x=expression ( "") , y="")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p <- p + scale_x_continuous( limits = c(40,100), )  


p



#+ graphvarn10,echo = FALSE,warning=FALSE, message = FALSE

p <- ggplot(data = mydata )

p <- p + geom_histogram( aes( x=var ) , color = "white" , fill = "blue")

p <- p + theme_solarized()

p <- p +  theme(axis.text.x=element_text(size=14),axis.title.x=element_text(size=16,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=16,vjust=+0.55))+
  theme(axis.ticks = element_line(linewidth = 1) , axis.ticks.length.x = unit(.25, "cm"), axis.ticks.length.y = unit(.25, "cm"))+
  labs(x=expression ( "") , y="")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


p



#+ graphmeann3,echo = FALSE,warning=FALSE, message = FALSE

n.sim <- 5000

n <- 3

mydata <- rnorm( n.sim*n , mean = 70 , sd= sqrt(70) )

mydata <- matrix( mydata , nrow = n.sim)

mean_vector <- apply( mydata , 1 , mean)

var_vector <- apply( mydata , 1 , var)

mydata <- data.frame( mean = mean_vector , var = var_vector )


p <- ggplot(data = mydata )

p <- p + geom_histogram( aes( x=mean ) , color = "white" , fill = "blue")

p <- p + theme_solarized()

p <- p +  theme(axis.text.x=element_text(size=14),axis.title.x=element_text(size=16,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=16,vjust=+0.55))+
  theme(axis.ticks = element_line(linewidth = 1) , axis.ticks.length.x = unit(.25, "cm"), axis.ticks.length.y = unit(.25, "cm"))+
  labs(x=expression ( "") , y="")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p <- p + scale_x_continuous( limits = c(40,100), )  


p


#+ graphmeann30,echo = FALSE,warning=FALSE, message = FALSE

n.sim <- 5000

n <- 30

mydata <- rnorm( n.sim*n , mean = 70 , sd= sqrt(70) )

mydata <- matrix( mydata , nrow = n.sim)

mean_vector <- apply( mydata , 1 , mean)

var_vector <- apply( mydata , 1 , var)

mydata <- data.frame( mean = mean_vector , var = var_vector )


p <- ggplot(data = mydata )

p <- p + geom_histogram( aes( x=mean ) , color = "white" , fill = "blue")

p <- p + theme_solarized()

p <- p +  theme(axis.text.x=element_text(size=14),axis.title.x=element_text(size=16,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=16,vjust=+0.55))+
  theme(axis.ticks = element_line(linewidth = 1) , axis.ticks.length.x = unit(.25, "cm"), axis.ticks.length.y = unit(.25, "cm"))+
  labs(x=expression ( "") , y="")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p <- p + scale_x_continuous( limits = c(40,100), )  


p
