#install.packages("ggplot2")
library(ggplot2)

#scatterplot
cars
myData <- cars
ggplot(myData, aes(x=speed, y=dist)) + geom_point()
g<-ggplot(myData, aes(x=speed, y=dist)) + geom_point()
g
gg<-g+coord_cartesian(xlim=c(10,15),ylim=c(25,50))
gg

#histogram
mtcars
myData <- mtcars
ggplot(myData, aes(x=wt)) + geom_histogram(binwidth=.5)
h<-ggplot(myData, aes(x=wt)) + geom_histogram(binwidth=.5,color="black",fill="white")
h
h+geom_vline(xintercept=mean(myData$wt),color="red",linetype="dashed",size=1)

ggplot(myData, aes(x=wt)) + 
  geom_histogram(binwidth=.5,aes(y=after_stat(density)),alpha=.3,color="black",fill="white")+
  geom_density(alpha=.2,fill="red")
