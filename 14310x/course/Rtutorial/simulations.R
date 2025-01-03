#define a vector
states<-c("Washington", "Massachusetts", "California")
#sample from the vector
sample(x=states, size=10, replace = TRUE)
sample(x=states, size=2, replace = FALSE)

#sample from a normal distribution
rnorm(n=100)
plot(density(rnorm(n=1000000)))

#for loops
coin<-c("heads", "tails")
toss<-c()

for (i in 1:100){
  toss[i]<-sample(x=coin, size=1)
}
table(toss)

marital<-c("single", "married")
income<-1:4
results<-matrix(nrow=100, ncol=3,data = NA)
colnames(results)<-c("marital", "income", "state")
head(results)

for (i in 1:100){
  results[i,1]<-sample(x=marital, size=1)
  results[i,2]<-sample(x=income, size=1)
  results[i,3]<-sample(x=states, size=1)
}
head(results)
apply(X = results, MARGIN = 2, FUN = table)
