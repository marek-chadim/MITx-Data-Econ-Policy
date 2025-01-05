# Preliminaries
#-------------------------------------------------
#install.packages('perm')
library("Matrix")
#install.packages('lfe')
library("lfe")

rm(list = ls())

qiandata <- read.csv('14310x/data/qian.csv')
qiandata$post <- (qiandata$biryr >= 1979)

qiandata$teapost <- qiandata$teasown * qiandata$post
summary(qiandata)

model1 <- lm(sex ~ teasown + post + teapost, data = qiandata)
summary(model1)

model2 <- felm(sex ~ teapost + post | factor(admin), data = qiandata)
summary(model2)
