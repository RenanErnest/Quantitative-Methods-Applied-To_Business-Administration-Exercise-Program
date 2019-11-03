mydata <- read.csv("IBM.csv")
library(dplyr)
library(ggplot2)
library(ggthemes)
install.packages('rms')
require(rms)
View(mydata)

cores <- c(2)

summary(mydata$DistanceFromHome)
plot(mydata$DistanceFromHome, pch=20, col=cores)

summary(mydata$Age)
plot(mydata$Age, pch=20, col=cores)

summary(mydata$Gender)
plot(mydata$Gender, pch=20, col=cores)

summary(mydata$MaritalStatus)
plot(mydata$MaritalStatus, pch=20, col=cores)

summary(mydata$MonthlyIncome)
plot(mydata$MonthlyIncome, pch=20, col=cores)

summary(mydata$JobRole)
plot(mydata$JobRole, pch=20, col=cores)

summary(mydata$TotalWorkingYears)
plot(mydata$TotalWorkingYears, pch=20, col=cores)


#convertendo Yes para 1 e No para 0
mydata$Attrition <- as.character(mydata$Attrition)
mydata$Attrition[mydata$Attrition == "Yes"] <- "1"
mydata$Attrition[mydata$Attrition == "No"] <- "0"
mydata$Attrition <- as.numeric(mydata$Attrition)

summary(mydata$Attrition)
plot(mydata$Attrition, pch=20, col=cores)



#correlações
cor(mydata$Attrition,mydata$Age) #-0.159205
cor(mydata$Attrition,mydata$DistanceFromHome) #0.07792358
cor(mydata$Attrition,mydata$TotalWorkingYears) #-0.1710632
cor(mydata$Attrition,mydata$MonthlyIncome) #-0.1598396

y <- Attrition~mydata$Age+mydata$DistanceFromHome+mydata$TotalWorkingYears+mydata$MonthlyIncome+mydata$JobRole+mydata$MaritalStatus+mydata$Gender

model<-lrm(y,data=mydata)#error
model


#ln(p/1-p)=a+bx
#p=1/1+e^-(a+bx)
#https://www.youtube.com/watch?v=CVL5vj1N1U8&list=PL5sf7vx2dHF7rPQ4eeNRtCcFs6flK4tnW&index=13 (min 27+33), precisa terminar a implementação (ver se modelo é viável e plotar os gráficos se for)
#resumao escrito http://www.estatisticacomr.uff.br/?p=598

glm(formula=mydata$Attrition~mydata$DistanceFromHome+mydata$Age, family=binomial,data=mydata,control=list(maxit=50))
