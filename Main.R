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

modelo1 = glm(formula=mydata$Attrition~mydata$DistanceFromHome+mydata$Age, family=binomial,data=mydata,control=list(maxit=50))

summary(modelo1)


predict(modelo1)

#Estimação dos parametros usando Método de Máxima Verossimilhança
modelo3=glm(formula = mydata$Attrition~mydata$MonthlyIncome,family=binomial(link="logit"));modelo3

#estimativas pontuais e erros padrão
summary(modelo3)

OR1=exp(modelo3$coefficients);OR1

ICbeta1=confint.default(modelo3,level=0.95);ICbeta1

ICOR1=exp(ICbeta1);ICOR1

round((cbind(OR1, ICOR1)),3)


##################YOUTUBE
dados <- read.csv("IBM.csv")
View(dados)

dados$Attrition <- as.character(dados$Attrition)
dados$Attrition[dados$Attrition == "Yes"] <- "1"
dados$Attrition[dados$Attrition == "No"] <- "0"
dados$Attrition <- as.numeric(dados$Attrition)

ggplot(dados, aes(x=Attrition, y=)) + geom_point() + geom_smooth(method = "glm", method.args = list(family="binomial"), se = TRUE)

model <- glm(Attrition ~ Gender, family = "binomial", data = dados)

summary(model)

exp(model$coefficients)
#18% mais provável que um homem saia

model <- glm(Attrition ~ TotalWorkingYears, family = "binomial", data = dados)

summary(model)

exp(model$coefficients)
#18% mais provável que um homem saia


#se logit > 0 -> prob > 50, senao, menor

sex <- dados$Gender
sexcode <- ifelse(sex == "Femeale", 1, 0)

plot(dados$MonthlyIncome,jitter(dados$Attrition,0.15), pch=19)
plot(dados$DailyRate,jitter(dados$Attrition,0.15), pch=19)
plot(dados$DistanceFromHome,jitter(dados$Attrition,0.15), pch=19)
barplot(dados$TotalWorkingYears)


model <- glm(dados$Attrition~dados$MonthlyIncome, binomial)
summary(model)



#Graficos com Attrition

boxplot(dados$MonthlyIncome~dados$Attrition)#Dependente
boxplot(dados$Age~dados$Attrition)
plot(dados$BusinessTravel)
boxplot(dados$DailyRate~dados$Attrition)
plot(dados$Department~dados$Attrition)
boxplot(dados$DistanceFromHome~dados$Attrition) #Dependente
boxplot(dados$Education~dados$Attrition)
plot(dados$EducationField~dados$Attrition)
boxplot(dados$EnvironmentSatisfaction~dados$Attrition) #Dependente
plot(dados$Gender~dados$Attrition)
boxplot(dados$HourlyRate~dados$Attrition)
boxplot(dados$JobInvolvement~dados$Attrition)
plot(dados$JobRole~dados$Attrition)
plot(dados$JobLevel$dados$Attrition)
boxplot(dados$JobSatisfaction~dados$Attrition) #Dependente
plot(dados$MaritalStatus~dados$Attrition)
boxplot(dados$MonthlyRate~dados$Attrition)
boxplot(dados$NumCompaniesWorked~dados$Attrition) #Dependente
boxplot(dados$PerformanceRating~dados$Attrition)
plot(dados$RelationshipSatisfaction~dados$Attrition)
plot(dados$StandardHours~dados$Attrition)
boxplot(dados$StockOptionLevel~dados$Attrition)#dependente
boxplot(dados$TotalWorkingYears~dados$Attrition)#dependente
boxplot(dados$TrainingTimesLastYear~dados$Attrition)
boxplot(dados$WorkLifeBalance~dados$Attrition)
boxplot(dados$YearsAtCompany~dados$Attrition)
boxplot(dados$YearsInCurrentRole~dados$Attrition)
boxplot(dados$YearsSinceLastPromotion~dados$Attrition)
boxplot(dados$YearsWithCurrManager~dados$Attrition) #Dependente

#Vendo como se comporta a dispersão de Attrition com base no total de antos trabalhados
plot(jitter(dados$Attrition,0.15)~dados$TotalWorkingYears)

modeloDados <- glm(dados$Attrition~dados$TotalWorkingYears+dados$Gender+dados$NumCompaniesWorked, dados$MonthlyIncome, binomial)

barplot(table(dados$Attrition,dados$Age, col=cores))

summary(modeloDados)

