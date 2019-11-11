mydata <- read.csv("IBM.csv")
mydataT <- read.csv("IBMTraduzido.csv")
library(dplyr)
library(ggplot2)
library(ggthemes)
library(caTools)
library(e1071)
library(glmnet)
install.packages('rms')
require(rms)
View(mydataT)

# ------------------------ Analise Exploratoria de dados ------------------------
str(mydataT)

dim(mydataT)

#desconsideradas: 4:DailyRate, 9:EmployeeCount, 13:HourlyRate, 20:MonthlyRate, 22:Over18
numeric_mydata <- mydataT[,c(1,6,7,10,11,14,15,17,19,21,24,25,26,28:35)] # coletando somente as variaveis numericas
numeric_Attrition = as.numeric(mydataT$Evasao)- 1 # transferindo Sim para 1 e Nao para 0
numeric_mydata = cbind(numeric_mydata, numeric_Attrition) # juntando o novo Attrition com as variaveis numericas
str(numeric_mydata)
install.packages("corrplot") # instalando biblioteca de matriz de correlacao para apresentacoes
library(corrplot) # importando a biblioteca
M <- cor(numeric_mydata) # guardando a matriz de correlacao em M
M
columncolor <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white", "cyan", "#007FFF", "blue", "#00007F"))
corrplot(M, type = "upper", method = "color", col = columncolor(25), bg = "gold2") # plotando a matriz

#Achando quantas correlacoes sao otimas, ou seja, com valores maiores ou iguais 0.75
k = 0
for(i in 1:22){
  for(r in i:22){
    if(M[i,r]>= 0.75 & i != r){
      k= k + 1
      print(paste0(colnames(M)[i], " vs ", colnames(M)[r], " = ", M[i,r]))
    }
  }
}
print(k) # = 6

#acima de 0.75
columncolor <- colorRampPalette(c("#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#000000"))
corrplot(M, type = "upper", method = "color", col = columncolor(8)) # plotando a matriz

l <- ggplot(mydataT, aes(Cargo,fill = Evasao))
l <- l + geom_histogram(stat="count")
print(l)

### Hora Extra vs Evasao
#Quantidade
l <- ggplot(mydataT, aes(HoraExtra,fill = Evasao)) +
  geom_histogram(stat="count") + ylab("Quantidade") + 
  theme(text = element_text(size=20))
#Proporcao
l <- ggplot(mydataT, aes(HoraExtra, fill = Evasao)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + ylab("Proporção") +
  theme(text = element_text(size=20))
print(l)
tapply(as.numeric(mydataT$Evasao) - 1 ,mydataT$HoraExtra,mean)
#Teste estatistico
tbl = table(mydataT$HoraExtra, mydataT$Evasao)
chisq.test(tbl)

### Estado Civil vs Evasao
#Quantidade
l <- ggplot(mydataT, aes(EstadoCivil,fill = Evasao)) +
  geom_histogram(stat="count") + ylab("Quantidade") + 
  theme(text = element_text(size=20))
#Proporcao
l <- ggplot(mydataT, aes(EstadoCivil, fill = Evasao)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + ylab("Proporção") +
  theme(text = element_text(size=20))
print(l)
tapply(as.numeric(mydataT$Evasao) - 1 ,mydataT$EstadoCivil,mean)
#Teste estatistico
tbl = table(mydataT$EstadoCivil, mydataT$Evasao)
chisq.test(tbl)

### Cargo vs Evasao
#Quantidade
l <- ggplot(mydataT, aes(Cargo,fill = Evasao)) +
  geom_histogram(stat="count") + ylab("Quantidade") + 
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1))
#Proporcao
l <- ggplot(mydataT, aes(Cargo, fill = Evasao)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + ylab("Proporção") +
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1))
print(l)
tapply(as.numeric(mydataT$Evasao) - 1 ,mydataT$HoraExtra,mean)
#Teste estatistico
tbl = table(mydataT$Cargo, mydataT$Evasao)
chisq.test(tbl)

### Genero vs Evasao
#Quantidade
l <- ggplot(mydataT, aes(Genero,fill = Evasao)) +
  geom_histogram(stat="count") + ylab("Quantidade") + 
  theme(text = element_text(size=20))
#Proporcao
l <- ggplot(mydataT, aes(Genero, fill = Evasao)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + ylab("Proporção") +
  theme(text = element_text(size=20))
print(l)
tapply(as.numeric(mydataT$Evasao) - 1 ,mydataT$Genero,mean)
#Teste estatistico
tbl = table(mydataT$Genero, mydataT$Evasao)
chisq.test(tbl)

### Area de Estudo vs Evasao
#Quantidade
l <- ggplot(mydataT, aes(AreaEstudo,fill = Evasao)) +
  geom_histogram(stat="count") + ylab("Quantidade") + 
  theme(text = element_text(size=20))
#Proporcao
l <- ggplot(mydataT, aes(AreaEstudo, fill = Evasao)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + ylab("Proporção") +
  theme(text = element_text(size=20))
print(l)
tapply(as.numeric(mydataT$Evasao) - 1 ,mydataT$AreaEstudo,mean)
#Teste estatistico
tbl = table(mydataT$AreaEstudo, mydataT$Evasao)
chisq.test(tbl)

### Departamento vs Evasao
l <- ggplot(mydataT, aes(Departamento,fill = Evasao)) +
  geom_histogram(stat="count") + ylab("Quantidade") + 
  theme(text = element_text(size=20))
#Proporcao
l <- ggplot(mydataT, aes(Departamento, fill = Evasao)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + ylab("Proporção") +
  theme(text = element_text(size=20))
print(l)
tapply(as.numeric(mydataT$Evasao) - 1 ,mydataT$Departamento,mean)
#Teste estatistico
tbl = table(mydataT$Departamento, mydataT$Evasao)
chisq.test(tbl)

### Viagem a Negocios vs Evasao
#Quantidade
l <- ggplot(mydataT, aes(ViagemNegocios,fill = Evasao)) +
  geom_histogram(stat="count") + ylab("Quantidade") + 
  theme(text = element_text(size=20))
#Proporcao
l <- ggplot(mydataT, aes(ViagemNegocios, fill = Evasao)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + ylab("Proporção") +
  theme(text = element_text(size=20))
print(l)
tapply(as.numeric(mydataT$Evasao) - 1 ,mydataT$ViagemNegocios,mean)
#Teste estatistico
tbl = table(mydataT$ViagemNegocios, mydataT$Evasao)
chisq.test(tbl)

### x=HoraExtra, y= Idade, z = EstadoCivil , t = Evasao
ggplot(mydataT, aes(HoraExtra, Idade)) +  
  facet_grid(.~EstadoCivil) +
  geom_jitter(aes(color = Evasao),alpha = 0.5, size = 5) +  
  ggtitle("x=HoraExtra, y= Idade, z = EstadoCivil , t = Evasao") +
  theme_light() +
  theme(text = element_text(size = 20))

### Salario vs. Idade, cor = Evasao
ggplot(mydataT, aes(SalarioMensal, Idade, color = Evasao)) + 
  geom_jitter(size = 3, alpha = 0.7) +
  ggtitle("SalarioMensal vs. Idade, cor = Evasao ") +
  theme_light() +
  theme(text = element_text(size = 20))

# -------------------------------------------------------------------------------

# --------------------------- Inferencia Estatistica ----------------------------

library(caTools)
library(e1071)
library(glmnet)

#Verificando variaveis
View(mydataT)
cdplot(Evasao~AnosCompanhia, data=mydataT)
cdplot(Evasao~Idade, data=mydataT)
cdplot(Evasao~SalarioMensal, data=mydataT)
cdplot(Evasao~NivelTrabalho, data=mydataT)

mydatanew = mydataT[,c(2,3,6,18,21,23)] # escolhendo variaveis correlacionadas com a evasao e um pouco indepentendes entre si
str(mydatanew)
mydatanew = mydataT[,-c(6,9,22)] # Todas exceto distancia de casa, as descartadas citadas no comeco
str(mydatanew)

# Divindo os dados em treino e teste
split <- sample.split(mydatanew$Evasao, SplitRatio = 0.80) 
train <- subset(mydatanew, split == T) 
test <- subset(mydatanew, split == F)
View(split)

# Utilizando a Regressao Logistica
model_glm <- glm(Evasao ~ ., data = train, family='binomial') 
cdplot(Evasao~ViagemNegocios, data=mydataT)

# Predizendo em casos de teste
predicted_glm <- predict(model_glm, test, type='response')
predicted_glm <- ifelse(predicted_glm > 0.5,1,0)
str(mydataT)
summary(model_glm)
print(model_glm)
# Matriz de Confusao
t = table(test$Evasao, predicted_glm)

# Porcentagem/Chance de Acerto
print( (t[1,"0"] + t[2,"1"]) / count(test)) # Acertos da matriz de confusao dividido pelo numero de elementos
# -------------------------------------------------------------------------------

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



#Graficos que auxiliaram na análise

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


cor(dados$NumCompaniesWorked,dados$TotalWorkingYears)


modeloDados <- glm(dados$Attrition~dados$BusinessTravel+dados$NumCompaniesWorked+dados$DistanceFromHome+dados$MaritalStatus+dados$OverTime, binomial)

barplot(table(dados$Attrition,dados$Age, col=cores))

summary(modeloDados)

modeloDados$coefficients

cor(modeloDados$coefficients,method=c("pearson"))

l <- ggplot(mydata, aes(OverTime,fill = Attrition))
l <- l + geom_histogram(stat="count")
print(l)
tapply(as.numeric(dados$Attrition) - 1 ,dados$OverTime,mean)
tapply(as.numeric(mydata$Attrition) - 1 ,mydata$MaritalStatus,mean)
tapply(as.numeric(mydata$Attrition) - 1 ,mydata$JobRole,mean)

OR1=exp(modeloDados$coefficients);OR1

ICbeta1=confint.default(modeloDados,level=0.95);ICbeta1

ICOR1=exp(ICbeta1);ICOR1

dados2 = dados[,-c(6,9,22)] #excui colunas desnecessarias
str(dados2)
split <- sample.split(dados2$Attrition, SplitRatio = 0.80) #ótimo
train <- subset(dados2, split == T) 
test <- subset(dados2, split == F)

model_glm <- glm(Attrition ~ ., data = train, family='binomial') 
predicted_glm <- predict(model_glm, test, type='response')
predicted_glm <- ifelse(predicted_glm > 0.5,1,0)

table(test$Attrition, predicted_glm)
print((237+22)/294)

glm(formula=mydata$Attrition~mydata$DistanceFromHome+mydata$Age, family=binomial,data=mydata,control=list(maxit=50))


#regressão logística glm com todas as variáveis, usada para decidir quais variáveis são as mais relevantes 
#(veio antes da ideia de fazer testes de hipótese em cada variável)
modelo1 = glm(formula = mydata$Attrition~mydata$Age+mydata$BusinessTravel+mydata$DailyRate+mydata$Department + mydata$DistanceFromHome+ mydata$Education+ mydata$EducationField+ mydata$EmployeeCount+ mydata$EmployeeNumber+ mydata$EnvironmentSatisfaction+ mydata$Gender+ mydata$HourlyRate+ mydata$JobInvolvement+ mydata$JobLevel+ mydata$JobSatisfaction+ mydata$MaritalStatus+ mydata$MonthlyIncome+ mydata$MonthlyRate+ mydata$NumCompaniesWorked+ mydata$OverTime+ mydata$PercentSalaryHike+ mydata$PerformanceRating+ mydata$RelationshipSatisfaction+ mydata$StandardHours+ mydata$StockOptionLevel+ mydata$TotalWorkingYears+ mydata$TrainingTimesLastYear+ mydata$WorkLifeBalance+ mydata$YearsAtCompany+ mydata$YearsInCurrentRole+ mydata$YearsSinceLastPromotion+ mydata$YearsWithCurrManager, family=binomial(link = logit ),data=mydata,control=list(maxit=50))
summary(modelo1)