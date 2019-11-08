mydata <- read.csv("IBM.csv")
mydataT <- read.csv("IBMTraduzido.csv")
library(dplyr)
library(ggplot2)
library(ggthemes)
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

glm(formula=mydata$Attrition~mydata$DistanceFromHome+mydata$Age, family=binomial,data=mydata,control=list(maxit=50))
