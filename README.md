# trabajo-final-cuanititiva
### 2. Instalar estos Paquetes 
install.packages("tidyverse")
install.packages("dplyr")
install.packages("plyr")
install.packages("tidyr")
install.packages("mlogit")
install.packages("stargazer")
install.packages("rsq")
install.packages("sjPlot")
install.packages("dslabs")

### 2.1 Cargar desde la biblioteca estos Paquetes
library(tidyverse)
library(dplyr)
library(plyr)
library(tidyr)
library(mlogit)
library(stargazer)
library(rsq)
library(sjPlot)
library(dslabs)

## 2. Encuesta CEP
### 2.1 Link: https://www.cepchile.cl/cep/encuestas-cep/encuestas-2010-2021/estudio-nacional-de-opinion-publica-n-85-septiembre-2021

### 2.2 Importar base de datos
library(haven)
base_85 <- read_sav("C:/Users/Alex/Desktop/Magíster/Segundo semestre/Metodologías Cuantitativas/R/encuesta_cep_ago2021/base_85.sav")
View(base_85)

# 2.3Variable numérica
base_85[sapply(base_85, is.numeric)] <- lapply(base_85[sapply(base_85, is.numeric)], as.factor)

## 3. Análisis descriptivo

### 3.1 Frecuencias

### Variables Independientes: info_enc_56
table(base_85$info_enc_56)

### Variables Independientes: religion_1
table(base_85$religion_1)

### Variables Independientes: religion_1
table(base_85$bienestar_21)

### Variable dependiente: 
table(base_85$bienestar_2)
table_A <- table(base_85$bienestar_2)
prop.table(table_A)


## 5. Recodificar y crear una nueva variable 

## 5.1 Variable dummy RELIGIÓN
table_7 <- table(base_85$religion_1.Dummy)
prop.table(table_7)

base_85$religion_1.Dummy<-ifelse(base_85$religion_1=="9",1,0)
table(base_85$religion_1.Dummy)

base_85$info_enc_56.Dummy<-ifelse(base_85$info_enc_56=="1",1,0)
table(base_85$bienestar_2_R.Dummy)

base_85["Trabajo"] <- as.numeric(as.character(base_85$info_enc_56))

base_85$Trabajo.Dummy<-ifelse(base_85$Trabajo=="1",1,0)
table(base_85$Trabajo.Dummy)

base_85["Ahorro"] <- as.numeric(as.character(base_85$bienestar_21))

base_85$Ahorro.Dummy<-ifelse(base_85$Ahorro=="1",1,0)
table(base_85$Ahorro.Dummy)


## 5.2 Recodificar variables de preferencias --MULTIPLE

## 1: Recodificación variable "Satisfacción con la vida personal".
table(base_85$bienestar_2)
base_85$bienestar_2_R = revalue(base_85$bienestar_2, c("1"="1", "2"="1","3"="1", "4"="1", "5"="1", "6"="0", "7"="0", "8"="0","9"="0", "10"="0", "88"="0", "99"="0"))
table(base_85$bienestar_2_R)


## 6. Análisis de correlación: Repaso 

library(corrplot)

cor.test(as.numeric(base_85$bienestar_2), as.numeric(base_85$religion_1.Dummy), 
         method=c("pearson"))

cor.test(as.numeric(base_85$bienestar_2), as.numeric(base_85$info_enc_56), 
         method=c("pearson"))

cor.test(as.numeric(base_85$bienestar_2), as.numeric(base_85$bienestar_21), 
         method=c("pearson"))

## 7. Análisis de causalidad

### 7.1. Definir variables para regresión lineal ***** Ejemplo ya que 
### la variable dependiente tiene que ser continua

##### 7.1.1. Variable Dependiente: base_85$bienestar_2_R

# Variable Independiente  1: Religión
# Variable Independiente  2: Ocupación
# Variable Independiente  3: Ingreso

##### 7.1.2. Variables numéricas

###### Religión: Religión_N_2

base_85["Religión_N_2"] <- as.numeric(as.character(base_85$religion_1.Dummy))

###### Ocup: Ocupación

base_85["Ocup"] <- as.numeric(as.character(base_85$info_enc_56))


###### ING: INGRESO

base_85["ING"] <- as.numeric(as.character(base_85$bienestar_21))

###### 7.1.3. Definir X, Y 

Y <- cbind(base_85$bienestar_2_R)
X1 <- cbind(base_85$`Religión_N_2`)
X <- cbind(base_85$`Religión_N_2`, base_85$info_enc_56, base_85$bienestar_21)

###### 7.1.4. Análisis de correlación

cor(X,Y)
cor(X1,Y)

###### 7.1.5. Gráfico de relación
plot(Y ~ X1, data = base_85)

## 8. Regresión Lineal SImple

olsreg1 <- lm(Y ~ X1)
summary(olsreg1)
confint(olsreg1, level = 0.95)
anova(olsreg1)

### 8.1. Graficando la regresión
abline(olsreg1)

### 8.2. Regresión 2

olsreg2 <- lm(Y ~ X)
summary(olsreg2)
confint(olsreg2, level = 0.95)
anova(olsreg2)

### 8.3. Graficando la regresión 2
abline(olsreg2)

### 9. Modelos de regresion usando stargazer

stargazer(olsreg1, olsreg2, title = "Bienestar", 
          align =TRUE, out = "Bienestar1.txt")

### 10. Correr Regresion Logistica Binaria 

### 10.1. Definir variables para regresión logistica

##### 10.1.1. Variable Dependiente: base_85$bienestar_2_R

# Variable Independiente  1: Ocupación
# Variable Independiente  2: Religión
# Variable Independiente  3: Ingreso

table(base_85$bienestar_2_R)

m3 = glm(base_85$bienestar_2_R ~ X, data = base_85,
         binomial())
summary(m3)

# 11. Instalar funcion de bondad de ajuste
install.packages("rsq")

library(rsq)

# 12. Calcular bondad de ajuste
rsq(m3)

# 13. Tabular Regresion Logistica Binaria
stargazer(m3, title = "Modelo 3", align =TRUE, out = "resultados2.txt")

# 14 Modelo adicional

m4 = glm(base_85$bienestar_2_R ~ X, data = base_85$religion_1.Dummy,
         data = binomial())
summary(m4)

stargazer(olsreg1, olsreg2, title = "Bienestar", 
          align =TRUE, out = "Bienestar4.txt")

m4 = glm(base_85$bienestar_2_R ~ X, data = base_85$sexo,
         data =
           binomial())
summary(m4)

# 19. 2 modelos adicional m7 y m8 sin cbind()

## 19.3 Variable dummy 1 candidato -- BINARIA


base_85["age_N-2"] <- as.numeric(as.character(base_85$edad)) 

base_85["GSE-N"] <- as.numeric(as.character(base_85$gse))


m7 = glm(base_85$bienestar_2_R ~ base_85$religion_1.Dummy + 
           base_85$Trabajo.Dummy + base_85$Ahorro.Dummy,
         data = base_85,
         binomial())
summary(m7)

m8 = glm(base_85$bienestar_2_R ~ base_85$religion_1.Dummy + 
           base_85$Trabajo.Dummy + base_85$Ahorro.Dummy + base_85$sexo,
         data = base_85,
         binomial())
summary(m8)


# 20. Tabular Regresion Logistica Binaria de m7 y m8
stargazer(m7, m8, title = "Modelo 7 y 8", align =TRUE, 
          out = "resultado definitivo.txt")
