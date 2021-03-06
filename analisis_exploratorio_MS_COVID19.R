# Bibliotecas -------------------------------------------------------------
install.packages("readr")
install.packages("dplyr")
install.packages("knitr")
install.packages("anchors")
install.packages("survival")
install.packages("broom")
install.packages("ggfortify")
install.packages("stargazer")
install.packages("skimr")
install.packages("rio")
install.packages("ggplot2",dependencies = T)
install.packages("data.table")
install.packages("pivottabler")
install.packages("crayon")
install.packages("corrplot")

library(corrplot)
library(crayon)
library(pivottabler)
library(data.table)
library(ggplot2)
library(rio)
library(skimr)
library(stargazer)
library(ggfortify)
library(broom)
library(survival)
library(anchors)
library(knitr)
library(dplyr)
library(readr)

# Directorio de trabajo y dataset ---------------------------------------------------
getwd()
setwd("C:/Users/Aaron/Desktop/2021-1/Modelos de supervivencia y series de tiempo/Proyectos/Modelos_COVID")

#Como estos datos e modificaron, se van a ocupar, los de la varible 'datos.covid.2'
datos.covid <- read.csv("201201COVID19MEXICO.csv")

# Definimos los que han muerto --------------------------
"
Esta parte del código añade la variable dicotomica 'MUERTES' , esta indica si la persona ya esta muerta; 
por lo que ya no se debe correr, de hacho los datos que se ocuparan son '201201COVID19MEXICO.csv'
ya que estos ya contienen la variable 'MUERTES'
"

dim(datos.covid)
largo <- nrow(datos.covid)
MUERTE <- vector(mode = "character",length = largo)

datos.covid <- cbind(datos.covid,MUERTE)

names(datos.covid)

datos.covid <- datos.covid[,c(13,41)]
names(datos.covid)
datos.covid$FECHA_DEF <- as.character(datos.covid$FECHA_DEF)

r <- which(datos.covid$FECHA_DEF %in% "9999-99-99" )#índices de los que no han muerto
class(r)
View(r)

datos.covid <- as.matrix(datos.covid)
datos.covid[r,2] <- "NO"
View(datos.covid)
datos.covid[-r,2] <- "SÍ"

datos.covid <- as.data.frame(datos.covid)
#datos.covid[is.na(datos.covid)] <- "SÍ"
#View(datos.covid)
#export(datos.covid,"los.muesrtos.csv")

MUERTES <- datos.covid$MUERTE#lo guardamos en un vector
View(MUERTES)
class(MUERTES)
#datos.covid <- read.csv("201201COVID19MEXICO.csv")

datos.covid <- cbind(datos.covid,MUERTES)
View(datos.covid)
#export(datos.covid,"datos.2.csv")#con esta data vamos a trabajar

#Análisis exploratorio 1, con respecto a las variables ---------------------------------------------------
#A partir de aqui se comienza el analisis exploratorio del dataset : datos.covid.2' 

datos.covid.2 <- read.csv("datos.2.csv")
resumen1 <- skim(datos.covid.2)#análisis exploratorio
class(resumen1)#que tipo de objeto es esta variable
"export:
Esta funcion sirve para exportar en nustro directorio de trabajo dataframes en el formato que le digamos.
"
#export(resumen1,"análisis exploratorio de las variables.csv")

View(datos.covid.2,title = "Datos COVID19")

"unique:
Asi podemos ver los valores unicos de cada variable, esto nos sirve para ver si las categorias estan
correctas, es decir que las que sean hombre o mujer, solo sean dos categorias, etc.
De lo contarrio, se deben trabajar este tipo de situaciones.
"
unique(datos.covid.2$PAIS_NACIONALIDAD)
unique(datos.covid.2$TIPO_PACIENTE)
unique(datos.covid.2$INTUBADO)

"Tables:
Estas estructuras sirven para ver las frequencias de las variables, es decir un conteo rapido

Prop.tables:
Estas estructuras nos dicen los porcentajes de las frecuencias de las variables
"
#Gente de habla indígena
table(datos.covid.2$HABLA_LENGUA_INDIG)#cuanta gente
prop.table(table(datos.covid.2$HABLA_LENGUA_INDIG))#sus respectivos porcentajes

table(datos.covid.2$SEXO)#1 mujer, 2 hombre
prop.table(table(datos.covid.2$SEXO))
table(datos.covid.2$EMBARAZO)

#cuanta gente falleció y no fallecio
ggplot(data = datos.covid.2,aes(x=MUERTES, fill=MUERTES))+geom_bar(stat = "count")+
  geom_text(aes(label=..count..),stat="count",position=position_dodge(0.9),vjust=-0.2)+
  ggtitle("Cantidad de personas que fallecieron")

"PIVOT TABLES:
Con las Pivot tables podemos conocer cuantos regitros hay por cada nivel de las variables
"
#Cuantas personas hay por nacionalidad
pt1 <- PivotTable$new()#creamos el objeto
pt1$addData(datos.covid.2)#le decimos de donde tomara valores
pt1$addRowDataGroups("PAIS_NACIONALIDAD")#que variable seran los grupos que contara
pt1$defineCalculation(calculationName = "ID_REGISTRO",summariseExpression = "n()")#que objetos contara
pt1$renderPivot()#muestra la pivot en el área de gráficos
print(pt1)#la podemos tambien imprimir en consola

"En esta parte del analisis vamos a ver cuantas personas hay INTUBADAS con una PIVOT, pero primero debemos
convertir en N/A'S las variables necesarias, es decir los niveles 99 y 97

"
intubados <- datos.covid.2$INTUBADO
indices.na.intubados <- which(intubados %in% c(97,99) )#índices ques seran NA porque tienen estas catego

intubados <- as.matrix(intubados)
intubados[indices.na.intubados,1] <- NA#les ponemos NAS
View(indices.na.intubados)
intubados <- as.factor(intubados)
unique(intubados)#listo, niveles adecuados; ya podemos trabajar con la pivot table

datos.covid.2[,"INTUBADO"] <- intubados#reemplazamos la varibale original con la nueva que creamos con NAS
#View(datos.covid.2$INTUBADO)#podemos validar

#Ahora si creamos la pivot
pt2 <- PivotTable$new()
pt2$addData(datos.covid.2)
pt2$addRowDataGroups("INTUBADO")
pt2$defineCalculation(calculationName = "ID_REGISTRO",summariseExpression = "n()")
pt2$renderPivot()#muestra la pivot en el área de gráficos; lista para exportar

#Número de personas en la CLASIFICACIÓN_FINAL
pt3 <- PivotTable$new()
pt3$addData(datos.covid.2)
pt3$addRowDataGroups("CLASIFICACION_FINAL")
pt3$defineCalculation(calculationName = "ID_REGISTRO",summariseExpression = "n()")
pt3$renderPivot()#muestra la pivot en el área de gráficos
print(pt3)#o l imprimis en la consola

"Tambien se puede convertir una pivot table a un data frame y trabajar con el como dataframe, aqui el 
ejemplo:
"
class(pt3)
pt3.df <- pt3$asDataFrame()#ya es un dataframe
class(pt3.df)
View(pt3.df)

"HISTOGRAMAS o grafica de barra:
Así podemos ver las freceuncias de las observaciones por variable
"
#Personas por sexo
ggplot(data = datos.covid.2,aes(x=SEXO))+geom_bar(stat = "count")+
  geom_text(aes(label=..count..),stat="count",position=position_dodge(0.9),vjust=-0.2)+
  ggtitle("Mujeres vs Hombres")
#Tipos de pacientes
ggplot(data = datos.covid.2,aes(x=TIPO_PACIENTE))+geom_bar(stat = "count")+
  geom_text(aes(label=..count..),stat="count",position=position_dodge(0.9),vjust=-0.2)+
  ggtitle("Tipos de pacientes")

"BOXPLOTS:
Estos graficos nos ayudan a ver la dispersion con respecto a la media de las observaciones por variable
"
#La edad de los intubados
boxplot(EDAD ~ INTUBADO, data = datos.covid.2, col = "lightgray", varwidth = TRUE, 
        main = "Intubados por edad",
        ylab = "Edad",xlab = "Intubados")
table(datos.covid.2$INTUBADO)

"PIE CHART:
Grafico que muestra los porcenyajes de la distribucion de cada variable
"
x <- pt3.df[-8,]
CF <- unique(datos.covid.2$CLASIFICACION_FINAL)
class(CF)
pie.percent <- round(100*x/sum(x),1)
pie.percent <- paste0(pie.percent,"%")

pie(x, labels = pie.percent, main = "Clasificación pacientes",col = rainbow(length(x)))
legend("topright", legend=c(3,7,5,4,1,2,6), cex = 0.8,fill = rainbow(length(x)))


pie(x, labels = "", main = "Clasificación pacientes",col = rainbow(length(x)))#FAIL
text(locator(7), c(3,7,5,4,1,2,6))
legend("topright", legend=c(3,7,5,4,1,2,6), cex = 0.8,fill = rainbow(length(x)))


# Análisis exploratorio 2, en función de las muerte --------------------------
# #porcentaje de personas que fallecieron por sexo
# ggplot(data = datos.covid.2,aes(x=SEXO,fill=MUERTES))+geom_bar(aes(y=(..count..)/sum(..count..)))+
#   geom_text(aes(label=..count..),stat="count",position=position_dodge(0.9),vjust=-0.2)+
#   ggtitle("Porcentaje de personas fallecidas por sexo")

#numero de personas que fallecioeron agrupadas por sexo
# ggplot(data = datos.covid.2,aes(x=SEXO,fill=MUERTES))+geom_bar()+
#   geom_text(aes(label=..count..),stat="count",position=position_dodge(0.9),vjust=0)+
#   ggtitle("Número de personas fallecidas por sexo")+
#   labs(x = 'Sexo', y = 'Cantidad',fill = 'Persona muerta')

#A_1 numero de personas que fallecioeron agrupadas por sexo
A_1 <- ggplot(data = datos.covid.2,aes(x=SEXO,fill=MUERTES))+geom_bar()+
  geom_text(aes(label=..count.. , y=(..count..)+0.05),stat="count",position=position_dodge(0.9),vjust=0, hjust=0.5)+
  #ggtitle("Número de personas fallecidas por sexo")+
  labs(x = 'Sexo', y = 'Cantidad',fill = 'Personas muertas', title ="Número de personas muertas por sexo" ,caption = "Clases:     1 = Mujeres  y  2 = Hombres")
A_1
#B_1
B_1 <- ggplot(data = datos.covid.2, aes(x = factor(SEXO), 
                                 y = prop.table(stat(count)), 
                                 fill = factor(MUERTES), 
                                 label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'SEXO', y = '%',fill = 'Personas muertas', title ="Porcentaje de personas muertas por sexo" ,caption = "Clases:     1 = Mujeres  y  2 = Hombres")
B_1

grafica.uno <- multiplot(A_1,B_1,cols = 2)#en formato .png n osale bien, se guarda recorte

#porcenaje de muertos agrupados por intubamiento
table(datos.covid.2$INTUBADO)
# ggplot(data = datos.covid.2,aes(x=INTUBADO,fill=MUERTES))+geom_bar(aes(y=(..count..)/sum(..count..)))+
#   ggtitle("Porcentaje de personas con y sin intubamiento")#de las personas intubada, la mayoria murio
# 
# ggplot(data = datos.covid.2,aes(x=INTUBADO,fill=MUERTES))+geom_bar()+
#   geom_text(aes(label=..count..),stat="count",position=position_dodge(0.9),vjust=0)+
#   ggtitle("Número de personas fallecidas con y sin intubamiento")

#A_2 numero de personas que fallecioeron agrupadas ocurrido el intubamiento
names(datos.covid.2)
datos.covid.3 <- datos.covid.2[-indices.na.intubados,c("INTUBADO","MUERTES")]#quitamos los NA'S
dim(datos.covid.3)

A_2 <- ggplot(data = datos.covid.3,aes(x=INTUBADO,fill=MUERTES))+geom_bar()+
  geom_text(aes(label=..count.. , y=(..count..)+0.05),stat="count",position=position_dodge(0.9),vjust=0, hjust=0.5)+
  #ggtitle("Número de personas fallecidas por sexo")+
  labs(x = 'INTUBADOS', y = 'Cantidad',fill = 'Personas muertas', title ="Número de personas muertas ocurrido el intubamiento")
A_2

#B_2
B_2 <- ggplot(data = datos.covid.3, aes(x = factor(INTUBADO), 
                                        y = prop.table(stat(count)), 
                                        fill = factor(MUERTES), 
                                        label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'INTUBAMIENTO', y = '%',fill = 'Personas muertas', title ="Porcentaje de personas muertas intubadas" ,caption = "Nota: Estos son los casos donde se identificaron personas 1 = Intubadas ó 2 = No intubadas")
B_2

grafica.dos <- multiplot(A_2,B_2,cols = 2)#en .png sale chafa, se exporta recorte y reemplaza la origi

#A_3 numero de personas mjuertas dado que fueron hospitalizadas o ambulatorias
unique(datos.covid.2$TIPO_PACIENTE)
table(datos.covid.2$TIPO_PACIENTE)
A_3 <- ggplot(data = datos.covid.2,aes(x=TIPO_PACIENTE,fill=MUERTES))+geom_bar()+
  geom_text(aes(label=..count.. , y=(..count..)+0.05),stat="count",position=position_dodge(0.9),vjust=0, hjust=0.5)+
  labs(x = 'Tipo de paciente', y = 'Cantidad',fill = 'Personas muertas', title ="Número de personas muertas por tipo de paciente",caption = "Clases:     1 = Ambulatorio  y  2 = Hospitalizado")
A_3
#B_3
B_3 <- ggplot(data = datos.covid.2, aes(x = factor(TIPO_PACIENTE), 
                                        y = prop.table(stat(count)), 
                                        fill = factor(MUERTES), 
                                        label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Tipo de paciente', y = '%',fill = 'Personas muertas', title ="Porcentaje de personas muertas por tipo de paciente" ,caption = "Clases:     1 = Ambulatorio  y  2 = Hospitalizado")
B_3

grafica.tres <- multiplot(A_3,B_3,cols = 2)#en .png sale chafa, se exporta recorte y reemplaza la origi

#A_4
unique(datos.covid.2$EDAD)
edades <- datos.covid.2 %>% count(EDAD,sort = T)
View(edades)
skim(datos.covid.2$EDAD)
hist(datos.covid.2$EDAD,breaks = 90, col = "orange",xlab = "Edad",ylab = "Frecuencia",main = "Distibución de la edad (personas contagiadas)")
#hacemos los 5 rangos en datasets: 0-20, 21-40, 41-60, 61-80, <80
#vamos a recortar el dataframe
colnames(datos.covid.2)
datos.covid.4 <- datos.covid.2[,c("EDAD","MUERTES")]
rango.1.edades <- datos.covid.4[which(datos.covid.2$EDAD >= 0 & datos.covid.2$EDAD <= 20),]
rango.1.edades['Rango'] <- 1
#View(rango.1.edades)
rango.2.edades <- datos.covid.4[which(datos.covid.2$EDAD >= 21 & datos.covid.2$EDAD <= 40),]
rango.2.edades['Rango'] <- 2
#View(rango.2.edades)
rango.3.edades <- datos.covid.4[which(datos.covid.2$EDAD >= 41 & datos.covid.2$EDAD <= 60),]
rango.3.edades['Rango'] <- 3
rango.4.edades <- datos.covid.4[which(datos.covid.2$EDAD >= 61 & datos.covid.2$EDAD <= 80),]
rango.4.edades['Rango'] <- 4
rango.5.edades <- datos.covid.4[which(datos.covid.2$EDAD >= 81),]
rango.5.edades['Rango'] <- 5
rango.edades <- rbind(rango.1.edades,rango.2.edades,rango.3.edades,rango.4.edades,rango.5.edades)
dim(rango.5.edades)#231650, 1255835, 1004121, 350380 y 50463
#Ahora si construimos la el grafico:
colnames(rango.edades)
A_4 <- ggplot(data = rango.edades,aes(x=Rango,fill=MUERTES))+geom_bar()+
  geom_text(aes(label=..count.. , y=(..count..)+0.05),stat="count",position=position_dodge(0.9),vjust=0, hjust=0.5)+
  labs(x = 'Rango de edad', y = 'Cantidad',fill = 'Personas muertas', title ="Número de personas muertas por rango de edades")
A_4
#B_4
B_4 <- ggplot(data = rango.edades, aes(x = factor(Rango), 
                                        y = prop.table(stat(count)), 
                                        fill = factor(MUERTES), 
                                        label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Rango de edad', y = '%',fill = 'Personas muertas', title ="Porcentaje de personas muertas por rango de edades" ,caption = "Los rangos en años son '1' : 0-20 , '2' : 21-40 , '3' : 41-60 , '4': 61-80 , '5' : =<80")
B_4

grafica.cuatro <- multiplot(A_4,B_4,cols = 2)#en .png sale chafa, se exporta recorte y reemplaza la origi

#A_5, paises de mueetos en orden
unique(datos.covid.2$NACIONALIDAD)
unique(datos.covid.2$PAIS_NACIONALIDAD)
unique(datos.covid.2$PAIS_ORIGEN)

#nacionalidades.ordenadadas <- datos.covid.2 %>% count(PAIS_NACIONALIDAD,sort = T)#num personas por pais
#con la pivot
pt4 <- PivotTable$new()
pt4$addData(datos.covid.2)
pt4$addRowDataGroups("PAIS_NACIONALIDAD")
pt4$addColumnDataGroups("MUERTES")
pt4$defineCalculation(calculationName = "ID_REGISTRO",summariseExpression = "n()")
# pt4$defineCalculation(calculationName="ID_REGISTRO", caption="Total Personas",
#                       summariseExpression="n()")
# pt4$defineCalculation(calculationName="Porcentaje", caption="% muertos",
#                      type="calculation", basedOn=c("Porcentaje", "ID_REGISTRO"),
#                      format="%.1f %%",
#                      calculationExpression="values$Pocentaje/values$ID_REGISTRO*100")
pt4$renderPivot()#muestra la pivot en el área de gráficos; lista para exportar
pt4.dataframe <- pt4$asDataFrame()
class(pt4.dataframe)
View(pt4.dataframe)
colnames(pt4.dataframe)
muertos.paises <- pt4.dataframe %>% arrange(desc(SÍ))#
View(muertos.paises)


"Grafico 6, hacer columna de los enfermos y agrupar por muerte

Recortar datframes por enfermedad, dejar la muerte
Quitar NAS
Unir dataframes
Grafico por enfermedad y coloreo por murte

"
unique(datos.covid.2$NEUMONIA)#quitar NAS
#COMENZAMOS LA REMOCON DE NAS:
colnames(datos.covid.2)
datos.covid.5 <- datos.covid.2[,c("NEUMONIA","MUERTES")] 
colnames(datos.covid.5)
dim(datos.covid.5)
nuemonia <- datos.covid.2$NEUMONIA
indices.na.nuemonia <- which(nuemonia %in% c(99) )#índices ques seran NA porque tienen estas catego
nuemonia <- as.matrix(nuemonia)
View(nuemonia)
nuemonia[indices.na.nuemonia,1] <- NA#les ponemos NAS
View(indices.na.nuemonia)
nuemonia <- as.factor(nuemonia)
unique(nuemonia)#listo, niveles adecuados; ya podemos trabajar con la pivot table
datos.covid.2[,"NEUMONIA"] <- nuemonia#reemplazamos la varibale original con la nueva que creamos con NAS
#Lo que debi de hacer
datos.covid.5 <- datos.covid.5[datos.covid.5$NEUMONIA==1,]
dim(datos.covid.5)
unique(datos.covid.5$NEUMONIA)
#anadimos la columna de enfermedad
datos.covid.5[,"enfermedad"] <- "neumonia"
View(datos.covid.5)
datos.covid.5 <- datos.covid.5[,c("MUERTES","enfermedad")]

#otra enfermedad
unique(datos.covid.2$DIABETES)#quitar NAS
colnames(datos.covid.2)
datos.covid.6 <- datos.covid.2[,c("DIABETES","MUERTES")] 
colnames(datos.covid.6)
diabetes <- datos.covid.2$DIABETES
indices.na.diabetes <- which(diabetes %in% c(98) )#índices ques seran NA porque tienen estas catego
diabetes <- as.matrix(diabetes)
View(diabetes)
diabetes[indices.na.diabetes,1] <- NA#les ponemos NAS
View(indices.na.diabetes)
diabetes <- as.factor(diabetes)
unique(diabetes)#listo, niveles adecuados; ya podemos trabajar con la pivot table
datos.covid.2[,"DIABETES"] <- diabetes#reemplazamos la varibale original con la nueva que creamos con NAS
#CON ESTO REALMENTE QUITO los registros NAs para el grafico
unique(datos.covid.2$DIABETES)#quitar NAS
datos.covid.6 <- datos.covid.2[,c("DIABETES","MUERTES")] 
dim(datos.covid.6)
datos.covid.6 <- datos.covid.6[datos.covid.6$DIABETES ==1,]
dim(datos.covid.6)
#anadimos la columna de enfermedad
datos.covid.6 <- datos.covid.2[,c("DIABETES","MUERTES")] 
datos.covid.6 <- datos.covid.6[datos.covid.6$DIABETES ==1,]
dim(datos.covid.6)
datos.covid.6[,"enfermedad"] <- "diabetes"
View(datos.covid.6)
datos.covid.6 <- datos.covid.6[,c("MUERTES","enfermedad")]
View(datos.covid.6)


#Ahora otra enfermedad
unique(datos.covid.2$EPOC)#quitar NAS
colnames(datos.covid.2)
datos.covid.7 <- datos.covid.2[,c("EPOC","MUERTES")] 
colnames(datos.covid.7)
epoc <- datos.covid.2$EPOC
indices.na.epoc <- which(epoc %in% c(98) )#índices ques seran NA porque tienen estas catego
epoc <- as.matrix(epoc)
#View(epoc)
epoc[indices.na.epoc,1] <- NA#les ponemos NAS
#View(indices.na.epoc)
epoc <- as.factor(epoc)
unique(epoc)#listo, niveles adecuados; ya podemos trabajar con la pivot table
datos.covid.2[,"EPOC"] <- epoc#reemplazamos la varibale original con la nueva que creamos con NAS

#CON ESTO REALMENTE QUITO los registros NAs para el grafico
unique(datos.covid.2$EPOC)#quitar NAS
datos.covid.7 <- datos.covid.2[,c("EPOC","MUERTES")] 
dim(datos.covid.7)
datos.covid.7 <- datos.covid.7[!(datos.covid.7$EPOC ==98),]
dim(datos.covid.7)#listo, podemos unir

#anadimos la columna de enfermedad
datos.covid.7 <- datos.covid.2[,c("EPOC","MUERTES")]
unique(datos.covid.7$EPOC)
datos.covid.7 <- datos.covid.7[datos.covid.7$EPOC ==1,]
dim(datos.covid.7)
datos.covid.7[,"enfermedad"] <- "epoc"
#View(datos.covid.7)
datos.covid.7 <- datos.covid.7[,c("MUERTES","enfermedad")]



#otra enfermedad:
unique(datos.covid.2$ASMA)#quitar NAS
#colnames(datos.covid.2)
datos.covid.8 <- datos.covid.2[,c("ASMA","MUERTES")] 
colnames(datos.covid.8)
asma <- datos.covid.2$ASMA
indices.na.asma <- which(asma %in% c(98) )#índices ques seran NA porque tienen estas catego
asma <- as.matrix(asma)
#View(epoc)
asma[indices.na.asma,1] <- NA#les ponemos NAS
#View(indices.na.epoc)
asma <- as.factor(asma)
unique(asma)#listo, niveles adecuados; ya podemos trabajar con la pivot table
datos.covid.2[,"ASMA"] <- asma#reemplazamos la varibale original con la nueva que creamos con NAS

#CON ESTO REALMENTE QUITO los registros NAs para el grafico
unique(datos.covid.2$ASMA)#quitar NAS
datos.covid.8 <- datos.covid.2[,c("ASMA","MUERTES")] 
dim(datos.covid.8)
datos.covid.8 <- datos.covid.8[!(datos.covid.8$ASMA ==98),]
dim(datos.covid.8)#listo, podemos unir

#anadimos la columna de enfermedad
datos.covid.8 <- datos.covid.2[,c("ASMA","MUERTES")]
unique(datos.covid.8$ASMA)
datos.covid.8 <- datos.covid.8[datos.covid.8$ASMA ==1,]
dim(datos.covid.8)
datos.covid.8[,"enfermedad"] <- "asma"
#View(datos.covid.7)
datos.covid.8 <- datos.covid.8[,c("MUERTES","enfermedad")]


#otra enfermedada
unique(datos.covid.2$HIPERTENSION)#quitar NAS
#colnames(datos.covid.2)
datos.covid.9 <- datos.covid.2[,c("HIPERTENSION","MUERTES")] 
colnames(datos.covid.9)
hipertension <- datos.covid.2$HIPERTENSION
indices.na.hipertension <- which(hipertension %in% c(98) )#índices ques seran NA porque tienen estas catego
hipertension <- as.matrix(hipertension)
#View(epoc)
hipertension[indices.na.hipertension,1] <- NA#les ponemos NAS
#View(indices.na.epoc)
hipertension <- as.factor(hipertension)
unique(hipertension)#listo, niveles adecuados; ya podemos trabajar con la pivot table
datos.covid.2[,"HIPERTENSION"] <- hipertension#reemplazamos la varibale original con la nueva que creamos con NAS

#CON ESTO REALMENTE QUITO los registros NAs para el grafico
unique(datos.covid.2$HIPERTENSION)#quitar NAS
datos.covid.9 <- datos.covid.2[,c("HIPERTENSION","MUERTES")] 
dim(datos.covid.9)
datos.covid.9 <- datos.covid.9[!(datos.covid.9$HIPERTENSION ==98),]
dim(datos.covid.9)#listo, podemos unir

#anadimos la columna de enfermedad
datos.covid.9 <- datos.covid.2[,c("HIPERTENSION","MUERTES")]
unique(datos.covid.9$HIPERTENSION)
datos.covid.9 <- datos.covid.9[datos.covid.9$HIPERTENSION ==1,]
dim(datos.covid.9)
datos.covid.9[,"enfermedad"] <- "hipertension"
#View(datos.covid.7)
datos.covid.9 <- datos.covid.9[,c("MUERTES","enfermedad")]


#otra enfermedad
unique(datos.covid.2$CARDIOVASCULAR)#quitar NAS
#colnames(datos.covid.2)
datos.covid.10 <- datos.covid.2[,c("CARDIOVASCULAR","MUERTES")] 
colnames(datos.covid.10)
cardiovascular <- datos.covid.2$CARDIOVASCULAR
indices.na.cardiovascular <- which(cardiovascular %in% c(98) )#índices ques seran NA porque tienen estas catego
cardiovascular <- as.matrix(cardiovascular)
#View(epoc)
cardiovascular[indices.na.cardiovascular,1] <- NA#les ponemos NAS
#View(indices.na.epoc)
cardiovascular <- as.factor(cardiovascular)
unique(cardiovascular)#listo, niveles adecuados; ya podemos trabajar con la pivot table
datos.covid.2[,"CARDIOVASCULAR"] <- cardiovascular#reemplazamos la varibale original con la nueva que creamos con NAS

#CON ESTO REALMENTE QUITO los registros NAs para el grafico
unique(datos.covid.2$CARDIOVASCULAR)#quitar NAS
datos.covid.10 <- datos.covid.2[,c("CARDIOVASCULAR","MUERTES")] 
dim(datos.covid.10)
datos.covid.10 <- datos.covid.10[!(datos.covid.10$CARDIOVASCULAR ==98),]
dim(datos.covid.10)#listo, podemos unir

#anadimos la columna de enfermedad
datos.covid.10 <- datos.covid.2[,c("CARDIOVASCULAR","MUERTES")]
unique(datos.covid.10$CARDIOVASCULAR)
datos.covid.10 <- datos.covid.10[datos.covid.10$CARDIOVASCULAR ==1,]
dim(datos.covid.10)
datos.covid.10[,"enfermedad"] <- "cardiovascular"
#View(datos.covid.7)
datos.covid.10 <- datos.covid.10[,c("MUERTES","enfermedad")]


#otra enfermedad
unique(datos.covid.2$OBESIDAD)#quitar NAS
#colnames(datos.covid.2)
datos.covid.11 <- datos.covid.2[,c("OBESIDAD","MUERTES")] 
colnames(datos.covid.11)
obesidad <- datos.covid.2$OBESIDAD
indices.na.obesidad <- which(obesidad %in% c(98) )#índices ques seran NA porque tienen estas catego
obesidad <- as.matrix(obesidad)
#View(epoc)
obesidad[indices.na.obesidad,1] <- NA#les ponemos NAS
#View(indices.na.epoc)
obesidad <- as.factor(obesidad)
unique(obesidad)#listo, niveles adecuados; ya podemos trabajar con la pivot table
datos.covid.2[,"OBESIDAD"] <- obesidad#reemplazamos la varibale original con la nueva que creamos con NAS

#CON ESTO REALMENTE QUITO los registros NAs para el grafico
unique(datos.covid.2$OBESIDAD)#quitar NAS
datos.covid.11 <- datos.covid.2[,c("OBESIDAD","MUERTES")] 
dim(datos.covid.11)
datos.covid.11 <- datos.covid.11[!(datos.covid.11$OBESIDAD ==98),]
dim(datos.covid.11)#listo, podemos unir

#anadimos la columna de enfermedad
datos.covid.11 <- datos.covid.2[,c("OBESIDAD","MUERTES")]
unique(datos.covid.11$OBESIDAD)
datos.covid.11 <- datos.covid.11[datos.covid.11$OBESIDAD ==1,]
dim(datos.covid.11)
datos.covid.11[,"enfermedad"] <- "obesidad"
#View(datos.covid.7)
datos.covid.11 <- datos.covid.11[,c("MUERTES","enfermedad")]


#otra enfermedad
unique(datos.covid.2$RENAL_CRONICA)#quitar NAS
#colnames(datos.covid.2)
datos.covid.12 <- datos.covid.2[,c("RENAL_CRONICA","MUERTES")] 
colnames(datos.covid.12)
renal_cronica <- datos.covid.2$RENAL_CRONICA
indices.na.renal_cronica <- which(renal_cronica %in% c(98) )#índices ques seran NA porque tienen estas catego
renal_cronica <- as.matrix(renal_cronica)
#View(epoc)
renal_cronica[indices.na.renal_cronica,1] <- NA#les ponemos NAS
#View(indices.na.epoc)
renal_cronica <- as.factor(renal_cronica)
unique(renal_cronica)#listo, niveles adecuados; ya podemos trabajar con la pivot table
datos.covid.2[,"RENAL_CRONICA"] <- renal_cronica#reemplazamos la varibale original con la nueva que creamos con NAS

#CON ESTO REALMENTE QUITO los registros NAs para el grafico
unique(datos.covid.2$RENAL_CRONICA)#quitar NAS
datos.covid.12 <- datos.covid.2[,c("RENAL_CRONICA","MUERTES")] 
dim(datos.covid.12)
datos.covid.12 <- datos.covid.12[!(datos.covid.12$RENAL_CRONICA ==98),]
dim(datos.covid.12)#listo, podemos unir

#anadimos la columna de enfermedad
datos.covid.12 <- datos.covid.2[,c("RENAL_CRONICA","MUERTES")]
unique(datos.covid.12$RENAL_CRONICA)
datos.covid.12 <- datos.covid.12[datos.covid.12$RENAL_CRONICA ==1,]
dim(datos.covid.12)
datos.covid.12[,"enfermedad"] <- "renal_cronica"
#View(datos.covid.7)
datos.covid.12 <- datos.covid.12[,c("MUERTES","enfermedad")]


#otra enfermedad
unique(datos.covid.2$TABAQUISMO)#quitar NAS
#colnames(datos.covid.2)
datos.covid.13 <- datos.covid.2[,c("TABAQUISMO","MUERTES")] 
colnames(datos.covid.13)
tabaquismo <- datos.covid.2$TABAQUISMO
indices.na.tabaquismo <- which(tabaquismo %in% c(98) )#índices ques seran NA porque tienen estas catego
tabaquismo <- as.matrix(tabaquismo)
#View(epoc)
tabaquismo[indices.na.tabaquismo,1] <- NA#les ponemos NAS
#View(indices.na.epoc)
View(tabaquismo)
tabaquismo <- as.factor(tabaquismo)
unique(tabaquismo)#listo, niveles adecuados; ya podemos trabajar con la pivot table
datos.covid.2[,"TABAQUISMO"] <- tabaquismo#reemplazamos la varibale original con la nueva que creamos con NAS

#CON ESTO REALMENTE QUITO los registros NAs para el grafico
unique(datos.covid.2$TABAQUISMO)#quitar NAS
datos.covid.13 <- datos.covid.2[,c("TABAQUISMO","MUERTES")] 
dim(datos.covid.13)
datos.covid.13 <- datos.covid.13[!(datos.covid.13$TABAQUISMO ==98),]
dim(datos.covid.13)#listo, podemos unir

#anadimos la columna de enfermedad
datos.covid.13 <- datos.covid.2[,c("TABAQUISMO","MUERTES")]
unique(datos.covid.13$TABAQUISMO)
datos.covid.13 <- datos.covid.13[datos.covid.13$TABAQUISMO ==1,]
dim(datos.covid.13)
datos.covid.13[,"enfermedad"] <- "tabaquismo"
#View(datos.covid.7)
datos.covid.13 <- datos.covid.13[,c("MUERTES","enfermedad")]


#Ahpra unimos las enfermedades
datos.covid.enfermedades <- rbind(datos.covid.5,datos.covid.6,datos.covid.7,datos.covid.8,datos.covid.9,datos.covid.10,datos.covid.11,datos.covid.12,datos.covid.13)
colnames(datos.covid.enfermedades)
#Por ultimo graficamos
A_5 <- ggplot(data = datos.covid.enfermedades,aes(x=enfermedad,fill=MUERTES))+geom_bar()+
  geom_text(aes(label=..count.. , y=(..count..)+0.05),stat="count",position=position_dodge(0.9),vjust=0, hjust=0.5)+
  labs(x = 'Enfermedades', y = 'Cantidad',fill = 'Personas muertas', title ="Número de personas muertas de acuerdo a enfermedades preexistentes")
A_5
#B_5
B_5 <- ggplot(data = datos.covid.enfermedades, aes(x = factor(enfermedad), 
                                       y = prop.table(stat(count)), 
                                       fill = factor(MUERTES), 
                                       label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Enfermedades', y = '%',fill = 'Personas muertas', title ="Porcentaje de personas muertas de acuerdo a enfermedades preexistentes")
B_5

multiplot(A_5,B_5,cols = 2)#no las exporte juntas, por separadas las exporte de recortes; 6_1 y 6_2

# multiplot, funcion para juntar graficas ---------------------------------------------------------------

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

