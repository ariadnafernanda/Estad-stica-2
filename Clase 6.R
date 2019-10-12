#cargando data#

install.packages("rio")
library(rio)
EcoFree="https://github.com/jcgcjuan/Magallanes-Clases-/raw/master/Data%20EconoFreedom.xlsx"
EcoFree2019=import(EcoFree)


#limpieza de data#
str(EcoFree2019)
names(EcoFree2019)
EcoFree2019$"Region"=NULL
EcoFree2019$"Region Rank"=NULL
EcoFree2019$"2019 Score"=NULL

names(EcoFree2019)
names(EcoFree2019)[names(EcoFree2019)=='CountryID']='ID'
names(EcoFree2019)[names(EcoFree2019)=='Country Name']='Country' 
names(EcoFree2019)

EcoFree2019=na.omit(EcoFree2019)
EcoFree2019[,-1:-2]=lapply(EcoFree2019[,-1:-2], as.numeric)
names(EcoFree2019) 

dataprueba = EcoFree2019
data = EcoFree2019
head(data)
row.names(data)=data$Country
data$"Country"=NULL
data=na.omit(data)
dataprueba=na.omit(dataprueba)

head(data)

#subsetear primero#
datasubset=data[,c(3:14)]

#OBSERVACIONES PREVIAS AL ANALISIS FACTORIAL#
#Calculemos matriz de correlación
install.packages("polycor")
library(polycor)
corMatrix=polycor::hetcor(datasubset)$correlations

#explorar contenidos
install.packages("ggcorrplot")
library(ggplot2)
library(ggcorrplot)
ggcorrplot(corMatrix)


#Evaluando significancia
#te va a mostrar el mismo gráfico de correlación pero tomando también la significancia de correlaciones
ggcorrplot(corMatrix,
           p.mat = cor_pmat(corMatrix),
           insig = "blank")

#Verificar si puedo factorizar
install.packages("psych")
library(psych)
psych::KMO(corMatrix) 
#debe ser mayor a 0.5 y mejor si es mayor a 0.7. 
#Esto indica que si sería posible hacer correlaciones válidas de variables.


#Hnula: La matriz de correlacion es una matriz identidad
#esto debería ser falso porque la idea es que no solo se correlacionen con sí mismas.
cortest.bartlett(corMatrix,n=nrow(datasubset))$p.value>0.05

#Hnula: La matriz de correlacion es una matriz singular.
#Cuando una variable solo se correlaciona con otra. Esto debe ser falso, queremos que se correlacionen entre varias.
install.packages("matrixcalc")
library(matrixcalc)
is.singular.matrix(corMatrix)

#Determinando con cuántos factores podemos redimensionar

fa.parallel(datasubset,fm = 'ML', fa = 'fa')

#Por encima del simulado se sugiere 3 factores

#Redimencionando
install.packages("GPArotation")
library(GPArotation)
resfa = fa(datasubset,nfactors = 3,cor = 'mixed',rotate = "varimax",fm="minres")
print(resfa$loadings)

#Resultado mejorado

print(resfa$loadings,cutoff = 0.51)

#Diagrama visual

fa.diagram(resfa)

#EVALUACIÓN
#La Raíz del error cuadrático medio corregida está cerca a cero?

resfa$crms

#La Raíz del error cuadrático medio de aproximación es menor a 0.05?

resfa$RMSEA

#El índice de Tucker-Lewis es mayor a 0.9:

resfa$TLI

#Que variables aportaron mas a los factores?

sort(resfa$communality)

#Que variables contribuyen a mas de un factor?

sort(resfa$complexity)

#¿Qué nombres les darías?

as.data.frame(resfa$scores)

#Diagrama
#Entonces para plotear aquí si utilizo el DATA PRUEBA porque tiene COUNTRY. Y le añado scores de los factores.
datascores=cbind(dataprueba,as.data.frame(resfa$scores))

install.packages("plotly")
library(plotly)


plot_ly(data=datascores, x = ~MR1, y = ~MR2, z = ~MR3, text=~Country) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Gobierno'),
                      yaxis = list(title = 'Impuestos'),
                      zaxis = list(title = 'Inversión')))

#OJO EXTRA
#Recordando

install.packages("fpc")
library(fpc)
install.packages("cluster")
library(cluster)
install.packages("dbscan")
library(dbscan)


g.dist.cmd = daisy(datascores[,c(3:14)], metric = 'euclidean')
kNNdistplot(g.dist.cmd, k=3)

#Para tener una idea de cada quien

resDB=fpc::dbscan(g.dist.cmd, eps=0.6, MinPts=3,method = 'dist')
datascores$clustDB=as.factor(resDB$cluster)
aggregate(cbind(MR1, MR2,MR3) # dependientes
          ~ clustDB, # nivel
          data = datascores,    # data
          max)            # operacion

library(dplyr)
plot_ly(data=datascores, x = ~MR1, y = ~MR2, z = ~MR3, text=~Country, 
        color = ~clustDB) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Gobierno'),
                      yaxis = list(title = 'Impuestos'),
                      zaxis = list(title = 'Inversión')))
