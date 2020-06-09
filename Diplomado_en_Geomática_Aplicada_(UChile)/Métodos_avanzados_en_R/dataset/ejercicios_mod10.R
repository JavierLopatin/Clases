### dataset 1
#library(hsdar)

setwd('/home/javier/Documents/Clases/Diplomado GEP/dataset')

data1 = read.csv('angers-leaf-optical-properties-database--2003.csv')

data1 = data[data$Measurement_type == 'reflectance', ]

colnames(data1)

traits = data1[c(2,4,7,9,15)]
colnames(traits) = c('Car', 'Cab', 'Cw', 'Cm', 'N')
min(traits$Cm); max(traits$Cm)


reflec = data1[, 22:ncol(data1)]


plot(seq(400,2450), colMeans(reflec), type='l', las=1, lwd=2, ylab='Reflectance', xlab='Wavelength [nm]', 
     main="Reflectancia media")


# ejercicio

# primero verificar que los datos no tengan NaNs
any(is.na(reflec))


# vamos a hacer un PCA para reducir la dimensionalidad de la reflectancia, ya que por el momento 2051 bandas 
# (o dimensiones) es mucho para analizar al mismo tiempo

# como recomendación, siempre utilicen scale=TRUE para normalizar las variables para que tengan media cero y desviación estándar 1
# PCA se ve afectado grandemente por diferencia en unidades entrevariables. Es este caso particular no es tan terrible,
# ya que todas las variables tienen la misma unidad de medida, pero es buena práctiva nrmalizarlas de todas formas. 
pca <- prcomp(reflec, scale=TRUE)
names(pca)

prop_varianza <- pca$sdev^2 / sum(pca$sdev^2)
prop_varianza[1:10]

prop_varianza_acum <- cumsum(prop_varianza)
plot(seq(1,20), prop_varianza_acum[1:20], las=1, type='l', xlab='Componentes', ylab="Prop. varianza acumulada [%]")
abline(v=3, lty=2)


library(vegan)
# obtener gradientes de los rasgos de ojas dentro de los componentes de PCA
fit <- envfit(pca, traits) 
fit

# plot pca con colores según las especies
plot(pca$x[,1:2], col=data$Latin.Name)
# plot las variables 
plot(fit, col = "red", lty = 2)







##############################3
## dataset 2



# cargar datos de biomasa, altura de vegetación, y carbono total a nivel de plot
data2 = read.csv('Peatland.csv')

# cargar datos de cobertura de especies 
cov_data = read.csv('Cover_spp_peatland.csv')

# cargar datos de teledetección 
RS = read.csv('Peatland_RSdata.csv')

plot(seq(450,950,12.5), colMeans(RS)[2:ncol(RS)], type='l', las=1, lwd=2, ylab='Reflectance', 
     xlab='Wavelength [nm]', main="Reflectancia media")

library(vegan)

# datos de eppresencia/cobertura de especies
cov_data[1:8, 1:5]

# Seleccionar el número de k de acuerdo al stress
nmds.stress <- sapply(1:6, function(x) metaMDS(cov_data[,2:ncol(cov_data)], distance='jaccard', k=x)$stress)
plot(1:6, nmds.stress, xlab="Número de componentes", ylab='Stress')

# NMDS
nmds <- metaMDS(cov_data[,2:ncol(cov_data)], distance='jaccard', k=3, trymax=1000)

# K-means
library(factoextra)
library(NbClust)
# seleccionar número óptimo de clusters
numero_clusters <- NbClust(data = scores(nmds), distance = "euclidean", min.nc = 2,
                           max.nc = 10, method = "kmeans", index = "alllong")
fviz_nbclust(numero_clusters)

# datos observados (Altura vegetación, biomasa, riqueza, y carbono)
obs_data = data2[,c(4:7)]
colnames(obs_data) <- c('H', 'BM', 'C', 'Riq.') # solo para abreviar 
# obtener gradientes de altura de vegetacion
fit <- envfit(nmds, obs_data) 
# K-means con 4 clusters
cluster <- kmeans(x=scores(nmds), centers=4)
# plot componentes y colores por clusters
plot(scores(nmds)[,1:2], col=cluster$cluster, pch=16)
# plot environmental fits
plot(fit, col = "red", lty = 2)
