##################################################
###clase análisis de clusters/conglomerados
###31/08/2020
###programa doctorado en psicología, u de talca
###docente: herman elgueta, herman.elgueta@umag.cl
###ejemplo 3: estrés/apoyo
##################################################

##################################################
###paso 0, configurar r
##################################################
#instalar librerías necesarias
#install.packages(c("factoextra","ggplot2","lavaan","NbClust","tidyr"))
#library(factoextra)
#library(ggplot2)
#library(NbClust)
#library(lavaan)
#library(tidyr)

##################################################
###paso 1, ingresar y configurar los datos
##################################################

df <- read.csv("2020-utalca-acluster/estresimp.csv")
cat(paste(colnames(df), collapse='\n' ) ) #list of vars


##################################################
###paso adocional: combinar cfa y extraer puntajes factoriales
##################################################

cfamod <- '
ep =~ ep07 + ep08 + ep09 + ep10
ae =~ ae07 + ae08 + ae09 + ae10
am =~ am1 + am2 + am3 + am4 + am5
ap =~ ap1 + ap2 + ap3 + ap4 + ap5
ac =~ ac1 + ac2 + ac3 + ac4 + ac5
'

fitcfamod <- lavaan::cfa(cfamod, data=df,estimator="MLR")
df <- as.data.frame(lavaan::predict(fitcfamod))
head(df)

#si es necesario, estandarizar las variables
df <- scale(df)
df <- as.data.frame(df)
cat(paste(colnames(df), collapse='\n' ) ) #list of vars

#remove outliers? discutible
df <- df[which(df$ep>-3 & df$ep < 3), ]
df <- df[which(df$ae>-3 & df$ae < 3), ]
df <- df[which(df$am>-3 & df$am < 3), ]
df <- df[which(df$ap>-3 & df$ap < 3), ]
df <- df[which(df$ac>-3 & df$ac < 3), ]

#revisar la matriz
df

##################################################
###paso 2, establecer el número de clústers
##################################################
#calcular y considerar la matriz de distancias
factoextra::get_dist(df)
m.distancia <- factoextra::get_dist(df, method = "euclidean")
factoextra::fviz_dist(m.distancia, 
											gradient = list(low = "blue", mid = "white", high = "red"))

#examinar diferentes sugerencias sobre cuántos clústers extraer
factoextra::fviz_nbclust(df, kmeans, method = "wss") #codo
factoextra::fviz_nbclust(df, kmeans, method = "silhouette") #silueta
resnumclust <- NbClust::NbClust(df, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")
factoextra::fviz_nbclust(resnumclust)

##################################################
###paso 3, extraer/examinar los clusters
##################################################
#ejecutar clustering según la cantidad de clústers escogidos
clus <- 4

#?kmeans
kmeans(df, centers = clus, nstart = 25)
k <- kmeans(df, centers = clus, nstart = 25)
kmeans(df, centers = clus, nstart = 25)$size

#graficar los clusters
factoextra::fviz_cluster(k, data = df)

#dendograma
tree <- factoextra::hcut(df, k = 3, stand = TRUE)
factoextra::fviz_dend(tree) 

##################################################
###paso 4, extraer, comprender y utilizar clusters
##################################################
df$cluster<-as.factor(k$cluster)
colnames(df)

head(df)

data_long <- tidyr::gather(df, caracteristica, valor, ep:ac, factor_key=TRUE)
data_long

ggplot2::ggplot(data_long, ggplot2::aes(as.factor(x = caracteristica), y = valor,group=cluster, colour = cluster)) + 
	ggplot2::stat_summary(fun = mean, geom="pointrange", size = 1)+
	ggplot2::stat_summary(geom="line")  #+ ggplot2::geom_point(ggplot2::aes(shape=cluster))
