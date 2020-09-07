##################################################
###clase análisis de clusters/conglomerados
###31/08/2020
###programa doctorado en psicología, u de talca
###docente: herman elgueta, herman.elgueta@umag.cl
###ejemplo 1: planetas
##################################################

##################################################
###paso 0, configurar r
##################################################
#instalar librerías necesarias
#install.packages(c("factoextra","ggplot2"))

#library(factoextra)
#library(ggplot2)

##################################################
###paso 1, ingresar y configurar los datos
##################################################
#cargar la matriz completa
df <- read.csv("2020-utalca-acluster/planets.csv")
#escoger sólo aquellas variables de interés
cat(colnames(df), sep = "\n")
df <- read.csv("2020-utalca-acluster/planets.csv")[,c(2:5,9,19)]
df
#darle nombre a las filas
rownames(df) <- read.csv("2020-utalca-acluster/planets.csv")[,1]
#si es necesario, estandarizar las variables
df <- scale(df)
df <- as.data.frame(df)
#revisar la matriz
df
#corregir cualquier error restante
df <- df[1:8,]
#re-revisar la matriz ad infinitum...
df

##################################################
###paso 2, establecer el número de clústers
##################################################
#calcular y considerar la matriz de distancias
factoextra::get_dist(df, method = "euclidean")
m.distancia <- factoextra::get_dist(df, method = "euclidean")
factoextra::fviz_dist(m.distancia, 
					gradient = list(low = "blue", mid = "white", high = "red"))

#examinar diferentes sugerencias sobre cuántos clústers extraer
factoextra::fviz_nbclust(df, kmeans, method = "wss", k.max=7) #codo
factoextra::fviz_nbclust(df, kmeans, method = "silhouette", k.max=7) #silueta

##################################################
###paso 3, extraer/examinar los clusters
##################################################
#ejecutar clustering según la cantidad de clústers escogidos
clus <- 3
kmeans(df, centers = clus, nstart = 25)
k <- kmeans(df, centers = clus, nstart = 25)

#graficar los clusters
factoextra::fviz_cluster(k, data = df)

#dendograma
tree <- factoextra::hcut(df, k = clus, stand = TRUE)
factoextra::fviz_dend(tree) 

##################################################
###paso 4, extraer, comprender y utilizar clusters
##################################################
df$cluster<-as.factor(k$cluster)
df

data_long <- tidyr::gather(df, caracteristica, valor, mass:number_of_moons, factor_key=TRUE)
data_long

ggplot2::ggplot(data_long, ggplot2::aes(as.factor(x = caracteristica), y = valor,group=cluster, colour = cluster)) + 
	ggplot2::stat_summary(fun = mean, geom="pointrange", size = 1) +
	ggplot2::stat_summary(geom="line")

ggplot2::ggplot(data_long, ggplot2::aes(as.factor(x = caracteristica), y = valor,group=cluster, colour = cluster)) + 
	ggplot2::stat_summary(fun = mean, geom="pointrange", size = 1, ggplot2::aes(shape = cluster))+
	ggplot2::stat_summary(geom="line")
