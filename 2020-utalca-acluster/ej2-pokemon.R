##################################################
###clase análisis de clusters/conglomerados
###31/08/2020
###programa doctorado en psicología, u de talca
###docente: herman elgueta, herman.elgueta@umag.cl
###ejemplo 2: pokemon!
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

df <- read.csv("pokemon.csv")
df <- dplyr::filter(df, !grepl('Mega', Name)) #filter out "mega" pokemon
df <- df[which(df$Generation==1),] #just keep gen1
names <- df[,2]
cat(paste(colnames(df), collapse='\n' ) ) #list of vars
df <- df[,c(6:11)]
rownames(df) <- names

#si es necesario, estandarizar las variables
df <- scale(df)
df <- as.data.frame(df)
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
clus <- 3
	 
#?kmeans
kmeans(df, centers = clus, nstart = 25)
k <- kmeans(df, centers = clus, nstart = 25)

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

data_long <- tidyr::gather(df, caracteristica, valor, HP:Speed, factor_key=TRUE)
data_long
+

	# :)
ggplot2::ggplot(data_long, ggplot2::aes(as.factor(x = caracteristica), y = valor,group=cluster, colour = cluster)) + 
	ggplot2::stat_summary(fun = mean, geom="pointrange", size = 1)+
	ggplot2::stat_summary(geom="line")  #+ ggplot2::geom_point(ggplot2::aes(shape=cluster))
