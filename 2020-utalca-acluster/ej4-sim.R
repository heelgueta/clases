#simulate data
set.seed(1000) 
n <- 100

intel <- rnorm(n, 0, 1) # n: number observations, 0: mean, 1: sd
psico <- rnorm(n, 0, 1)

df <- data.frame(intel,psico)

#make groups
df[1:50,2] <- df[1:50,2]-3 #los no psicopatas
df[51:100,2] <- df[51:100,2]+3 #los psicopatas

df$succe <- rnorm(n, 0, 1)+.6*intel+.3*psico

hist(df$psico)

cor(df)






factoextra::get_dist(df, method = "euclidean")
m.distancia <- factoextra::get_dist(df, method = "euclidean")
factoextra::fviz_dist(m.distancia, 
											gradient = list(low = "blue", mid = "white", high = "red"))

#examinar diferentes sugerencias sobre cuántos clústers extraer
factoextra::fviz_nbclust(df, kmeans, method = "wss", k.max=7) #codo
factoextra::fviz_nbclust(df, kmeans, method = "silhouette", k.max=7) #silueta
resnumclust <- NbClust::NbClust(df, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")
factoextra::fviz_nbclust(resnumclust)

##################################################
###paso 3, extraer/examinar los clusters
##################################################
#ejecutar clustering según la cantidad de clústers escogidos
clus <- 2
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

data_long <- tidyr::gather(df, caracteristica, valor, intel:succe, factor_key=TRUE)
data_long

ggplot2::ggplot(data_long, ggplot2::aes(as.factor(x = caracteristica), y = valor,group=cluster, colour = cluster)) + 
	ggplot2::stat_summary(fun = mean, geom="pointrange", size = 1) +
	ggplot2::stat_summary(geom="line")
