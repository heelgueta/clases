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

df <- read.csv("estresimp.csv")
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
###paso 2, bic clustering?
##################################################
library(mclust)
BIC <- mclust::mclustBIC(df)
plot(BIC)
summary(BIC)

ICL <- mclust::mclustICL(df)
plot(ICL)
summary(ICL)

mod <- mclust::Mclust(df, modelNames = "VII", G = 4, x = mclust::mclustBIC(df))
summary(mod)

#probabilities
z <- as.data.frame(mod$z)
view(z)


#mclust::mclustBootstrapLRT(df, modelName = "VVE")



#VISUALIZE
library(tidyverse)

means <- data.frame(mod$parameters$mean) %>%
	rownames_to_column() %>%
	rename(Characteristic = rowname) %>%
	pivot_longer(cols = c(X1, X2, X3, X4), names_to = "Profile", values_to = "Mean") %>%
	mutate(Mean = round(Mean, 2),
				 Mean = ifelse(Mean > 1, 1, Mean))


means <- data.frame(mod$parameters$mean) %>%
	rownames_to_column() %>%
	rename(Characteristic = rowname) %>%
	pivot_longer(cols = c(X1, X2, X3, X4), names_to = "Profile", values_to = "Mean") %>%
	mutate(Mean = round(Mean, 2))

means
means %>%
	ggplot(aes(Characteristic, Mean, group = Profile, color = Profile)) +
	geom_point(size = 2.25) +
	geom_line(size = 1.25) +
	scale_x_discrete(limits = c("ep","ae","am","ap","ac")) +
	labs(x = NULL, y = "Standardized mean") +
	theme_bw(base_size = 14) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

#other viz
plot(mod, what = "classification")
plot(mod, what="density")
moddr <- mclust::MclustDR(mod)
plot(moddr, what = "scatterplot")
plot(moddr, what = "boundaries", ngrid = 200)


###extract?
df$clas <- mod$classification
df
