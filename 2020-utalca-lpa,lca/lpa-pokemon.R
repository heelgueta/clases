##################################################
###clase lpa
###07/09/2020
###programa doctorado en psicología, u de talca
###docente: herman elgueta, herman.elgueta@umag.cl
###ejemplo: pokemon!
##################################################
###fuentes:
###https://willhipson.netlify.app/post/latent-profile/latent-profile/
###https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5096736/
###https://cran.r-project.org/web/packages/mclust/vignettes/mclust.html
###https://cran.r-project.org/web/packages/tidyLPA/vignettes/Introduction_to_tidyLPA.html
##################################################
###paso 0, configurar r
##################################################
#instalar librerías necesarias
#install.packages("mclust";"dplyr")
##################################################
###paso 1, ingresar y configurar los datos
##################################################

df <- read.csv("2020-utalca-acluster/pokemon.csv")
df <- dplyr::filter(df, !grepl('Mega', Name)) #filter out "mega" pokemon
df <- df[which(df$Generation==1),] #just keep gen1
names <- df[,2]
cat(paste(colnames(df), collapse='\n' ) ) #list of vars
df <- df[,c(6:8,11)]
rownames(df) <- names

#si es necesario, estandarizar las variables
df <- scale(df)
df <- as.data.frame(df)
#quitar univariate outliers
df <- df[which(df$HP>-3 & df$HP < 3), ]
df <- df[which(df$Attack>-3 & df$Attack < 3), ]
df <- df[which(df$Defense>-3 & df$Defense < 3), ]
#df <- df[which(df$Sp..Atk>-3 & df$Sp..Atk < 3), ]
#df <- df[which(df$Sp..Def>-3 & df$Sp..Def < 3), ]
df <- df[which(df$Speed>-3 & df$Speed < 3), ]

df

##################################################
###paso 2, bic clustering?
##################################################
library(mclust)
BIC <- mclust::mclustBIC(df)
plot(BIC)
summary(BIC)

#ICL <- mclust::mclustICL(df)
#plot(ICL)
#summary(ICL)

mod <- mclust::Mclust(df, modelNames = "VVE", G = 2, x = mclust::mclustBIC(df))
summary(mod)

#probabilities
z <- as.data.frame(mod$z)
view(z)

mod$uncertainty

#mclust::mclustBootstrapLRT(df, modelName = "VVE")



#VISUALIZE
library(tidyverse)

means <- data.frame(mod$parameters$mean) %>%
	rownames_to_column() %>%
	rename(Characteristic = rowname) %>%
	pivot_longer(cols = c(X1, X2), names_to = "Profile", values_to = "Mean") %>%
	mutate(Mean = round(Mean, 2))

means %>%
	ggplot(aes(Characteristic, Mean, group = Profile, color = Profile)) +
	geom_point(size = 2.25) +
	geom_line(size = 1.25) +
	scale_x_discrete(limits = c("HP","Attack","Defense","Speed")) +
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
