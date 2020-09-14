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

df <- read.csv("datasets/pokemon.csv")
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

library(tidyLPA)
library(dplyr)

colnames(df)


df %>%
	single_imputation() %>%
	estimate_profiles(1:5, 
										variances = c("equal", "varying"),
										covariances = c("zero", "varying")) %>%
	compare_solutions(statistics = c("AIC", "BIC","AWE","CLC","KIC"))


df %>%
	single_imputation() %>%
	estimate_profiles(3)

#?estimate_profiles;
#models are
#1. Equal variances and covariances fixed to 0
#2. Varying variances and covariances fixed to 0
#3. Equal variances and equal covariances
#4. Varying variances and equal covariances (not able to be fit w/ mclust)
#5. Equal variances and varying covariances (not able to be fit w/ mclust)
#6. Varying variances and varying covariances



df %>%
	single_imputation() %>%
	scale() %>%
	estimate_profiles(3) %>% 
	plot_profiles()


