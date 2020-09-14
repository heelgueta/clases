#install.packages("tidyLPA")
library(tidyLPA)
library(dplyr)

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




df %>%
	estimate_profiles(1:5, 
										variances = c("equal", "varying"),
										covariances = c("zero", "varying")) %>%
	compare_solutions(statistics = c("AIC", "BIC","AWE","CLC","KIC"))

compare_solutions(estimate_profiles(df,1:5,
									variances = c("equal", "varying"),
									covariances = c("zero", "varying"))

estimate_profiles(df = df, n_profiles = 1:5, 
									variances = c("equal", "varying"), 
									covariances = c("zero", "varying"))

#1. Equal variances and covariances fixed to 0
#2. Varying variances and covariances fixed to 0
#3. Equal variances and equal covariances
#4. Varying variances and equal covariances (not able to be fit w/ mclust)
#5. Equal variances and varying covariances (not able to be fit w/ mclust)
#6. Varying variances and varying covariances

df %>%
	estimate_profiles(4) %>% 
	plot_profiles(alpha_range = 0)

?plot_profiles


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

df %>%
	estimate_profiles(1:5, 
										variances = c("equal", "varying"),
										covariances = c("zero", "varying")) %>%
	compare_solutions(statistics = c("AIC", "BIC","AWE", "CLC", "KIC"))
#1. Equal variances and covariances fixed to 0
#2. Varying variances and covariances fixed to 0
#3. Equal variances and equal covariances
#4. Varying variances and equal covariances (not able to be fit w/ mclust)
#5. Equal variances and varying covariances (not able to be fit w/ mclust)
#6. Varying variances and varying covariances


colnames(df)

df %>%
	estimate_profiles(4) %>% 
	plot_profiles(alpha_range = 0.4)


