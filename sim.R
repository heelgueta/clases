#simulator 1 = EL CFA EN QUE NADA CORRELACIONA

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
