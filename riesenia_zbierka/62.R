#--------------------------------------------------------------------------------------
#uloha 62
korytnacky <- read.table(file = "zbierka_data/korytnacky.txt", header = TRUE)
head(korytnacky)
attach(korytnacky)
korytnackyMODEL <- lm(PN ~ 1 + michelangelo + raphael + donatello + leonardo, x = TRUE)

# Intercept vlastne predstavuje Majstera Sprintera aky ma vplyv jeho uder na PN
# michelangelo, raphael, donatello a leonardo - kolko dni PN sposobi ich jeden uder

err <- korytnackyMODEL$residuals
hist(err)
# p-value = 0.2398 > 5% => data su z normalneho rozdelenia
ks.test(err, "pnorm", mean = mean(err), sd = sqrt(var(err)))


# Sprinterove udery su rovnako nicive ako Donatellovi beta0 == beta3
# H0: beta0==beta3 vs. H1: beta0 != beta3
# MZR: H0 zamietame ak beta0 << beta1 alebo beta0 >> beta3
a <- c(-1, 0, 0, 1, 0)
betaHAT <- korytnackyMODEL$coefficients
n <- length(PN)
k <- length(betaHAT)
X <- korytnackyMODEL$x
S <- sqrt(sum((korytnackyMODEL$residuals)^2) / (n - k))

T <- (t(a) %*% betaHAT) / S * sqrt(t(a) %*% solve(t(X) %*% X) %*% a) #1.536745
qt(0.95, df = n-k) # 1.679427 H0 nezamietame lebo -1.679427 < T < 1.679427
# Majster ma rovnako silne udery ako Donatello
2*(1-pt(T, df = n - k)) # p-value = 0.131358 > 5% => H0 nezamietame


# Michelangelove a Rafaelove udery nesposobuju ziadnu ujmu na zdravi je test o submodeli
korytnackyMODEL
# H0: beta1 == 0 & beta2 == 0 vs. H1: beta1 != 0 | beta2 != 0
# MZR: H0 zamietame, ked SSeSUBMODELU >> SSe
korytnackySUBMODEL <- lm(PN ~ 1 + donatello + leonardo, x = TRUE)
SSe <- sum((korytnackyMODEL$residuals)^2)
SSeSUBMODEL <- sum((korytnackySUBMODEL$residuals)^2)
m <- 2 # pocet zabitych parametrov
n <- length(PN)
k <- length(korytnackyMODEL$coefficients)
T <- ((SSeSUBMODEL - SSe)/m) / (SSe / (n - k)) # 37.95026
qf(0.95, df1 = m, df2 = n - k) # 3.204317 
1 - pf(T, df1 = m, df2 = n - k) # p-value = 2.201144e-10 < 5% => H0 zamietame
# Aspon jeden z nich ma vplyv na PN