#uloha 61
auta <- read.table(file = "zbierka_data/peugeot_skoda.txt", header = TRUE)
head(auta)
attach(auta)

# kazdym kilometrom sa znizi obsah nadrze o 0.06525
pMODEL <- lm(nadrzP ~ 1 + kilometreP, x = TRUE)

# kazdym kilometrom sa znizi obsah nadrze o 0.07993
sMODEL <- lm(nadrzS ~ 1 + kilometreS, x = TRUE)

plot(kilometreP, nadrzP, col = "blue")
abline(pMODEL$coefficients, col = "blue", lty = 1)

points(kilometreS, nadrzS, col = "red")
abline(sMODEL$coefficients, col = "red", lty = 2)

legend(x=200 ,
       y=42 ,
       legend=c("Peugeot","Skoda") ,
       col = c("blue", "red"),
       lty = c(1, 2)
)

pN <- length(kilometreP)
sN <- length(kilometreS)
stlpec1 <- c(rep(1, pN), rep(0, sN))
stlpec2 <- c(rep(0, pN), rep(1, sN))
stlpec3 <- c(kilometreP, rep(0, sN))
stlpec4 <- c(rep(0, pN), kilometreS)
X <- cbind(stlpec1, stlpec2, stlpec3, stlpec4)
Y <- c(nadrzP, nadrzS)
zlozenyMODEL <- lm(Y ~ 0 + X, x = TRUE)

err <- zlozenyMODEL$residuals
hist(err)
# p-value = 0.8898 > 5% => data su z normalneho rozdelenia
ks.test(err, "pnorm", mean = mean(err), sd = sqrt(var(err)))

# Ideme otestovat, ci beta2 a beta3 sa rovnaju
# H0: beta2 == beta3 vs. H1: beta2 != beta3
# MZR: H0 zamietame, ak beta2 << beta3 alebo beta2 >> beta3
a <- c(0, 0, 1, -1)
betaHAT <- zlozenyMODEL$coefficients
X <- zlozenyMODEL$x
n <- pN + sN
k <- length(betaHAT)
SSe <- sum((zlozenyMODEL$residuals)^2)
S <- sqrt(SSe / (n - k))
T <- (t(a) %*% betaHAT) / S*sqrt(t(a)%*%solve(t(X)%*%X)%*%a) 0.0002415818
qt(0.95, df = n - k) #1.672522 H0 nezamietame, pretoze -1.672522 < T < 1.672522
2 * (1 - pt(T, df = n - k)) # p-value = 0.9998081 > 5% => H0 nezamietame a mozmme
# povedat, ze auta maju rovnaku spotrebu v zavislosti od poctu kilometrov