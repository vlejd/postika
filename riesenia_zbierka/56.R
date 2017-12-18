#uloha 56
superstar <- read.table(file = "zbierka_data/superstar.txt", header = TRUE)
head(superstar)
attach(superstar)

reklama_z <- reklama[pohlavie == "Z"]
predane_z <- predane[pohlavie == "Z"]
reklama_m <- reklama[pohlavie == "M"]
predane_m <- predane[pohlavie == "M"]

zenyMODEL = lm(formula = predane_z ~ 1 + reklama_z)
muziMODEL = lm(formula = predane_m ~ 1 + reklama_m)

plot(reklama_z, predane_z, main = "Albumy zeny a muzi", col = "red")
abline(zenyMODEL$coefficients, col = "red")
points(reklama_m, predane_m, col = "blue")
abline(muziMODEL$coefficients, col = "blue")

# pre porovnanie priamok potrebujeme vytvorit zlozeny model
n1 <- length(predane_z)
n2 <- length(predane_m)
stlpec1 <- c(rep(1, times = n1), rep(0, times = n2))
stlpec2 <- c(rep(0, times = n1), rep(1, times = n2))
stlpec3 <- c(reklama_z, rep(0, times = n2))
stlpec4 <- c(rep(0, times = n1), reklama_m)
Y = c(predane_z, predane_m)
X <- cbind(stlpec1,stlpec2,stlpec3,stlpec4)
zlozenyMODEL <- lm(formula = Y ~ 0 + X, x = TRUE)
zlozenyMODEL

# testujeme
# H0: beta3 <= beta2 vs. H1: beta3 > beta2
# H0 zamietame ak beta3 - beta1 >> 0
# H0 zamietneneme ak T (a = (0, 0, -1, 1)) >> 0
a <- c(0, 0, -1, 1)
n <- length(predane)
X <- zlozenyMODEL$x
betaHAT <- zlozenyMODEL$coefficients
S <- summary(zlozenyMODEL)$sigma
k <- length(betaHAT)

T <- (t(a) %*% betaHAT) / (S * sqrt( t(a) %*% solve(t(X) %*% X) %*% a)) # 3.724261
qt(0.95, n-k) # 1.688298
1 - pt(T, n-k) # p-value 0.0003341997 < 5% => H0 zamietame
# reklama u muzov ma vyssi vplyv na predaj ako u zien