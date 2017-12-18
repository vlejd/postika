#uloha 54
zber <- read.table(file = "zbierka_data/zber_jahod.txt", header = TRUE)
head(zber)
attach(zber)

jahodyMODEL <- lm(formula = jahody~0 + poliaci + slovaci, x = TRUE)
jahodyMODEL
# Poliak denne v priemere vyzbiera 20.23kg jahod
# Slovak denne v priemere vyzbiera 19.99kg jahod

# predpokladame, ze odchylky su z normalneho rozdelenia
plot(jahodyMODEL, which = 1)  # zda sa, ze nemame systematicke chyby = rovnomerny mrak

err <- jahodyMODEL$residuals
hist(err)
ks.test(err, "pnorm", mean = mean(err), sd = sqrt(var(err)))
# odchylky su normalne, tento predpoklad je teda splneny
# mohli by sme este overit, ze ich stredna hodnota je 0
t.test(err, alternative = "two.sided", mu = 0)
# a ano, je

# ideme testovat hypotezu o kontraste
# zda sa, ze poliacci su usilovnejsi, tak otestujeme
# H0: beta_p <= beta_s   vs.    H1: beta_p > beta_s
# H0 zamietneneme ak T (a = (1, -1)) >> 0
a <- c(1, -1)
n <- length(jahody)
X <- jahodyMODEL$x
betaHAT <- jahodyMODEL$coefficients
S <- summary(jahodyMODEL)$sigma
k <- 2

T <- (t(a) %*% betaHAT) / (S * sqrt( t(a) %*% solve(t(X) %*% X) %*% a)) # 1.05965
qt(0.95, n-k) # 1.670649
1 - pt(T, n-k) # p-value 0.1467755 -> nemozeme tvrdit, ze poliaci su usilovnejsi