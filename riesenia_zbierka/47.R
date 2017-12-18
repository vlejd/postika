nhl <- read.table(file = "zbierka_data/nhl_vychod_zapad.txt", header = TRUE)
head(nhl)
attach(nhl)
platy_z <- platy[konferencia == "zapad"]
body_z <- body[konferencia == "zapad"]
platy_v <- platy[konferencia == "vychod"]
body_v <- body[konferencia == "vychod"]

plot(platy_z, body_z, main = "Zapad")
plot(platy_v, body_v, main = "Vychod")

cor(platy_z, body_z) # 0.2451867
cor(platy_v, body_v) # 0.321325

# data su z normalneho rozdelenia, mozme pouzit Pearsona
shapiro.test(platy_z) # p-value = 0.3136
shapiro.test(body_z) # p-value = 0.381
shapiro.test(platy_v) # p-value = 0.1953
shapiro.test(body_v) # p-value = 0.381

# H0: rho <= 0 vs. H1: rho > 0
# p-value = 0.1892 > 5% => H0 sa nezamieta
cor.test(body_z, platy_z, alternative = "greater", method = "pearson")

# H0: rho <= 0 vs. H1: rho > 0
# p-value = 0.1214 > 5% => H0 sa nezamieta
cor.test(body_v, platy_v, alternative = "greater", method = "pearson")

# Moze sa vobec toto robit, ked rho nemame potvrdene?
Z_z <- atanh(cor(platy_z, body_z))
n_z <- length(platy_z)
Z_v <- atanh(cor(platy_v, body_v))
n_v <- length(platy_v)
# testujeme
# H0: rho_v <= rho_z vs. H1: rho_v > rho_z
stat <- (Z_v - Z_z) / sqrt(1/(n_v-3) + 1/(n_z-3)) #0.2029129
qnorm(0.95) # 1.644854
1 - pnorm(stat) # 0.4196015 > 5% => H0 sa nezamieta
