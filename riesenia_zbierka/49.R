#uloha 49
kupaliska <- read.table(file = "zbierka_data/kupaliska.txt", header = TRUE)
head(kupaliska)
tail(kupaliska)
attach(kupaliska)

teplotaA <- teplota[kupalisko == "Londyn"]
ludiaA <- ludia[kupalisko == "Londyn"]

teplotaS <- teplota[kupalisko == "Diakovce"]
ludiaS <- ludia[kupalisko == "Diakovce"]

is.normal <- function(data) {
  ks.test(data, "pnorm", mean = mean(data), sd = sqrt(var(data)))
}

# data su z normalneho rozdelenia a preto mozme pouzit pearsona
is.normal(teplotaA) # p-value = 0.6709
is.normal(ludiaA) # p-value = 0.8572
is.normal(teplotaS) # p-value = 0.8686
is.normal(ludiaS) # p-value = 0.8084

# Vyzera to, ze ludia na SR su viac ovplyvneny teplotou
cor(teplotaA, ludiaA) # 0.4619113
cor(teplotaS, ludiaS) # 0.8607794

# p-value = 0.01005 < 5% => H0 zamietame
cor.test(teplotaA, ludiaA, alternative = "greater", method = "pearson")

# p-value = 1.71e-14 < 5% => H0 zamietame
cor.test(teplotaS, ludiaS, alternative = "greater", method = "pearson")

# Otestujme, ci su ludia v SR ovplyvneny viac teplotou ako ludia v A
# H0: rhoS <= rhoA vs. H1: rhoS > rhoA
Za <- atanh(cor(teplotaA, ludiaA))
Zs <- atanh(cor(teplotaS, ludiaS))
na = length(teplotaA)
ns = length(teplotaS)
V <- (Zs - Za) / sqrt((1 / (ns-3)) + (1 / (na-3))) # 3.026845
qnorm(0.95) # 1.644854 < V => H0 zamietame
1 - pnorm(V) # p-value = 0.001235602 < 5% => H0 zamietame