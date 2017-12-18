#uloha 50
maj <- read.table(file = "zbierka_data/studeny_maj.txt", header = TRUE)
head(maj)
attach(maj)
teplotaT <- teplota[druzstvo == "Trstena"]
urodaT <- uroda[druzstvo == "Trstena"]
nT <- length(teplotaT)

teplotaM <- teplota[druzstvo == "Marcelova"]
urodaM <- uroda[druzstvo == "Marcelova"]
nM <- length(teplotaM)

plot(teplotaT, urodaT)
is.normal <- function(data){
  ks.test(data, "pnorm", mean = mean(data), sd = sqrt(var(data)))
}

#data su z normalneho rozdelenia, mozme pouzit pearsona
is.normal(teplotaT) # p-value = 0.8657
is.normal(urodaT) # p-value = 0.8258

# testujeme ci je rfo < 0
# H0: rho >= 0 vs. H1: rho < 0
# MZR: H0 zamietame, ked je rho << 0
# rho = -0.5042585
# p-value = 0.01169 < 5% => H0 zamietame a pranostiku potvrdzujeme
cor.test(teplotaT, urodaT, alternative = "less", method = "pearson")

#data su z normalneho rozdelenia, mozme pouzit pearsona
is.normal(teplotaM) # p-value = 0.9749
is.normal(urodaM) # p-value = 0.7858

rhoT <- cor(teplotaT, urodaT) # -0.5042585 Sever Slovenska
rhoM <- cor(teplotaM, urodaM) # -0.7310254 Juh Slovenska

# Ideme testovat rovnost korelacii
# H0: rhoT == rhoM vs. H1: rhoT != rhoM
# MZR: H0 zamietame, ked rhoT << rhoM alebo rhoT >> rhoM

Zt <- atanh(rhoT)
Zm <- atanh(rhoM)

stat <- (Zt - Zm) / sqrt(1/(nT-3) + 1/(nM-3)) # 1.096003
qnorm(0.95) # -1.644854 < stat < 1.644854 => H0 nezamietame
2*(1-pnorm(stat)) # 0.2730777 > 5% => H0 nezamietame
# Mozme povedat, ze na J SR plati pranostika rovnako silno