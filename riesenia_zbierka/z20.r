pocasie <- read.table("../zbierka_data/weatherDOTcom.txt", header = TRUE)
attach(pocasie)

plot(predpoved, skutocnost)
cov(predpoved, skutocnost)

rozdiely <- predpoved-skutocnost
ks.test(rozdiely, "pnorm", mean=mean(rozdiely), sd=sqrt(var(rozdiely)))
# niesu normalne

# H0: mu_pred <= mu_sku vs.   H1: mu_pred > mu_sku
wilcox.test(predpoved, skutocnost, alternative = "greater", paired=TRUE)
wilcox.test(rozdiely, alternative = "greater", mu=0)
# p-value = < 5%, potvrdila sa hypoteza

library(moments)
skewness(skutocnost) # > 0, pozitivny
qqnorm(skutocnost) # U-cko
