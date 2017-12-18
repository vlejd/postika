#uloha 29
#TREBA SKONTROLOVAT NIE SOM SI ISTA :)
pes <- read.table(file='zbierka_data/pes_a_drogy.txt', header = TRUE)
head(pes)
attach(pes)
X <- sum(nasiel)
n <- sum(auta)
HATp <- X / n

# H0: p >= 0.95 vs H1: p < 0.95
# Metoda zdraveho rozumu, H0 zamietame, ak HATp << 0.95
# H0 zamietame V=(HATp - 0.95) / sqrt(HATp*(1-HATp)/n) << 0
# u(5%) = -u(95%) = -1.644854
# V = -1.098768
# V > -u(0.95)
# V nie je extremne zaporne => H0 nezamietame
# rozdiel nie je statisticky vyznamny
# p-value plocha od V smerom dole (13.59% > 5%) H0 nezamietame
# p(V < -1.098768) = 13.59% > 5% => H0 nezamietame
# je malo dat
# Fu(-1.098768)
V <- (HATp - 0.95) / sqrt(HATp*(1-HATp)/n)
qnorm(0.05)
#rucny vypocet p-value
pnorm(V)

# IS 
dolna <- HATp - qnorm(0.975)*sqrt(HATp*(1-HATp)/n)
horna <- HATp + qnorm(0.975)*sqrt(HATp*(1-HATp)/n)
# s 95% pravdepodobnostou lezi p v intervale, podla nameranych dat