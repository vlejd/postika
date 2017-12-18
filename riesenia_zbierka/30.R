#uloha 30
auta <- read.table(file='zbierka_data/volvo_vs_skoda.txt', header = TRUE)
head(auta)
attach(auta)
X1 <- sum(volvoZ)
n1 <- sum(volvo)
X2 <- sum(skodaZ)
n2 <- sum(skoda)

HATpv <- X1 / n1 # 0.07657
HATps <- X2 / n2 # 0.12871

# vyzera ze skoda pritahuje viac policajtov
# H0: ps <= pv  vs.  H1: ps > pv

V <- (HATps - HATpv)/sqrt(HATps*(1-HATps)/n2 + HATpv*(1-HATpv)/n1) # 1.379248

# H0 zamietame ak V > u(5%)
qnorm(0.95) # 1.644854
# p-value = 0.08390922 > 5% => H0 nezamietame takze skoda nepritahuje viac pozornosti policajtov
1-pnorm(V) 