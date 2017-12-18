#uloha 28
stb <- read.table(file='zbierka_data/stb.txt', header = TRUE)
head(stb)
attach(stb)

plot(ziskanychK / oslovenychK, type = 'l')
plot(hist(ziskanychK / oslovenychK))
plot(ziskanychP / oslovenychP, type = 'l')
plot(hist(ziskanychP / oslovenychP))
boxplot(ziskanychK/oslovenychK, ziskanychP/oslovenychP, notch=TRUE)

HATpk <- sum(ziskanychK) / sum(oslovenychK)
HATpp <- sum(ziskanychP) / sum(oslovenychP)

# Kosice sa zdaju byt nachylnejsie, otestujeme
# H0: pk <= pp  vs.  H1: pk > pp

V <- (HATpk - HATpp)/sqrt(HATpk*(1-HATpk)/sum(oslovenychK) + HATpp*(1-HATpp)/sum(oslovenychP))
# H0 zamietame ak V > u(5%)
qnorm(0.95)
# p-value = 0.0154086 < 5% => H0 zamietame takze Kosicania su zavislejsi
1-pnorm(V) 