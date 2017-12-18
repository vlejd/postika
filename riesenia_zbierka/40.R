#uloha 40
semienka <- read.table(file = "zbierka_data/klicenie_semiacok.txt", header = TRUE)
head(semienka)
attach(semienka)

zasadeneV <- zasadene[znacka == "Veleliby"]
vykliciloV <- vyklicilo[znacka == "Veleliby"]
zasadeneS <- zasadene[znacka == "Smrzice"]
vykliciloS <- vyklicilo[znacka == "Smrzice"]

Xv <- sum(vykliciloV)
nv <- sum(zasadeneV)
Xs <- sum(vykliciloS)
ns <- sum(zasadeneS)

pvHAT <- Xv/nv # 0.960373
psHAT <- Xs/ns # 0.8857143

# zda sa ze klicenie semienok Smrzice ma vasciu uspesnost
# H0: pv <= ps vs. H1: pv > ps
# MZR: H0 zamietame, ked pvHAT >> psHAT
V <- (pvHAT - psHAT) / sqrt((pvHAT*(1-pvHAT)/nv) + (psHAT*(1-psHAT)/ns)) # 3.518559
qnorm(0.95) # 1.644854 < V => H0 zamietame
1 - pnorm(V) # p-value = 0.0002169489 < 5% => H0 zamietame
# Mozme prehlasit, ze Smrzice semienka maju vacsiu uspesnost vyklicenia

# IS pre rozdiel klicivosti
dolna <- (pvHAT - psHAT) - qnorm(0.975)*sqrt((pvHAT*(1-pvHAT)/nv) + (psHAT*(1-psHAT)/ns))
horna <- (pvHAT - psHAT) + qnorm(0.975)*sqrt((pvHAT*(1-pvHAT)/nv) + (psHAT*(1-psHAT)/ns))
dolna # 0.0330711
horna # 0.1162462
# S 95% pravdep. je pravy rozdiel pravdepodobnosti medzi 3.3% az 11.62%