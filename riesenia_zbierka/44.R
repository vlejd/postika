navstevnost <- read.table(file = "zbierka_data/navstevnost.txt", header = TRUE)
head(navstevnost)
tail(navstevnost)
attach(navstevnost)

points_old <- points[sezona == "92-93"]
divaci_old <- divaci[sezona == "92-93"]
points_new <- points[sezona == "06-07"]
divaci_new <- divaci[sezona == "06-07"]

plot(points_old, divaci_old, main = "Sezona 92-93")
plot(points_new, divaci_new, main = "Sezona 06-07")

# pearson korelacny koeficient
rho_old <- cor(points_old, divaci_old) # 0.410008444882311
rho_new <- cor(points_new, divaci_new) # 0.673864606148048

# Zda sa ze v sezone 06-07 je vacsia zavislost
# Zistime normalitu dat

# divaci_old a divaci_new nie su z normalneho rozdelenia 
shapiro.test(points_old) # p-value = 0.07454
shapiro.test(divaci_old) # p-value = 0.01452
shapiro.test(points_new) # p-value = 0.2478
shapiro.test(divaci_new) # p-value = 0.01974

#musime pouzit spearmanovu metodu
# H0: rho <= 0 vs. H1: rho > 0
# p-value = 0.03369 < 5% => H0 zamietame
res <- cor.test(points_old, divaci_old, alternative = "greater", method = "spearman")
rho_old <- res$estimate # rho_old = 0.3969438

#musime pouzit spearmanovu metodu
# H0: rho <= 0 vs. H1: rho > 0
# p-value = 0.002971 < 5% => H0 zamietame
res <- cor.test(points_new, divaci_new, alternative = "greater", method = "spearman")
rho_new <- res$estimate # rho_new = 0.5921747 

# Vyzera to, ze v sezone 06-07 viac koreluje navstevnost s uspesnostou timu
Z_old <- atanh(rho_old)
n_old = length(points_old)
Z_new <- atanh(rho_new)
n_new <- length(points_new)
# ideme testovat
# H0: rho_new <= rho_old vs. H1: rho_new > rho_old
# H0 zamietame ak rho_new >> rho_old
stat <- (Z_new - Z_old) / sqrt((1 / (n_new-3)) + (1 / (n_old-3)))
# stat je menej ako kriticka hodnota
stat # 0.781769 
# kriticka hodnota 1.644854
qnorm(0.95)
# p-value = 0.2171752 > 5% => H0 nezamietame
1 - pnorm(stat)

# statisticky nevyznamne, mame malo dat