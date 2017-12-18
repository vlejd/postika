#uloha 51
volby <- read.table(file = "zbierka_data/nezamestnanost_volby.txt", header = TRUE)
head(volby)
attach(volby)
is.normal <- function(data){
  ks.test(data, "pnorm", mean = mean(data), sd = sqrt(var(data)))
}

# data su z normalneho rozdelenia, mozme pouzit pearsona
is.normal(nezamestnanost) # p-value = 0.321
is.normal(podpora) # p-value = 0.8186

cor(nezamestnanost, podpora) # 0.1449977
# H0: rho <= 0 vs. H1: rho > 0
# p-value = 0.1012 > 5% => nezamietame H0 
# nemozme tvrsit, ze nezamestnanost pozitivne suvisi s mierou podpory danej strany
cor.test(nezamestnanost, podpora, alternative = "greater", method = "pearson")

head(volby)
kraj
nezamestnanostBA <- nezamestnanost[kraj == "BA"]
podporaBA <- podpora[kraj == "BA"]
nBA <- length(nezamestnanostBA)

nezamestnanostKE <- nezamestnanost[kraj == "KE"]
podporaKE <- podpora[kraj == "KE"]
nKE <- length(nezamestnanostKE)

# Mame malo dat, tak pouzijeme shapiro test na normalitu dat
# data su z normalneho rozdelenia
shapiro.test(nezamestnanostBA) # p-value = 0.3843
shapiro.test(podporaBA) # p-value = 0.4032
shapiro.test(nezamestnanostKE) # p-value = 0.2015
shapiro.test(podporaKE) # p-value = 0.4789

cor(nezamestnanostBA, podporaBA) # 0.7303617
cor(nezamestnanostKE, podporaKE) # 0.1894002

Zba <- atanh(cor(nezamestnanostBA, podporaBA))
Zke <- atanh(cor(nezamestnanostKE, podporaKE))

stat <- (Zba - Zke) / sqrt(1/(nBA-3) + 1/(nKE-3)) # 1.294164
qnorm(0.95) # 1.644854 > stat => H0 nezamietame
1 - pnorm(stat) # 0.09780432 > 5% => H0 nezamietame
# Nemozme tvrdit o silnejsom suvise... Potrebovali by sme viac dat