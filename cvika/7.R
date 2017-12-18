# TESTOVANIE PRAVDEPODOBNOSTI:

sestry.a.rukavice <- read.table("./data/sestry_a_rukavice.txt" ,header=TRUE, na.strings="*")
sestry.a.rukavice <- na.omit(sestry.a.rukavice)
attach(sestry.a.rukavice)

X <- sum(Gloves[Period==1])
n <- sum(Observed[Period==1])
HATp <- X/n
#testovacia statistika:
V <- (HATp-0.20) / sqrt(HATp*(1-HATp)/n)
#p-hodnota:
1-pnorm(V)

# test obojstrannej alternativy:
#testovacia statistika:
V <- (HATp-0.3) / sqrt(HATp*(1-HATp)/n)
#p-hodnota:
2*( 1-pnorm( abs(V) ) )


####################
# co je to p-value?#
####################


# IS pre p:
#dolna hranica:
HATp-sqrt(HATp*(1-HATp)/n)*qnorm(.975)
#horna hranica:
HATp+sqrt(HATp*(1-HATp)/n)*qnorm(.975)


# porovnanie 2 pravdepodobnosti:
X1 <- sum(Gloves[Period==1 & Experience<=3])
n1 <- sum(Observed[Period==1 & Experience<=3])
X2 <- sum(Gloves[Period==1 & Experience>3])
n2 <- sum(Observed[Period==1 & Experience>3])
HATp1 <- X1/n1
HATp2 <- X2/n2
#testovacia statistika:
V <- (HATp1-HATp2) / sqrt( HATp1*(1-HATp1)/n1 + HATp2*(1-HATp2)/n2 )
#p-hodnota:
2*( 1-pnorm(V) )
#alebo inac:
2*pnorm(-V)


# IS pre p1-p2:
#dolna hranica:
HATp1-HATp2 - sqrt( HATp1*(1-HATp1)/n1 + HATp2*(1-HATp2)/n2 ) * qnorm(0.975)
#horna hranica:
HATp1-HATp2 + sqrt( HATp1*(1-HATp1)/n1 + HATp2*(1-HATp2)/n2 ) * qnorm(0.975)



# KORELACNA ANALYZA:

staty <- read.table(file="e:\\americke_staty.txt" , header=TRUE)
attach(staty)

#vyberova kovariancia:
var( Income , Frost )

#vyberovy korelacny koeficient
cor( Income , Frost )

#korelacna matica:
#toto nefunguje:
cor( staty )
#toto funguje:
cor( staty[,-1] )

plot( staty[-1] )

#normalita je zamietnuta pri "Population" a "Area":
ks.test(Population , "pnorm" , mean=mean(Population), sd=sqrt(var(Population)) )
ks.test(Income , "pnorm" , mean=mean(Income) , sd=sqrt(var(Income)) )
ks.test(Illiteracy , "pnorm" , mean=mean(Illiteracy) , sd=sqrt(var(Illiteracy)) )
ks.test(Life.Exp , "pnorm" , mean=mean(Life.Exp) , sd=sqrt(var(Life.Exp)) )
ks.test(Murder , "pnorm" , mean=mean(Murder) , sd=sqrt(var(Murder)) )
ks.test(HS.Grad , "pnorm" , mean=mean(HS.Grad) , sd=sqrt(var(HS.Grad)) )
ks.test(Frost , "pnorm" , mean=mean(Frost) , sd=sqrt(var(Frost)) )
ks.test(Area , "pnorm" , mean=mean(Area) , sd=sqrt(var(Area)) )

# 0.22
cor.test( Income , Frost ,
          alternative="two.sided" ,
          method="pearson" )

# 0.61
cor.test( Income , HS.Grad ,
          alternative="greater" ,
          method="pearson" )

# -0.33
cor.test( Population , Frost ,
          alternative="less" ,
          method="spearman" )