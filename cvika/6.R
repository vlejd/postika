# PAROVY t-TEST:

podrazky <- read.table("e:\\podrazky.txt" , header=TRUE)
attach(podrazky)

plot( materialA-materialB )
abline(a=0,b=0)

boxplot(materialA,materialB)

plot( materialA , materialB )

#parovy t-test:
t.test(materialA , materialB ,
       alternative="greater" ,
       paired=TRUE)

#toto je zly postup:
var.test(materialA , materialB)
t.test(materialA , materialB ,
       alternative="greater" ,
       var.equal=TRUE)

# PRECO TUZIME PO NORMALITE?

# TESTY NORMALITY:

michelson <- read.table("e:\\michelson.txt" , header=TRUE , sep=",")
attach(michelson)
hist(Velocity,col="blue")

hist(materialA - materialB , col="blue")

# Kolmogorovov-Smirnovov test:

plot( ecdf(Velocity) )
osX <- seq(600,1100,by=1)
lines( osX , pnorm(osX,mean=mean(Velocity),sd=sqrt(var(Velocity))) ,
       col="red")

ks.test( Velocity ,
         "pnorm" ,
         mean=mean(Velocity) ,
         sd=sqrt(var(Velocity)) )

ks.test( Velocity[Trial=="first"] ,
         "pnorm" ,
         mean=mean(Velocity[Trial=="first"]) ,
         sd=sqrt(var(Velocity[Trial=="first"])) )

ks.test( Velocity[Trial=="second"] ,
         "pnorm" ,
         mean=mean(Velocity[Trial=="second"]) ,
         sd=sqrt(var(Velocity[Trial=="second"])) )

ks.test( Velocity[Trial=="fourth"] ,
         "pnorm" ,
         mean=mean(Velocity[Trial=="fourth"]) ,
         sd=sqrt(var(Velocity[Trial=="fourth"])) )


ks.test( materialA - materialB ,
         "pnorm" ,
         mean=mean(materialA - materialB) ,
         sd=sqrt(var(materialA - materialB)) )

# Shapirov-Wilkov test (1965):
shapiro.test( materialA - materialB)


# NEPARAMETRICKE TESTY:

levi.strauss <- read.table("e:\\levi_strauss.txt" , header=TRUE , na.string="*")
attach(levi.strauss)

boxplot(levi.strauss)

#data nie su z normalneho rozdelenia:
shapiro.test(PT1)
shapiro.test(PT2)

#Wilcoxonov znamienkovany poradovy test:
mean(PT1)
wilcox.test(PT1,alternative="greater",mu=0)

# porovnanie 1. a 2. tovarne:
mean(PT1)
mean(PT2)

#2-vyberovy Wilcoxonov test:
wilcox.test(PT1,PT2,
            alternative="two.sided")

#______________________
#takto sa to robit nema:

#test rovnosti disperzii:
var.test(PT1,PT2)
#2-vyberovy t-test:
t.test(PT1,PT2,
       alternative="two.sided" ,
       var.equal=TRUE)
#Welchov 2-vyberovy t-test:
t.test(PT1,PT2,
       alternative="two.sided",
       var.equal=FALSE)
#______________________


#Wilcoxonov znamienkovany test (pre parove pozorovania):
wilcox.test(materialA,materialB,
            alternative="greater",
            paired=TRUE)