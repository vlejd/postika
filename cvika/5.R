# t - T E S T Y:

michelson <- read.table("../data/michelson.txt" , header=TRUE , sep=",")
attach(michelson)

mean( Velocity )

# jednostranny t-test:
t.test( Velocity , alternative="greater" , mu=840 )
qt(0.95 , df=99 )

# 90% interval spolahlivosti:
t.test( Velocity , alternative="greater" , mu=840 , conf.level=.90 )

# obojstranny t-test:
# (presna rychlost svetla je 299 792 km/s)
t.test( Velocity , alternative="two.sided" , mu=792 )
qt(0.975 , df=99 )

# jednostranny t-test
# vtedy znama rychlost svetla bola 299 860 km/s
t.test( Velocity ,
        alternative="less" ,
        mu=860 )
-qt(.95 , df=99 )
# rucny vypocet p-hodnoty
pt(-0.9619 , df=99)

boxplot(Velocity~Trial)

# porovnanie 2. a 4. dna:
# test rovnosti disperzii:
var.test( Velocity[Trial=="second"] , Velocity[Trial=="fourth"] )

#2-vyberovy t-test
t.test( Velocity[Trial=="second"] , Velocity[Trial=="fourth"] ,
        alternative="two.sided" ,
        var.equal=TRUE )

# porovnanie 1. a 2. dna:
# test rovnosti disperzii:
var.test( Velocity[Trial=="first"] , Velocity[Trial=="second"] )
#Welchov 2-vyberovy t-test:
t.test( Velocity[Trial=="first"] , Velocity[Trial=="second"] ,
        alternative="two.sided" ,
        var.equal=FALSE )

# PAROVY t-TEST:

podrazky <- read.table("../data/podrazky.txt" , header=TRUE)
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