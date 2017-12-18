# O B R A Z K Y :

#######
#znovu o kraboch:

kraby <- read.table(file="../data/kraby.txt" , header=TRUE , dec=",")
attach(kraby)

aggregate(  kraby[,4:8]  ,  by = list(sp,sex) , FUN=mean)

# zlozitejsi boxplot:
boxplot(CL~sp+sex)

# este viac 'vymakany' boxplot:
boxplot(CL~sp+sex,
        notch=TRUE,
        varwidth=TRUE,
        col=c("blue","orange","blue","orange"),
        names=c("samicky","samicky","samcekovia","samcekovia")
        )

# povedz o propagatorovi medianu


# OBYCAJNE GRAFY:

osX <- seq(-6,6,by=.1)
plot(osX , dnorm(osX) )
plot(osX , dnorm(osX),
     type="l")
lines( osX , dcauchy(osX) )


# 2 grafy v jednom:
osX <- seq(-6,6,by=.01)
plot(osX , dnorm(osX) ,
     type="l",
     ylim=c(0,.8)
    )
lines(osX,dcauchy(osX),
      lty=2)
title(main="Lahke a tazke chvosty")
legend(x=-6 ,
       y=.7 ,
       legend=c("Normalne rozdelenie","Cauchyho rozdelenie") ,
       lty=c(1,2)
      )
text(locator(2) , labels=c("Normalne","Cauchyho"))

#preco trva kreslenie tak dlho?

# este krajsi graf:
osX <- seq(-6,6,by=.1)
plot(osX,dnorm(osX),
     type="l",
     ylim=c(0,.8),
     col="red",
     xlab="x",
     ylab="2 rozne hustoty")
lines(osX,dcauchy(osX),
      col="blue")
title(main="Lahke a tazke chvosty")
legend(x=-6,y=.7,
       legend=c("Normalne rozdelenie","Cauchyho rozdelenie"),
       lty=c(1,1),
       col=c("red","blue") )


# HISTOGRAM:

loteria1 <- scan(file="e:\\loteria1.txt" , comment.char = "#")

hist(loteria1)
hist(loteria1, nclass=15)
hist(loteria1, nclass=200)
hist(loteria1,
     xlab="vysky vyhier",
     ylab="pocetnosti"  )

#do loterie pridame outlier 1500:
upravena.loteria <- c(loteria1,1500)
hist(upravena.loteria)

FD.pocet.tried <- function(x)
 {
	h <- 2* (quantile(x,probs=0.75)-quantile(x,probs=0.25)) * length(x)^(-1/3)
	return( ceiling( (max(x)-min(x)) /h ) )
 }

FD.pocet.tried(upravena.loteria)
hist(upravena.loteria,
     nclass=FD.pocet.tried(upravena.loteria) )
# alebo jednoducho:
hist(upravena.loteria,
     nclass="Freedman-Diaconis" )

#hustota do histogramu:
hist(loteria1, nclass=15)
hist(loteria1,
     nclass=15,
     col="springgreen",
     probability=TRUE )
osX <- seq( min(loteria1) , max(loteria1) , by=1)
lines(osX , dnorm(osX , mean=mean(loteria1) , sd=sqrt(var(loteria1))  ) )

#pozrite si prikaz "par(...)"