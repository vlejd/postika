# KORELACNA ANALYZA:

staty <- read.table(file="e:\\americke_staty.txt" , header=TRUE)
attach(staty)

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

# Fisherova Z-premenna:
cor(Income,HS.Grad)
Z <- atanh( cor(Income,HS.Grad) )
# IS pre rho:
#dolna hranica:
tanh( Z - qnorm(0.975) / sqrt(length(Income)-3) )
#horna hranica:
tanh( Z + qnorm(0.975) / sqrt(length(Income)-3) )


###########################
# povedat N O R M A L I T U!!!!!
###########################

#testovanie rovnosti 2 korelacnych koeficientov:
mean(Frost)
cor( Income[Frost<104] , HS.Grad[Frost<104] )
cor( Income[Frost >= 104] , HS.Grad[Frost >= 104] )
Z1 <- atanh( cor( Income[Frost<104] , HS.Grad[Frost<104] ) )
Z2 <- atanh( cor( Income[Frost >= 104] , HS.Grad[Frost >= 104] ) )
n1 <- length( Frost[Frost<104] )
n2 <- length( Frost[Frost>=104] )
#testovacia statistika:
Z12 <- (Z1-Z2) / sqrt( 1/(n1-3) + 1/(n2-3) )
#p-hodnota:
2*( 1-pnorm(Z12) )



# LINEARNA REGRESIA:

newyorsky.vzduch <- read.table(file="../data/newyorsky_vzduch.txt" , header=TRUE )
attach(newyorsky.vzduch)

plot(temperature , ozone)
vzduchMODEL <- lm(formula = ozone~1+temperature , x=TRUE)
vzduchMODEL
vzduchMODEL$x
vzduchMODEL$coeff
abline(vzduchMODEL$coeff)


# IS pre kontrast:

ISpreKontrast <- function(a,betaHAT,Y,X)
{
	#n je pocet merani:
	n <- length(Y)
	
	#k je pocet parametrov:
	k <- length(betaHAT)
	
	#SSe je Sucet Stvorcov rezidui:
	SSe <- sum( (Y - X%*%betaHAT)^2 )
	S <- sqrt( SSe / (n-k) )

	DolnaHranica <- t(a)%*%betaHAT - qt(0.975,df=n-k)*S*sqrt( t(a)%*%solve(t(X)%*%X)%*%a )
	HornaHranica <- t(a)%*%betaHAT + qt(0.975,df=n-k)*S*sqrt( t(a)%*%solve(t(X)%*%X)%*%a )
	return( c(DolnaHranica , t(a)%*%betaHAT , HornaHranica) )
}

ISpreKontrast( a=c(0,1) , betaHAT=vzduchMODEL$coeff , Y=ozone , X=vzduchMODEL$x)
ISpreKontrast( a=c(1,0) , betaHAT=vzduchMODEL$coeff , Y=ozone , X=vzduchMODEL$x)
ISpreKontrast( a=c(1,85) , betaHAT=vzduchMODEL$coeff , Y=ozone , X=vzduchMODEL$x)