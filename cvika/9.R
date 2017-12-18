newyorsky.vzduch <- read.table(file="../data/newyorsky_vzduch.txt" , header=TRUE )
attach(newyorsky.vzduch)

plot(temperature , ozone)
vzduchMODEL <- lm(formula = ozone~1+temperature , x=TRUE)
abline(vzduchMODEL$coeff)

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


#_______________________________________
#pas spolahlivosti pre regresnu priamku:

Scheffe1 <- function(a,betaHAT,Y,X)
{
	#n je pocet merani:
	n <- length(Y)
	
	#k je pocet parametrov:
	k <- length(betaHAT)
	
	#SSe je Sucet Stvorcov rezidui:
	SSe <- sum( (Y - X%*%betaHAT)^2 )
	S <- sqrt( SSe / (n-k) )

	DolnaHranica <- t(a)%*%betaHAT - sqrt(k*qf(0.95,df1=k,df2=n-k))*S*sqrt( t(a)%*%solve(t(X)%*%X)%*%a )
	HornaHranica <- t(a)%*%betaHAT + sqrt(k*qf(0.95,df1=k,df2=n-k))*S*sqrt( t(a)%*%solve(t(X)%*%X)%*%a )
	return( c(DolnaHranica , HornaHranica) )
}

osX <- seq(min(temperature) , max(temperature) , by=1)
okraje.pasu <- matrix( nrow=2 , ncol=length(osX) )
for (i in 1:length(osX))
   okraje.pasu[,i] <- Scheffe1( a=c( 1,osX[i] ) , betaHAT=vzduchMODEL$coeff , Y=ozone , X=vzduchMODEL$x )

#dolna hranica pasu:
lines(osX , okraje.pasu[1,] ,
      col="red")
#horna hranica pasu:
lines(osX , okraje.pasu[2,] ,
      col="red")

##########
#povedat ROZSIROVANIE
##########


#____________________
#predikcny interval:

PredikcnyInterval <- function(a,betaHAT,Y,X)
{
	#n je pocet merani:
	n <- length(Y)
	
	#k je pocet parametrov:
	k <- length(betaHAT)
	
	#SSe je Sucet Stvorcov rezidui:
	SSe <- sum( (Y - X%*%betaHAT)^2 )
	S <- sqrt( SSe / (n-k) )

	DolnaHranica <- t(a)%*%betaHAT - qt(0.975,df=n-k)*S*sqrt(1+t(a)%*%solve(t(X)%*%X)%*%a )
	HornaHranica <- t(a)%*%betaHAT + qt(0.975,df=n-k)*S*sqrt(1+t(a)%*%solve(t(X)%*%X)%*%a )
	return( c(DolnaHranica , t(a)%*%betaHAT , HornaHranica) )
}


#porovnanie sirky:

PredikcnyInterval( a=c(1,85) , betaHAT=vzduchMODEL$coeff , Y=ozone , X=vzduchMODEL$x)
ISpreKontrast( a=c(1,85) , betaHAT=vzduchMODEL$coeff , Y=ozone , X=vzduchMODEL$x)


#_______________
#predikcny pas:                

Scheffe2 <- function(a,betaHAT,Y,X)
{
	#n je pocet merani:
	n <- length(Y)
	
	#k je pocet parametrov:
	k <- length(betaHAT)
	
	#SSe je Sucet Stvorcov rezidui:
	SSe <- sum( (Y - X%*%betaHAT)^2 )
	S <- sqrt( SSe / (n-k) )

	DolnaHranica <- t(a)%*%betaHAT - sqrt(k*qf(0.95,df1=k,df2=n-k))*S*sqrt(1+t(a)%*%solve(t(X)%*%X)%*%a )
	HornaHranica <- t(a)%*%betaHAT + sqrt(k*qf(0.95,df1=k,df2=n-k))*S*sqrt(1+t(a)%*%solve(t(X)%*%X)%*%a )
	return( c(DolnaHranica , HornaHranica) )
}

osX <- seq(min(temperature) , max(temperature) , by=1)
okraje.pasu <- matrix( nrow=2 , ncol=length(osX) )
for (i in 1:length(osX))
   okraje.pasu[,i] <- Scheffe2( a=c( 1,osX[i] ) , betaHAT=vzduchMODEL$coeff , Y=ozone , X=vzduchMODEL$x )
#dolna hranica pasu:
lines(osX , okraje.pasu[1,] ,
      col="green")
#horna hranica pasu:
lines(osX , okraje.pasu[2,] ,
      col="green")


# predpoved ozonu, ak teplota ma byt medzi 70 a 80:
Scheffe2( a=c( 1,70 ) , betaHAT=vzduchMODEL$coeff , Y=ozone , X=vzduchMODEL$x )
Scheffe2( a=c( 1,80 ) , betaHAT=vzduchMODEL$coeff , Y=ozone , X=vzduchMODEL$x )


#POLYNOMICKA REGRESIA:

anaerob <- read.table("../data/anaerob.txt" , header=TRUE)
attach(anaerob)

plot(Oxygen,Ventilation)
anaerobMODEL <- lm(formula=Ventilation~1+Oxygen , x=TRUE)
abline(anaerobMODEL$coeff)

anaerobMODEL2 <- lm( formula=Ventilation~1+Oxygen+I(Oxygen^2) , x=TRUE)
osX <- seq(min(Oxygen) , max(Oxygen) , by=100)
lines(osX , anaerobMODEL2$coeff[1] + anaerobMODEL2$coeff[2]*osX + anaerobMODEL2$coeff[3]*osX^2)

#___________________________________________
#pas spolahlivosti regresnu krivku ANAEROB:

osX <- seq(min(Oxygen) , max(Oxygen) , by=100)
okraje.pasu <- matrix( nrow=2 , ncol=length(osX) )
for (i in 1:length(osX))
   okraje.pasu[,i] <- Scheffe1( a=c( 1,osX[i],(osX[i])^2 ) , betaHAT=anaerobMODEL2$coeff , Y=Ventilation , X=anaerobMODEL2$x )
#dolna hranica pasu:
lines(osX , okraje.pasu[1,] ,
      col="red")
#horna hranica pasu:
lines(osX , okraje.pasu[2,] ,
      col="red")


#_______________________
#predikcny pas ANAEROB:

osX <- seq(min(Oxygen) , max(Oxygen) , by=100)
okraje.pasu <- matrix( nrow=2 , ncol=length(osX) )
for (i in 1:length(osX))
   okraje.pasu[,i] <- Scheffe2( a=c( 1,osX[i],(osX[i])^2 ) , betaHAT=anaerobMODEL2$coeff , Y=Ventilation , X=anaerobMODEL2$x )
#dolna hranica pasu:
lines(osX , okraje.pasu[1,] ,
      col="green")
#horna hranica pasu:
lines(osX , okraje.pasu[2,] ,
      col="green")


summary(anaerobMODEL2)

