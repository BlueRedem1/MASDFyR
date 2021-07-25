library(nortest)
Data<-read.csv('Datos_p18.csv')
obs<-unlist(Data$Retencion.100.)
n=length(obs)
mu_MLE_ln=sum(log(obs))/n
sd_MLE_ln=sqrt(sum((log(obs)-mu_MLE_ln)^2)/n)
ks.test(obs,plnorm,mu_MLE_ln,sd_MLE_ln) #Rechazamos
mu_MLE_norm=mean(obs)
sd_MLE_norm=sqrt(sum((obs-mu_MLE_norm)^2)/n)
sd_insesgado=sd(obs)
ks.test(obs,pnorm,mu_MLE_norm,sd_MLE_norm)
ad.test(obs)
ad.test(log(obs))
lillie.test(obs)
min_datos=min(obs)
max_datos=max(obs)
#La beta se descarta pues solo toma valores entre 0 y 1 
par(mfrow=c(1,1)) 
par(cex.axis = 0.6, cex.lab = 1, cex.main = 1, cex.sub = 1, mgp = c(1.5,0.5, 0))
hist(obs, breaks=200,freq=FALSE,col = "deepskyblue3",border = "deepskyblue4",
     lwd=2, xlab ="obs",ylab ="Densidad", main = 'Histograma vs Normal')
y<-curve(dnorm(x,mean=mu_MLE_norm,sd=sd_MLE_norm), from=min_datos, to=max_datos, add=TRUE, 
         col="black")
legend("topright",legend=c("Histograma","Distribución Normal"),lty=1, 
       col=c('deepskyblue3', 'black'), bty='n')
par(mfrow=c(1,1)) 
par(cex.axis = 0.6, cex.lab = 1, cex.main = 1, cex.sub = 1, mgp = c(1.5,0.5, 0))
hist(obs, breaks=200,freq=FALSE,col = "deepskyblue3",border = "deepskyblue4",
     lwd=2, xlab ="obs",ylab ="Densidad", main = 'Histograma vs Log-Normal')
z<-curve(dlnorm(x,mean=mu_MLE_ln,sd=sd_MLE_ln), from=min_datos, to=max_datos, add=TRUE, 
         col="red")
legend("topright",legend=c("Histograma","Distribución Log-Normal"),lty=1, 
       col=c('deepskyblue3', 'red'), bty='n')
#La lognormal presenta un mayor p value en ambas pruebas (A-D y K-S) así que le ajustamos la log-normal
