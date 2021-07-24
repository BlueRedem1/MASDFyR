###MODELO DE RECLAMACIONES DE UNA C?A. DE SEGUROS###

#El siguiente c?digo permite simular al proceso de Poisson compuesto cuando la
#distribuci?n de salto es la de 100,000 veces una Beta de par?metros 1/2 y 1/2. 
#La idea es modelar una situaci?n en la que es m?s probable que los montos de 
#los siniestros sean o muy peque?os o muy grandes (hasta el tope de 1,000,000 que
#podr? ser la suma asegurada).
set.seed(2009)
SNt_PPC<-function(lambda=15,tiempo=1,SA=1000000){
  #lambda <-15 # Intensidad
  #tiempo<-1
  t <-rexp (1, lambda ) # t representa el tiempo del primer siniestro
  x <-SA* rweibull (1 ,shape=1/5,scale=1/5) # x representa el monto del primer siniestro
  T <-c(0,t) # El vector T ira acumulando los tiempos en que van ocurriendo siniestros
  X <-c(0,x) # X ira acumulando los montos de los siniestros
  N <-0 # N nos dira cuantos siniestros ocurren hasta el tiempo 1
  while (t <tiempo) { # Mientras no se haya sobrepasado el tiempo del proceso
    N <-N+1 # Aumentamos el numero de siniestros
    t <-t+ rexp (1, lambda ) # Vemos el tiempo en el que ocurre el siguiente siniestro
    x <- SA * rbeta (1 ,1/2,1/2) # Calculamos su monto
    T <-c(T,t) # Aumentamos un evento temporal
    X <-c(X, tail (X ,1)+x) # Agregamos el monto de la reclamacion
  }
  return(N) #Regresamos el número de siniestos
}
nb_sims=c(10,100,10000)
siniestros_esp=c()
i=1
for (nb in nb_sims){
  counter=0
  siniestros=0
  while(counter<nb){
    siniestros=siniestros+SNt_PPC()
    counter=counter+1
  }
  siniestros_esp[i]=siniestros/nb
  i=i+1
}
Ej5<-data.frame(nb_sims,siniestros_esp)