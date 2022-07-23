
canal <- function(){
  n1 <- c(0.64,0.36,0.05,0.82,0.83,0.43,0.58,0.31,0.19,0.63,0.44,0.99,0.46,
          0.76,0.69,0.47,0.13,0.26,0.77,0.53,0.96,0.11,0.62,0.67,0.28)
  n2 <- c(0.38,0.83,0.06,0.64,0.80,0.66,0.24,0.79,0.86,0.13,0.85,0.63,0.05,
          0.86,0.53,0.69,0.03,0.90,0.65,0.78,0.06,0.26,0.27,0.60,0.01)
  
  hora_llegada<-c()
  llega_acum <- c()
  tiem_servicio <- c()
  ini_servicio <- c()
  fin_servicio <- c()
  tiem_espera <- c()
  tiempo_total <-c()
  for (i in 1:length(n1) ) {
    
    hora_llegada[i] <- c(round(-log(n1[i])*6,2))
    
    if(i==1){
      llega_acum[i] <- hora_llegada[i]
    } else{
      llega_acum[i] <- hora_llegada[i]+llega_acum[i-1]
    }
    
    tiem_servicio[i]<- c(round((-log(n2[i])/15)*60,2))
    
    if(i==1){
      ini_servicio[i] <- hora_llegada[i]
      fin_servicio[i] <- tiem_servicio[i]+ini_servicio[i]
      tiem_espera[i] <- ini_servicio[i]-llega_acum[i]
    } else {
      ini_servicio[i] <- ifelse(llega_acum[i]>fin_servicio[i-1],
                                llega_acum[i],fin_servicio[i-1])    
      fin_servicio[i] <- ini_servicio[i]+tiem_servicio[i]
      tiem_espera[i] <- ini_servicio[i]-llega_acum[i]
    }
    tiempo_total[i] <- tiem_espera[i]+tiem_servicio[i]
  }
  
  data.frame(n1,hora_llegada,llega_acum,n2,tiem_servicio,ini_servicio,
             fin_servicio,tiem_espera,tiempo_total)
  
}

y<-canal()
y
y2<-c(mean(y$tiem_espera),mean(y$tiempo_total))
y2




canal <- function(n){
  
  t<-c(0.64,0.36,0.05,0.82,0.83,0.43,0.58,0.31,0.19,0.63,0.44,0.99,0.49,
       0.76,0.69,0.47,0.13,0.26,0.77,0.53,0.96,0.11,0.62,0.67,0.28)
  t2<-c(0.38,0.83,0.06,0.64,0.80,0.66,0.24,0.79,0.86,0.13,0.85,0.63,0.05,
        0.86,0.53,0.69,0.03,0.90,0.65,0.78,0.06,0.26,0.27,0.60,0.01)
  n1 <- c(t,sample(t,n,replace = T))
  n2 <- c(t,sample(t,n,replace = T))
  
  
  #n1<- runif(n)
  #n2<- runif(n)
  
  
  #n1<- c(rpois(n,10))
  #n2<-c(rpois(n,4))
  
  hora_llegada<-c()
  llega_acum <- c()
  tiem_servicio <- c()
  ini_servicio <- c()
  fin_servicio <- c()
  tiem_espera <- c()
  tiempo_total <-c()
  for (i in 1:length(n1) ) {
    
    hora_llegada[i] <- c(round(-log(n1[i])*6,2))
    
    if(i==1){
      llega_acum[i] <- hora_llegada[i]
    } else{
      llega_acum[i] <- hora_llegada[i]+llega_acum[i-1]
    }
    
    tiem_servicio[i]<- c(round((-log(n2[i])/15)*60,2))
    
    if(i==1){
      ini_servicio[i] <- hora_llegada[i]
      fin_servicio[i] <- tiem_servicio[i]+ini_servicio[i]
      tiem_espera[i] <- ini_servicio[i]-llega_acum[i]
    } else {
      ini_servicio[i] <- ifelse(llega_acum[i]>fin_servicio[i-1],
                                llega_acum[i],fin_servicio[i-1])    
      fin_servicio[i] <- ini_servicio[i]+tiem_servicio[i]
      tiem_espera[i] <- ini_servicio[i]-llega_acum[i]
    }
    tiempo_total[i] <- tiem_espera[i]+tiem_servicio[i]
  }
  
  data.frame(n1,hora_llegada,llega_acum,n2,tiem_servicio,ini_servicio,
             fin_servicio,tiem_espera,tiempo_total)
}

v<-canal(100000)
v2<-c(mean(v$tiem_espera),mean(v$tiempo_total))
v2




