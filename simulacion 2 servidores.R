
dos_servidores<-function(){
  t1 <- c(6.7,10.3,4.8,8.0,11.5,8.7,5.5,4.7,10.3,8.5,8.6,8.0,6.7,
          10.5,9.5,7.6,6.8,7.3,5.9,7.6,4.8,5.6,5.7,7.1,7.2)
  t2 <- c(15.4,12.5,14.8,11.6,10.6,17.3,14.2,11.4,14.1,14.6,10.2,
          14.6,13.3,13.9,16.6,11.3,15.6,17.3,15.2,10.2,11.5,13.2,13.6,7.9,13.5)
  
  hora_de_llegada <- c()
  tiempo_de_servicio <- c()
  hora_acumulada <- c()
  atiende <- c()
  inicio_del_servicio <- c()
  fin_del_servicio <- c()
  tiempo_espera <-c()
  tiempo_total <- c()
  
  
  
  
  
  for (i in 1:length(t1)) {
    
    hora_de_llegada[i] <- t1[i]
    tiempo_de_servicio[i] <- t2[i]

# condiciones para el primer ensayo
    
    if(i==1){
      hora_acumulada[i] <- hora_de_llegada[i]
      atiende[i] <- sample(c("A","B"),1)
      inicio_del_servicio[i] <- hora_acumulada[i]
      fin_del_servicio[i] <- inicio_del_servicio[i] + tiempo_de_servicio[i]
      tiempo_espera[i] <- inicio_del_servicio[i] - hora_acumulada[i]
      tiempo_total[i] <- fin_del_servicio[i] - hora_acumulada[i] 
    }
# condiciones para el segundo ensayo    
    if(i==2){
      hora_acumulada[i] <- hora_acumulada[i-1] + hora_de_llegada[i]
      
# si el priemro en iatender fue A entonces atenderá B, o si A termino, 
# atenderá uno de los dos. Lo mismo si inicio B. 
      
      if(atiende[i-1]=="A"){
        atiende[i] <- ifelse(hora_acumulada[i]<fin_del_servicio[i-1],"B",
                             sample(c("A","B"),1))
      }
      if(atiende[i-1]=="B"){
        atiende[i] <- ifelse(hora_acumulada[i]<fin_del_servicio[i-1],"A",sample(c("A","B"),1))
      }
      
      inicio_del_servicio[i] <- hora_acumulada[i]
      fin_del_servicio[i] <- inicio_del_servicio[i] + tiempo_de_servicio[i]
      tiempo_espera[i] <- inicio_del_servicio[i] - hora_acumulada[i]
      tiempo_total[i] <- fin_del_servicio[i] - hora_acumulada[i] 
      
    } 
    
    if(i>2){
      hora_acumulada[i] <- hora_acumulada[i-1] + hora_de_llegada[i]
      
      
# Quien atiende al n-esimo cliente
      
      atiende[i] <- ifelse(fin_del_servicio[i-2]==min(fin_del_servicio[i-2],
                                                      fin_del_servicio[i-1]),atiende[i-2],
                           ifelse(fin_del_servicio[i-1]==min(fin_del_servicio[i-2],
                                                             fin_del_servicio[i-1]),atiende[i-1],
                                  sample(c("A","B"),1)))
      if(atiende[i-1]==atiende[i-2]){
        atiende[i] <- ifelse(fin_del_servicio[i-3] == min(fin_del_servicio[i-3],
                                          fin_del_servicio[i-1]),atiende[i-3],
               ifelse(fin_del_servicio[i-1]==min(fin_del_servicio[i-3],
                                                 fin_del_servicio[i-1]),atiende[i-1],
                      sample(c("A","B"),1)))
      }
      
      inicio_del_servicio[i] <- ifelse(hora_acumulada[i]< min(fin_del_servicio[i-2],fin_del_servicio[i-1]),
                                        min(fin_del_servicio[i-2],fin_del_servicio[i-1]),
                                        hora_acumulada[i])
      
      fin_del_servicio[i] <- inicio_del_servicio[i] + tiempo_de_servicio[i]
      
      tiempo_espera[i] <- inicio_del_servicio[i] - hora_acumulada[i]
      
      tiempo_total[i] <- fin_del_servicio[i] - hora_acumulada[i] 
      
      
    } 
    
  }
  
  data.frame(hora_de_llegada,hora_acumulada,atiende,tiempo_de_servicio,
             inicio_del_servicio,fin_del_servicio,tiempo_espera,tiempo_total)
  
  
  
}
tt<- dos_servidores()


mean(tt$tiempo_de_servicio)
mean(tt$tiempo_espera)
mean(tt$tiempo_total)







####  General para n clientes ####


dos_servidores<-function(n){
  # t1 <- c(6.7,10.3,4.8,8.0,11.5,8.7,5.5,4.7,10.3,8.5,8.6,8.0,6.7,
  #         10.5,9.5,7.6,6.8,7.3,5.9,7.6,4.8,5.6,5.7,7.1,7.2)
  # t2 <- c(15.4,12.5,14.8,11.6,10.6,17.3,14.2,11.4,14.1,14.6,10.2,
  #         14.6,13.3,13.9,16.6,11.3,15.6,17.3,15.2,10.2,11.5,13.2,13.6,7.9,13.5)
  
  t1 <- rgamma(n,shape = 16,scale = 0.5)
  t2 <-rweibull(n,shape = 8,scale = 15)
  
  hora_de_llegada <- c()
  tiempo_de_servicio <- c()
  hora_acumulada <- c()
  atiende <- c()
  inicio_del_servicio <- c()
  fin_del_servicio <- c()
  tiempo_espera <-c()
  tiempo_total <- c()
  servi <- c()
  
  for (i in 1:length(t1)) {
    
    hora_de_llegada[i] <- t1[i]
    tiempo_de_servicio[i] <- t2[i]
    
    
    # condiciones para el primer ensayo
    
    if(i==1){
      hora_acumulada[i] <- hora_de_llegada[i]
      atiende[i] <- sample(c("A","B"),1)
      inicio_del_servicio[i] <- hora_acumulada[i]
      fin_del_servicio[i] <- inicio_del_servicio[i] + tiempo_de_servicio[i]
      tiempo_espera[i] <- inicio_del_servicio[i] - hora_acumulada[i]
      tiempo_total[i] <- fin_del_servicio[i] - hora_acumulada[i] 
    }
    
    # condiciones para el segundo ensayo    
    if(i==2){
      hora_acumulada[i] <- hora_acumulada[i-1] + hora_de_llegada[i]
      
      # si el priemro en atender fue A entonces atenderá B, o si A termino, 
      # atenderá uno de los dos. Lo mismo si inicio B. 
      
      if(atiende[i-1]=="A"){
        atiende[i] <- ifelse(hora_acumulada[i]<fin_del_servicio[i-1],"B",
                             sample(c("A","B"),1))
      }
      if(atiende[i-1]=="B"){
        atiende[i] <- ifelse(hora_acumulada[i]<fin_del_servicio[i-1],"A",
                             sample(c("A","B"),1))
      }
      
      inicio_del_servicio[i] <- hora_acumulada[i]
      fin_del_servicio[i] <- inicio_del_servicio[i] + tiempo_de_servicio[i]
      tiempo_espera[i] <- inicio_del_servicio[i] - hora_acumulada[i]
      tiempo_total[i] <- fin_del_servicio[i] - hora_acumulada[i] 
      
    } 
    
    if(i>2){
      
      # se actualiza los tiempos de finalizacion para A,B
      
      servi <- split(fin_del_servicio,atiende)
      A <- max(servi$A)
      B <- max(servi$B)
      
      hora_acumulada[i] <- hora_acumulada[i-1] + hora_de_llegada[i]
      
      
      # Quien atiende al n-esimo cliente
      
      atiende[i] <- ifelse(A<B,"A",ifelse(B<A,"B",sample(c("A","B"),1)))
      
      inicio_del_servicio[i] <- max(hora_acumulada[i],min(A,B))
      
      fin_del_servicio[i] <- inicio_del_servicio[i] + tiempo_de_servicio[i]
      
      tiempo_espera[i] <- inicio_del_servicio[i] - hora_acumulada[i]
      
      tiempo_total[i] <- fin_del_servicio[i] - hora_acumulada[i] 
      
      
    } 
    
  }
  
  data.frame(hora_de_llegada,hora_acumulada,atiende,tiempo_de_servicio,
             inicio_del_servicio,fin_del_servicio,tiempo_espera,tiempo_total)
  
}


tt<- dos_servidores()
tt





w <- split(x=c(1,5,12,13,17,19),f=c("a","b","b","a","a","b"))
a<- max(w$a)
b<- max(w$b)

which()

min(a,b)

