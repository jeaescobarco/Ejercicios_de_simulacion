
ven_periodicos <- function(){
  N <- c(0.02,0.32,0.50,0.77,0.33,0.05,0.70,0.28,0.42,0.34,0.83,0.72,0.77,0.68,
         0.91,0.91,0.19,0.87,0.18,0.75)
  
  q<-40
  v_venta <- 1700
  V_compra <- 1200
  porcentaje_devuelto <- 0.4
  demanda <-c()
  vendidos <- c()
  sobrantes <- c()
  utilidades <- c()
  
  for (k in 1:length(N)) {
    demanda[k] <- ifelse(N[k]<0.12,
                         25,ifelse(N[k]<0.2,
                                   30,ifelse(N[k]<0.5,
                                             35,ifelse(N[k]<0.75,40,
                                                       ifelse(N[k]<0.85,
                                                              45,ifelse(N[k]<0.95,
                                                                        50,55))))))
    
    

    vendidos[k] <- ifelse(demanda[k]<=q,demanda[k],q)
    sobrantes[k] <- ifelse(demanda[k]<=q,q-vendidos[k],0)
    utilidades[k] <- (v_venta*vendidos[k])+
      (porcentaje_devuelto*V_compra*sobrantes[k])-(V_compra*q)
     
  }
  
  data.frame(demanda,vendidos,sobrantes,utilidades)
}




f<-ven_periodicos()

ff <- c(mean(f$sobrantes),mean(f$utilidades))
ff



# general para n dias, q pedido, x valor de venta, y valor de compra y 
# z porcentaje de devolucion.


ven_periodicos_G <- function(n,Q,V_venta=1700,V_compra=1200,porcent_de_devolucion=0.4){
  #N <- c(0.02,0.32,0.50,0.77,0.33,0.05,0.70,0.28,0.42,0.34,0.83,0.72,0.77,0.68,
   #      0.91,0.91,0.19,0.87,0.18,0.75)
  N<- runif(n)
  q<-Q
  v_venta <- V_venta
  V_compra <- V_compra
  porcentaje_devuelto <- porcent_de_devolucion
  demanda <-c()
  vendidos <- c()
  sobrantes <- c()
  utilidades <- c()
  
  for (k in 1:length(N)) {
    demanda[k] <- ifelse(N[k]<0.12,
                         25,ifelse(N[k]<0.2,
                                   30,ifelse(N[k]<0.5,
                                             35,ifelse(N[k]<0.75,40,
                                                       ifelse(N[k]<0.85,
                                                              45,ifelse(N[k]<0.95,
                                                                        50,55))))))
    
    
    
    vendidos[k] <- ifelse(demanda[k]<=q,demanda[k],q)
    sobrantes[k] <- ifelse(demanda[k]<=q,q-vendidos[k],0)
    utilidades[k] <- (v_venta*vendidos[k])+
      (porcentaje_devuelto*V_compra*sobrantes[k])-(V_compra*q)
    
  }
  
  data.frame(demanda,vendidos,sobrantes,utilidades)
}


barplot(prop.table(table(ven_periodicos_G(1000,37)$utilidades)),col="blue")

z<-ven_periodicos_G(5,40)
m<-data.frame(replicate(2,expr=z))

barplot(prop.table(table(m$utilidades)))

m$X1$utilidades

