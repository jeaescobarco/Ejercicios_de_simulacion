
carga<-function(){
    tina<- rep(c(1:5))
    alea <- round(runif(5),4)
    peso_tina <- ifelse(alea<0.5,190+(800*alea)^0.5,230-(800*(1-alea))^0.5)
    peso_acomulado<-c()
    se_exedio <- c()
    for (i in 1:5) {
      if(i==1){
        peso_acomulado[i] <- peso_tina[i]
      }else{
        peso_acomulado[i] <- sum(peso_tina[i]+peso_acomulado[i-1])
      }
      
      se_exedio[i] <- ifelse(peso_acomulado[i]>1000,"si","no")
       
    }
    data.frame(tina,alea,peso_tina,peso_acomulado,se_exedio)
}

carga()



