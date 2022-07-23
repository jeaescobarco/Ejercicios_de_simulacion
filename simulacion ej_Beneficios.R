

beneficios<-function(ensayos){
  costo <- c()
  rendimiento <- c()
  precio <- c()
  Beneficio <- c()
  for (i in 1:ensayos) {
    costo[i] <- sample(c(400,500,600),1,prob = c(0.7,0.2,0.1),replace = T)
    rendimiento[i] <- sample(c(210,220,230,240,250),1,
                             prob = c(0.1,0.1,0.4,0.3,0.1))
    precio[i] <- sample(c(2:7),1,prob = c(0.1,0.2,0.5,0.1,0.05,0.05),replace = T)
    Beneficio[i] <- (rendimiento[i]*precio[i])-costo[i]
  }
  data.frame(costo,rendimiento,precio,Beneficio)
}

b<-beneficios(25)
prop.table(table(b$Beneficio<100))



