

vagones <- function(dias, prueba, descarga_por_dia,n_ver) {
  
  descargas_diarias <- c()
  llegan <- c()
  diast <- dias + prueba
  para_descargar <- c()
  para_manana <- c()
  descargados <- c()
  
  for (i in 1:diast) {
    descargas_diarias[i] <- descarga_por_dia
    llegan[i] <- sample(c(0:5), 1, prob = c(0.23, 0.3, 0.3, 0.1, 0.05,
                                            0.02), replace = T)
    
    if (i == 1) {
      llegan[i] <- llegan[i]
      para_descargar[i] <- llegan[i]
      descargados[i] <- ifelse(para_descargar[i] < descargas_diarias[i],
                               para_descargar[i], descargas_diarias[i])
      para_manana[i] <- ifelse(para_descargar[i] >= descargas_diarias[i],
                               para_descargar[i] - descargas_diarias[i], 0)
    }
    
    if (i > 1) {
      
      llegan[i] <- llegan[i]
      para_descargar[i] <- llegan[i] + para_manana[i - 1]
      descargados[i] <- ifelse(para_descargar[i] < descargas_diarias[i],
                               para_descargar[i], descargas_diarias[i])
      para_manana[i] <- ifelse(para_descargar[i] >= descargas_diarias[i],
                               para_descargar[i] - descargas_diarias[i], 0)
    }
    
    
  }
  ini <- prueba + 1  # donde inicia a contar lo real 
  
  # Muestra las medias de interes
  print(c(media_llegada = mean(llegan[ini:diast]), 
          media_para_manana = mean(para_manana[ini:diast])))
  
  dias <- c(rep("x", prueba), 1:dias)
  
  head(data.frame(dias, llegan, para_descargar, descargados, para_manana),
       n_ver)
  
  
}

vagones(1000,2,2,12)


