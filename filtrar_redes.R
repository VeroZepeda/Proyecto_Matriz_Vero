#Verónica Zepeda
#verozepeda@ciencias.unam.mx


#Función para enlistar cuantas especies de cada gremio hay en cada red de interacciones tróficas
filtro <- function(array_A, n_redes){
  n_tipo = matrix(NA, ncol=4,nrow = n_redes)
  colnames(n_tipo)=c("top","inter","basal","ais")
  for(i in 1:n_redes){
    tipos <- tipoTIBA(array_A[,,i])
    n_tipo[i,1] = tipos$numtipo[1]
    n_tipo[i,2] = tipos$numtipo[2]
    n_tipo[i,3] = tipos$numtipo[3]
    n_tipo[i,4] = tipos$numtipo[4]
  }
  n_tipo
}

#Para obtener la base
aa=filtro(array_redes,n_redes = dim(array_redes)[3])

sp_ais=which(aa[,4] > 0) #redes con especies aisladas

new_array = array_redes[,,-sp_ais] #removemos las redes con especies aisladas

bb=filtro(new_array,n_redes = dim(new_array)[3]) #Volvemos a hacer la base 

which(bb[,4] > 0) #para revisar que ya no hay especies aisladas
which(bb[,3] >= 1) #redes con al menos una especie basal
no_bas=which(bb[,3] == 0)

redes_filtradas = new_array[,,-no_bas] #array de redes sin especies aisladas y con al menos una especie basal
pru=filtro(redes_filtradas,n_redes = dim(redes_filtradas)[3])
which(pru[,4] > 0)
which(pru[,3] == 0)

#Guarda el array de redes tróficas en un objeto Rdata
save(redes_filtradas,file="/Users/veronicazepeda/Documents/PosdoctLANCIS/Datos/array_redes_filtradasBUENO.Rdata")
