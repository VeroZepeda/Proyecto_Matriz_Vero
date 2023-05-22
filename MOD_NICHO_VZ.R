#Verónica Zepeda
#verozepeda@ciencias.unam.mx


#Función para obtener las redes de interacciones con base en el modelo de nicho de Williams y Martínez, 2000
#Este código es una traducción y adaptación del código reportado en CITA ARTICULO LETY

# Argumentos de la función
# S     : numero de especies. Valor = 10
# C     : conectividad.  Valor = 0.2

# Salidas de la función
#   A       : matriz de interacciones tróficas.

mod_nicho <- function(S,C){
  alpha = 1
  beta = (alpha/(2*C))-alpha
  A = matrix(data=0, nrow=S, ncol=S)
  x = rbeta(1,alpha,beta) #valor de distribución beta B(alpha,beta) con alpha=1 y beta=alpha/2C - alpha 
  nicho=runif(S) #vector que corresponde al valor de nicho de cada especie
  
  #intervalos ri's de las especies
  interv = rep(0, S)
for(i in 1:S){
  interv[i]= x * nicho[i]
}
#centros ci : numero aleatorio entre ri/2 y min(ni, 1-ri/2)
  cent = rep(0,S)
 for(i in 1:S){
  ai = interv[i]/2
  bi = min(nicho[i], 1-interv[i]/2)
  cent[i] = ai + (bi-ai)*runif(1)
}
  #haciendo la matriz A
  for( i in 1:S){
    for(j in 1:S){
      if((cent[i] - interv[i]/2 <= nicho[j]) && (nicho[j] <= cent[i] + interv[i]/2)) {A[j,i]=1}
    }
  }

  # Para eliminar canibalismo 
  for(i in 1:S){
    A[i,i] = 0
  }
  A
}


#Para obtener una matriz de interacciones tróficas. 
matriz_interacciones = mod_nicho(10,0.2)


#Para obtener multiples matrices y guardarlas en un array.

redes <- function(S, C, n_redes){
  array_redes = array(NA,dim = c(S, S, n_redes))
  for(i in 1:n_redes){
    array_redes[,,i] = mod_nicho(S,C)
  }
  array_redes
}

mul_redes=redes(10, 0.2, 150)

#Para guardar el array con las redes como Rdata
save(mul_redes,file="/Users/veronicazepeda/Documents/PosdoctLANCIS/Datos/array_redes.Rdata")

##Cargar bibliotecas para gráficar la red
library(GGally)
library(network)
library(sna)
library(ggplot2)

net = network(matriz_interacciones, directed = TRUE)
ggnet2(net)
ggnet2(matriz_interacciones)

#ggnet2 can also size nodes by calculating their in-degree, out-degree, or total (Freeman) degree, using the degree function of the sna package.
ggnet2(matriz_interacciones, size = "degree")

# remove any isolated nodes
x = ggnet2(matriz_interacciones, size = "degree", size.min = 1)


