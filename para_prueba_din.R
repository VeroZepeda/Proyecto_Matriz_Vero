
##Cargar array con las redes generadas con la función MOD_NICHO_VZ
#el nombre del objeto es redes_filtradas
load("/Users/veronicazepeda/Documents/PosdoctLANCIS/Datos/array_redes_filtradasBUENO.Rdata")

##Cargar la matriz que contiene los valores de las ri 
load("/Users/veronicazepeda/Documents/PosdoctLANCIS/Datos/ris_BUENO.Rdata")

##Cargar la matriz que contiene los valores de las X0
load("/Users/veronicazepeda/Documents/PosdoctLANCIS/Datos/X0_BUENO.Rdata")

#Cargar la función que clasifica a las especies según su tipo (top, intermedia, basales, aisladas)
source("/Users/veronicazepeda/Documents/PosdoctLANCIS/Código/tipoTIBA_VZ.R")
source('~/Documents/PosdoctLANCIS/Código/solEuler_peso_VZ.r', chdir = TRUE)

#Para correr sólo una de las matrices de interacciones tróficas del array
#Parámetros que cambian
redes_filtradas[,,1]
ris2[1,]
X0[1,]

Tf <- 100
num <- 1000
n_redes = dim(X0)[1]


#Parámetros fijos

w <- 1
Ti <- 0
K <- rep(1, S)
bound <- 0.0000001
S=10

objeto_din$evol_cont
riqueza = objeto_din$Sp
abun = objeto_din$abundancia



#solEuler_peso(X0,K,R2,A,w,Ti,Tf,num,bound)
din_com <- function(array_redes_filtradas,matriz_ri,matriz_X0, Tf, num,n_redes,S){
  #Parámetros fijos
  w <- 1
  Ti <- 0
  K <- rep(1, S)
  bound <- 0.0000001
  S <- 10
  
  #para guardar las salidas
  riqueza <- rep(0,n_redes) #Vector para resultados riqueza
  abundancia <- matrix(nrow=n_redes,ncol=S) #Matriz para resultados abundancia
  array_dinamica <- array(NA, dim = c(num+1,S,n_redes)) #Array para resultados dinámica poblacional
 
  #generar las dinámicas poblacionales de cada comunidad en ausencia de paisaje 
  for(i in 1:n_redes){
    sol <- solEuler_peso(matriz_X0[i,],K,matriz_ri[i,],array_redes_filtradas[,,i],w,Ti,Tf,num,bound)
    riqueza[i] = sol$Sp
    abundancia[i,] = sol$abundancia
    array_dinamica[,,i] =  sol$evol_cont
  }
  
  list("riqueza"= riqueza,"abundancia"= abundancia,"array_dinamica"= array_dinamica)
  
}

#Corre la dinámica poblacional para todas las redes tróficas obtenidas
dinamica_redes <- din_com(redes_filtradas,ris2,X0,100,1000,n_redes = dim(X0)[1],S=10)

which(dinamica_redes$riqueza == 0)  
dinamica_redes$riqueza
  
riq = dinamica_redes$riqueza
abun = dinamica_redes$abundancia
dina = dinamica_redes$array_dinamica

save(riq,file="/Users/veronicazepeda/Documents/PosdoctLANCIS/Datos/riquezaTODO.Rdata")
save(abun,file="/Users/veronicazepeda/Documents/PosdoctLANCIS/Datos/abunTODO.Rdata")
save(dina,file="/Users/veronicazepeda/Documents/PosdoctLANCIS/Datos/dinaTODO.Rdata")
  
  
  
  
  
  
