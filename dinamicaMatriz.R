# Función que describe la dinámica de la matriz. Acopla la dinámica de cada una de las comunidades con la dinámica de la migración. 
#Verónica Zepeda
#verozepeda@ciencias.unam.mx

#Import libraries
library(deSolve)

###Argumentos
#Paisaje
#Matriz de interacciones ----- Matriz A, así está en mi código anterior
#Tasas de reproducción ---- Son las ri´s en mi caso
#Condiciones iniciales ---- Vector X0
#t_total --- = 100, según lo reportado en el artículo. 
#Dispersión
#Mortalidad
#pasos_mm=5, mm signfica migración mortalidad, según lo reportado en el artículo deberían ser 5 pasos 



matriz_agroecologica <- function(paisaje, matriz_interacciones, tasas_reproduccion, condiciones_iniciales, t_total, Dispersion, Mortalidad, pasos_mm=5) {
 
  # Get dimensions
  x_celdas <- nrow(paisaje)
  y_celdas <- ncol(paisaje)
  n_especies <- length(matriz_interacciones) #abundancia de las especies
  
  # Initialize population array
  #Inicializa array poblacion de 3 dimensiones con forma (x, y, n_especies)
  poblacion <- array(0, dim = c((pasos_mm + 1) * t_total + 1, x_celdas, y_celdas, n_especies)) 
  
  # Set initial population in grassland cells
  poblacion[1, , , ] <- genera_poblacion_inicial(paisaje, n_especies, p0_pastizal = condiciones_iniciales)
  
  # Loop through time steps
  for (t in 2:(t_total + 1)) {
    T <- (pasos_mm + 1) * t - pasos_mm 
    
    # Copy previous time step's population
    poblacion[T, , , ] <- poblacion[T - 1, , , ]
    
    # Lotka-Volterra dynamics in grassland cells
    for (i in 1:x_celdas) {
      for (j in 1:y_celdas) {
        if (paisaje[i, j] == "p") {
          poblacion[T, i, j, ] <- ode(y = poblacion[T, i, j, ], times = c(0, 0.5), func = lotka, parms = list(r = tasas_reproduccion, a = matriz_interacciones))[[2]][2, ]
        }
      }
    }
    
    # Migration and death steps
    for (k in 1:pasos_mm) {
      # Migration
      poblacion[T + k, , , ] <- migracion(X = poblacion[T + k - 1, , , ], tp = paisaje, L = Dispersion)
      
      # Death
      for (i in 1:x_celdas) {
        for (j in 1:y_celdas) {
          tasa_muerte <- Mortalidad[paisaje[i, j]]
          poblacion[T + k, i, j, ] <- (1 - tasa_muerte) * poblacion[T + k, i, j, ]
        }
      }
    }
  }
  
  return(poblacion)
}



#Función que asigna cuanta población migra dependiendo del parche en el que estuvo
#X = es la matriz de dimension 3 x (x,y,i) donde la especie i-esima tiene su representación para todos los parches.
#t es la distribución de tipos de parche en todo el espacio. 
#L es un "diccionario" en dónde están guardados los valores de migración:
#L["p"] = valor pastizal
#L["r"] = valor riego
#L["t"] = valor temporal


migracion <- function(X, tp, L) {
  # Get dimensions
  s <- dim(X)
  t <- as.vector(tp)
  P <- array(0, dim = s)
  G <- array(0, dim = s)
  R <- array(0, dim = s)
  
  # Loop through species
  for (idx in 1:s[3]) {
    esp <- X[, , idx]
    dm <- dim(esp)
    xesp <- dm[1]
    yesp <- dm[2]
    
    loss_e <- array(0, dim = dm)
    gain_e <- array(0, dim = dm)
    
    # Calculate loss and gain
    for (x in 1:xesp) {
      for (y in 1:yesp) {
        loss_e[x, y] <- esp[x, y] * L[t[x, y]]
      }
    }
    for (x in -1:dm[1]) {
      for (y in -1:dm[2]) {
        gain_e[x, y] <- (loss_e[(x - 1) %% dm[1] + 1, (y - 1) %% dm[2] + 1] + loss_e[(x - 1) %% dm[1] + 1, y %% dm[2] + 1] + loss_e[(x - 1) %% dm[1] + 1, (y + 1) %% dm[2] + 1] + loss_e[x %% dm[1] + 1, (y - 1) %% dm[2] + 1] + loss_e[x %% dm[1] + 1, (y + 1) %% dm[2] + 1] + loss_e[(x + 1) %% dm[1] + 1, (y - 1) %% dm[2] + 1] + loss_e[(x + 1) %% dm[1] + 1, y %% dm[2] + 1] + loss_e[(x + 1) %% dm[1] + 1, (y + 1) %% dm[2] + 1]) / 8
      }
    }
    
    # Calculate new population
    P[, , idx] <- loss_e
    G[, , idx] <- gain_e
    R[, , idx] <- esp + (gain_e - loss_e)
  }
  
  return(R)
}


#Aquí iría la ecuación de Lety
lotka <- function(t, x, r, a) {
  # Lotka-Volterra equation
  dx <- x * (r + a %*% x)
  return(list(dx))
}



genera_poblacion_inicial <- function(paisaje, n_especies, p0_pastizal, p0_temporal = rep(0, n_especies), p0_riego = rep(0, n_especies)) {
  # Initialize population array
  poblacion_0 <- array(0, dim = c(nrow(paisaje), ncol(paisaje), n_especies))
  
  # Set initial population in grassland cells
  for (x in 1:nrow(paisaje)) {
    for (y in 1:ncol(paisaje)) {
      if (paisaje[x, y] == "p") {
        poblacion_0[x, y, ] <- p0_pastizal
      }
      if (paisaje[x, y] == "t") {
        poblacion_0[x, y, ] <- p0_temporal
      }
      if (paisaje[x, y] == "r") {
        poblacion_0[x, y, ] <- p0_riego
      }
    }
  }
  
  return(poblacion_0)
}


