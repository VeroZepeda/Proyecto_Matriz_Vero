##Prueba para verificar el código usando los valores de los parámetros de Lety
# se definen los parámetors de la función
aa = c(0,0,0,1,0,0,0,0,0,0,
       0,0,1,0,0,0,0,1,0,0,
       0,0,0,0,0,0,0,0,0,0,
       0,1,0,0,0,0,0,0,0,0,
       0,1,1,1,0,0,0,1,0,1,
       0,1,1,0,0,0,0,1,0,1,
       1,1,0,1,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,
       1,0,0,0,1,1,1,0,0,0,
       0,0,0,0,0,0,0,0,0,0)

A <- matrix(aa, nrow=10, ncol=10,byrow=FALSE)
X0 <- c(0.5342,0.8083,0.9959,0.8765,0.0359,0.5016,0.8760,0.6893,0.43210,0.8755)
R2 <- c(-0.0066,-0.0413,0.5481,-0.0116,-0.0174,-0.0483,-0.0945,0.3690,-0.0215,0.2083)
S <- nrow(A)
w <- 1
Ti <- 0
Tf <- 100
num <- 1000
K <- rep(1, S)
bound <- 0.0000001

#Para calcular la dinámica
#solEuler_peso(X0,K,R2,A,w,Ti,Tf,num,bound)
sol <- solEuler_peso(X0,K,R2,A,w,Ti,Tf,num,bound)

#Para graficar
data <- data.frame(c(1:nrow(sol$evol_cont)),sol$evol_cont)
colnames(data)<- c("tiempo",  paste(rep("sp",10),c(0:9)))
library(tidyr)
library(ggplot2)
data.graficar <- pivot_longer(data,cols=2:11,names_to="sp",values_to="abundancia")
ggplot(data.graficar, aes(x = tiempo, y = abundancia))+
  geom_line(aes(color = sp),size=0.85) +
  theme_bw()

