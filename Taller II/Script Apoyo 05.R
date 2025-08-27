###########################
## Script Clase Apoyo 05 ##
###########################

## Semilla de Simulación 
set.seed(2306)

## Ejemplo 1: Lanzamiento de un dado 4 veces
y <- c()
for(i in 1:200000){
aux <- table(sample(1:6, 4, replace = T))
y[i] <- (max(aux) >= 2 & sum(aux == 2)<=1)*1
}
M <- cbind(mean(y), (choose(4,1)*6*5+choose(4,2)*6*5*4+6)/6^4)
colnames(M) <- c("Empírica", "Teórica")
round(M,4)

## Ejemplo 2: Seleccionar 6 pilas entre 12
PILAS <- c(1,1,1,1,1/2,1/2,0,0,0,0,0,0)
Y <- c()
for(i in 1:1000000){
Y[i] <- sum(sample(PILAS,6))
}
M <- cbind(mean(Y>=3), (choose(4,4)*choose(8,2)+choose(4,3)*choose(8,3)+choose(4,2)*choose(2,2)*choose(6,2))/choose(12,6))
colnames(M) <- c("Empírica", "Teórica")
round(M,4)

## Ejemplo 3: 


## Ejemplo 4: 

## Distribución Exponencial
## X ~ Exp(lambda)
## f(x) = lambda * exp(-lambda * x), x > 0

## Ej: Tiempo entre dos mensajes de chat
X <- c(0.5,1, 4, 2,5,1,26,2,15,1,21,18,1,3,51,5,0.1,7,10,9,8,36, 41, 16 ,13 ,15, 16  ,6, 16 , 2, 34 , 1,  6  ,2, 31 , 4 ,20  ,5 ,15 ,11, 26, 39,  6 , 3  ,5,  2 ,23 , 1  ,8, 19 , 6 , 3  ,7 , 5  ,5, 23,  2, 25 )


hist(X, freq = F, col = "gray", border = "white", breaks = seq(0,100,10),las = 1)
lambda <- 0.07
curve(dexp(x, rate = lambda), from = 0, to = 120, add = T, lwd = 2, n = 1000)

