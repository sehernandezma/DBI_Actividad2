setwd('C:\\Users\\Sebastian Hernandez\\Especializacion\\Semestre2\\Decisiones_incertidumbre\\practicas\\Actividad2')
regresion <- read.csv("datos_regresion.csv",sep=" ")
x = unlist(regresion['x'])
y = unlist(regresion['y'])
  
df <- data.frame(matrix(unlist(regresion), nrow=10, byrow=TRUE))
plot(regresion)

abline(a = 1.7432636416524427, b= 1.2761869441178362, col='red' )

plot(1, type="n", xlab="intercepto", ylab="pendiente", xlim=c(0, 5), ylim=c(0, 5))
punto1 <- locator(1)
punto2 <- locator(1)
punto3 <- locator(1)
punto4 <- locator(1)
punto5 <- locator(1)

plot(regresion)
x1 <- as.numeric(punto1[['x']])
y1 <- as.numeric(punto1[['y']])
abline(x1,y1)
plot(regresion)
x2 <- as.numeric(punto2[['x']])
y2 <- as.numeric(punto2[['y']])
abline(x2,y2)
plot(regresion)
x3 <- as.numeric(punto3[['x']])
y3 <- as.numeric(punto3[['y']])
abline(x3,y3)
plot(regresion)
x4 <- as.numeric(punto4[['x']])
y4 <- as.numeric(punto4[['y']])
abline(x4,y4)
plot(regresion)
x5 <- as.numeric(punto5[['x']])
y5 <- as.numeric(punto5[['y']])
abline(x5,y5)
####### Cálculo de MSE #########
m = 1.2761869441178362
b = 1.7432636416524427
sum = 0
typeof(regresion[[2]][1])

for (i in 1:10){
  
  x = regresion[[1]][i]
  y = regresion[[2]][i]
  
  pred = x*m+b
  print(pred)
  sum = sum + (y-pred)^2
  
}

MSE = sum/10
MSE


lista <- list(punto1,punto2,punto3,punto4,punto5) 
library(animation)
library(ggplot2)
library(RColorBrewer)
#library(tidyverse)
#library(tweenr)

saveGIF({
  for(i in lista){
    plot(regresion)
    x <- as.numeric(i[['x']])
    y <- as.numeric(i[['y']])
    abline(x,y)
    
    }
})

saveGIF({
  for (i in 1:10) plot(runif(10), ylim = 0:1)
})
#################### REGRESION LOGISTICA #############################

clasificacion <- read.csv("datos_clasificacion.csv",sep=" ")
cont <- read.csv("logistic1.csv",sep=",")
M <- matrix(unlist(cont), ncol = 10, byrow = TRUE)
plot(clasificacion)
for (i in 1:10){
  x = cont[[1]][i]
  y = cont[[2]][i]
  segments(x0=x,y0=0,x1=x,y1=y,col="red")
}
M
counts <- table(mtcars$gear)
#barplot(M, main="Conteo",xlab="X",names.arg=c("0.05","0.15","0.25","0.35","0.45","0.55","0.65","0.75","0.85","0.95"))
#barplot(M, main="Conteo",xlab="X")

counts
abline(v=2)

l_p <- punto1

plot(1, type="n", xlab="B0", ylab="B1", xlim=c(-10,0), ylim=c(0, 15))
punto6 <- locator(1)

X = seq(0,1,0.05)
X
Y <- vector()
B0 <- as.numeric(punto6[['x']])
B1 <- as.numeric(punto6[['y']])
a=0
for (x in X){
  print(a)
  tmp = 1/(1+exp(-1*(B0+B1*x)))
  Y = c(Y,tmp)
  
  a = a+1}
typeof(Y)
Y
graf <- list(X,Y)
names(graf) <- c("x", "y")
plot(clasificacion)
for (i in 1:10){
  x = cont[[1]][i]
  y = cont[[2]][i]
  segments(x0=x,y0=0,x1=x,y1=y,col="red")
}
lines(graf,type="l")

###### Accuracy ######

Tp=0
Tn=0
Fp=0
Fn=0
for (i in 1:100){
  
  x = clasificacion[[1]][i]
  y = clasificacion[[2]][i]
  
  B0 <- as.numeric(punto6[['x']])
  B1 <- as.numeric(punto6[['y']])
  
  pred = 1/(1+exp(-1*(B0+B1*x)))
  print(pred)
  if (pred >=0.5){
    p = 1
    if(p==y){
      Tp = Tp+1
    }
    if(p != y){
      Fp = Fp+1
    }
  }
  if (pred < 0.5){
    p = 0
    if(p==y){
      Tn = Tn+1
    }
    if(p != y){
      Fn = Fn+1
    }
  }
  
}
pres = (Tp+Tn)/(Tp+Tn+Fp+Fn)
pres


lista <- list(punto1,punto2,punto3,punto4,punto5,punto6) 
saveGIF({
  X = seq(0,1,0.05)
  for (punto in lista){
    Y <- vector()
    B0 <- as.numeric(punto[['x']])
    B1 <- as.numeric(punto[['y']])
    a=0
    for (x in X){
      print(a)
      tmp = 1/(1+exp(-1*(B0+B1*x)))
      Y = c(Y,tmp)
      
      a = a+1}
   
    graf <- list(X,Y)
    names(graf) <- c("x", "y")
    plot(clasificacion)
    for (i in 1:10){
      x = cont[[1]][i]
      y = cont[[2]][i]
      segments(x0=x,y0=0,x1=x,y1=y,col="red")
    }
    lines(graf,type="l")}
  
})
l_p <-append(l_p, punto1)














