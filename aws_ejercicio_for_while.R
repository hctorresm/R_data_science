###########################
##Punto 1 distribucion binomial
###########################

pbinom(10, size = 100, prob = (1/9))

1- pbinom(10, size = 100, prob = (1/9))

pnorm(8, mean = 6.5, sd = 2, lower.tail = TRUE)

qnorm(0.95, mean = 6.5, sd = 2)

media = 100*0.92
desv_std= sqrt(100*0.92*(1-0.92))



pnorm(5.09, mean = 5, sd = 0.2, lower.tail = TRUE)


pbinom(10, size = 10, prob = pnorm(5.09, mean = 5, sd = 0.2, lower.tail = TRUE))



pbinom(10, size = 10, prob = pnorm(5.09, mean = 5, sd = 0.2, lower.tail = TRUE))



pbinom(50, size = 50, prob = (1- pnorm(1.19, mean = 1.23, sd = 0.2, lower.tail = TRUE)))




pbinom(30, size = 50, prob = (1- pnorm(18, mean = 17.5, sd = 4, lower.tail = TRUE)))

(1- pnorm(18, mean = 17.5, sd = 4, lower.tail = TRUE))^50


#############################################
###Sesión presencial 1 
#############################################


## Ciclos 
## cilo while 

limite <- 5 
valor <- 0 
while(valor < limite)
{
  print(paste("Todavía no.", valor))
  #print(valor)
  valor <- valor + 1
}




rep <- 50 # Número de repeticiones
for (n in 1:rep)
{
  print (n)
}



#####################################
## Lectura de datos
######################################




aws <- read.csv("C:/Users/COMPUTEL/Documents/RDatascience/AMZN.csv")

head(aws)



summary(aws)
aws[1, 5]
promedio = mean(aws$Close)
renglon = 0
for (renglon in 1:13)
{
  if (aws[renglon, 5] >= promedio)
  {
    print('Arriba promedio', str(aws[renglon, 5]))
  }
}





head(aws)
#aws[1, 5]



promedio <- mean(aws$Close)
renglon <- 0
for (renglon in 1:nrow(aws))
{
  if (aws$Close[renglon] >= promedio)
  {
    print(paste(
      'arriba del promedio, y su valor es',
      aws$Close[renglon],
      'y la fecha es',
      aws$Date[renglon]
    ))
  }
}




plot(
  aws$Close ,
  main = 'Acciones AWS',
  xlab = 'Días' ,
  ylab = 'Precio',
  pch = 19,
  frame.plot = TRUE
)
points(aws$Open, col = "green", pch = 19)
abline(h = promedio, lty = 2, col = "red")


###########################################
## Funciones lógicas
##########################################

#Funciones lógicas para manipular datos
library(data.table)
ventas <- read.csv("C:/Users/COMPUTEL/Documents/RDatascience/Ventas_registradascsv.csv")
summary(ventas)
ventas <- as.data.table(ventas)
ventas1 <- ventas[!is.na(ingreso)]
ventas1 <- ventas1[!is.na(edad)]
prom_ingreso <- mean(ventas1[,ingreso]) 
promedio_edad <- mean(ventas1[,edad])
##Remplazo de ingreso y edad por los promedios
ventas[, ingreso:= ifelse(is.na(ingreso),prom_ingreso, ingreso)]
ventas[, edad:= ifelse(is.na(edad),promedio_edad, edad)]
##Distribuciiones por variables
hist(ventas[,edad])
hist(ventas[,ingreso])
##Para graficar los que comprarons y los que no compraron
compra <- ventas[, .N, by = compra ]
barplot(compra[, N], main = "Compras y no commpran", col = rainbow(3))
##Nuevo data set 
ventas_final <- ventas[ingreso > 6000 & compra == "si"]
summary(ventas_final)








