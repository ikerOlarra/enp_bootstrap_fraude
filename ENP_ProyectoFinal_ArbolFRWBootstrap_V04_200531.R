
# El siguiente codigo es una adaptacion del contenido en:
# https://scholarcommons.usf.edu/cgi/viewcontent.cgi?article=9209&context=etd

# Importacion de librerias
library(MCMCpack)
library(rpart)
library(data.table)
library(gtools)

# Lectura de datos
#datos <- read.csv("Documents/Me/ITAM/10_Semestre/ENP/ProyectoFinal/paysim_procesado_sample.csv")

setwd("C:/Users/norma/Downloads")
# Lectura de datos
datos <- datos<-read.csv("paysim_procesado_sample1.csv")
n <- nrow(datos)

# Visualizacion de datos
head(datos)

# Conteo de operaciones fraudulentos y no fraudulentos
table(datos$isFraud)

datos <- datos[-c(1, 2)]

# Con esta funcion obtenemos una muestra bootstrap de tamano tamano
# -------------------------------------------------------------------
bootstrap <- function (datos, tamano, reemplazo)
{
  muestra <- datos[sample(nrow(datos), tamano, replace = reemplazo),]
  return (muestra)
}
# -------------------------------------------------------------------

# Arbol con muestra bootstrap
# datos - datos totales en los que se entrena el arbol
# regresores - nombres de las columnas
# etiqueta - lo que quiero predecir (en este caso isFraud)
# tamano - el tamano de la muestra bootstrap
# RF - Es el numero de columnas que consideraremos para Random Forest
# -------------------------------------------------------------------
bootstrap_tree <- function (datos, regresores, etiqueta, tamano, RF_regresores=0)
{
  # Primero vemos si debemos obtener una mestra de los regresores (Para Ranfom Forest)
  if (RF_regresores > 0)
  {
    # Seleccionamos el numero de regresores
    regresores <- sample(regresores, RF_regresores)
  }
  # Hacemos la muestra bootstrap de tamano train_size y con reemplazo
  muestra <- bootstrap(datos, tamano, TRUE)
  # Si todos los datos son de un unico grupo
  if(length(unique(muestra[,etiqueta])) < 2)
  {
    # Entonces no es necesario hacer la clasificacion
    root_tree <- as.character(muestra[1, etiqueta])
    return (root_tree)
  }
  else
  {
    # Generamos un arbol
    # Y ~ X + G + D
    # Equivalente en Reformulate (termlabels = c(X, G, D), response=Y)
    Tree <- rpart(reformulate(termlabels=regresores, response=etiqueta), data=muestra, method="class")
    return (Tree)
  }
}
# -------------------------------------------------------------------


# Arbol utilizando pesos FRW Bootstrap
# -------------------------------------------------------------------
frwb_tree <- function (datos, regresores, etiqueta, RF_regresores=0)
{
  pesos <- t(rdirichlet(1, rep(1, nrow(datos))))
  if (RF_regresores > 0)
  {
    # Seleccionamos el numero de regresores
    regresores <- sample(regresores, RF_regresores)
  }
  # Si todos los datos son de un unico grupo no hay nada que clasificar
  if(length(unique(datos[,etiqueta])) < 2)
  {
    # Entonces no es necesario hacer la clasificacion
    root_tree <- as.character(datos[1, etiqueta])
    return (root_tree)
  }
  else
  {
    # Generamos un arbol pero ahora tomando en cuenta los pesos
    Tree <- rpart(reformulate(termlabels=regresores, response=etiqueta), data=datos, weights=pesos, method="class")
    return (Tree)
  }
}
# -------------------------------------------------------------------

# Ahora haremos el Bagging del arbol Bootstrap
# -------------------------------------------------------------------
bagging_bootstrap_tree <- function(datos, regresores, etiqueta, tamano, numero_arboles, RF_regresores=0)
{
  bootstrap_tree_bag <- replicate (numero_arboles , bootstrap_tree(datos, regresores, etiqueta, tamano, RF_regresores), simplify = FALSE)
  return(bootstrap_tree_bag)
}
# -------------------------------------------------------------------

# Ahora hacemos el Bagging del arbol FRW
# -------------------------------------------------------------------
bagging_frwb_tree <- function(datos, regresores, etiqueta, numero_arboles, RF_regresores=0)
{
  if(RF_regresores > 0)
  {
    regresores <- sample(regresores, RF_regresores)
  }
  frwb_tree_bag <- lapply(as.list(seq(1, numero_arboles)), function(x) frwb_tree(datos, regresores, etiqueta))
  return(frwb_tree_bag)
}
# -------------------------------------------------------------------

# Ya generado el arbol vamos a entrenarlo
# Hacemos un train test split
# -------------------------------------------------------------------
# Generamos una muestra del 20% para prueba y el resto para entrenamiento
set.seed(20)
train <- sample(1:n, n * 0.8, replace = FALSE)

# Separamos los datos totales
training <- datos[train,]
testing <- datos[-train,]
# -------------------------------------------------------------------

# Definimos los regresores del arbol
# ------------------------------------------------------------------
regresores <- names(datos)[-6]
etiqueta <- "isFraud"
# ------------------------------------------------------------------

# Probemos el arbol FRW Boostrap
# -------------------------------------------------------------------
arbol <- frwb_tree(training, regresores, etiqueta)
# Informacion del regresor
arbol

# Prediccion de variables
prediccion <- predict(arbol, testing, type="class")

table(prediccion)

table(prediccion, testing$isFraud)
# -------------------------------------------------------------------

# Probemos el arbol Boostrap
# -------------------------------------------------------------------
arbol <- bootstrap_tree(training, regresores, etiqueta, nrow(training))
# Informacion del regresor
arbol

# Prediccion de variables
prediccion <- predict(arbol, testing, type="class")

table(prediccion)

table(prediccion, testing$isFraud)
# -------------------------------------------------------------------
# número de arboles a calcular
m<-10
# Probemos Bagging para arbol bootstrap
# -------------------------------------------------------------------
arboles1 <- bagging_bootstrap_tree(training, regresores, etiqueta, nrow(training), m)
# Esta lista contiene a 10 arboles
arboles1
# -------------------------------------------------------------------

# Probemos Bagging para arbol FRWBootstrap
# -------------------------------------------------------------------
arboles2 <- bagging_frwb_tree(training, regresores, etiqueta, m)
# Esta lista contiene a 10 arboles
arboles2
# -------------------------------------------------------------------

# Probemos Random Forest para arbol bootstrap
# -------------------------------------------------------------------
arboles3 <- bagging_bootstrap_tree(training, regresores, etiqueta, nrow(training), m, RF_regresores = 4)
# Esta lista contiene a 10 arboles
arboles3
# -------------------------------------------------------------------

# Probemos Random Forest para arbol FRWBootstrap
# -------------------------------------------------------------------
arboles4 <- bagging_frwb_tree(training, regresores, etiqueta, m, RF_regresores = 4)
# Esta lista contiene a 10 arboles
arboles4
# --


## Función para ver cuantos asignará bien para el dataset de testing 

#m el numero de arboles 
#bag lista de arboles 
#datapoint es el database para hacer el test de la predicción
#porcentaje_d con que porcentaje se va a decidir que se tome como notFraud or isFraud
#verdaderos=testing$isFraud

resultados<-function (bag, m, datapoint, porcentaje_d=.5, verdaderos ) {
  # vemos que resultados sacaron los 10 arboles del set testing 
  bag_prediction <- function ( bag , datapoint ) {
    l<-lapply ( seq (1 , length( bag ) ) , function( x) tree_prediction( bag[[ x ]] , datapoint ) )
    predictions <- data.frame(matrix(unlist(l), nrow=length(l), byrow=T))
    
    
    return  ( predictions) 
  }
  
  h<-bag_prediction(bag,datapoint)
  # predictions nos devuelve factores, necesitamos contar los 0's y 1's de las classes 
  #para decir que proporción de arboles clasififican a cada una y tomar una decisión. 
  hh<-matrix(0,nrow=dim(h)[1], ncol=dim(h)[2])  
  for(i in 1:dim(h)[2]){
    
    hh[,i]<-as.numeric( levels(h[[i]]))[h[[i]]]
    
  }
  
  colSums(hh) # nos devuelve la suma de 0's y 1's 
  p<-(m-colSums(hh))/ m # nos dice proporción de los  arboles que  marcaron como notFraud a la i'esima observación  del testing 
  r<-NULL
  r[p>porcentaje_d]<-0 #clase de la i-esima observación, si más del porcentaje_d  de arboles marcaron notFraud, entonces notFraud
  r[p<=porcentaje_d]<-1  #clase de la i-esima observación, si menos del porcentaje_d  de arboles marcaron notFraud, entonces isFraud
  
  
  
  return(table(verdaderos==r))
  
}

verdaderos<-testing$isFraud
### muestra la proporción de los que fueron bien clasificados o no del dataset testing 
resultados( arboles1, m,testing, .9,verdaderos)
resultados( arboles2, m,testing, .9,verdaderos)
resultados( arboles3, m,testing, .9,verdaderos)
resultados( arboles4, m,testing, .9,verdaderos)

resultados( arboles1, m,testing, .5,verdaderos)
resultados( arboles2, m,testing, .5,verdaderos)
resultados( arboles3, m,testing, .5,verdaderos)
resultados( arboles4, m,testing, .5,verdaderos)
