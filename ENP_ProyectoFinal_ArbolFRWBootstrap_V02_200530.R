
# El siguiente codigo es una adaptacion del contenido en:
# https://scholarcommons.usf.edu/cgi/viewcontent.cgi?article=9209&context=etd

# Importacion de librerias
library(MCMCpack)
library(rpart)
library(data.table)
library(gtools)

# Lectura de datos
datos <- read.csv("Documents/Me/ITAM/10_Semestre/ENP/ProyectoFinal/paysim_procesado_sample.csv")
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
bootstrap_tree <- function (datos, regresores, etiqueta, tamano, RF=0)
{
  # Primero vemos si debemos obtener una mestra de los regresores (Para Ranfom Forest)
  if (RF > 0)
  {
    # Seleccionamos el numero de regresores
    regresores <- sample(regresores, RF)
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
frwb_tree <- function (datos, regresores, etiqueta, peso, RF=0)
{
  if (RF > 0)
  {
    # Seleccionamos el numero de regresores
    regresores <- sample(regresores, RF)
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
    print(regresores)
    print(nrow(datos))
    print(nrow(pesos))
    print(ncol(pesos))
    Tree <- rpart(reformulate(termlabels=regresores, response=etiqueta), data=datos, weights=pesos, method="class")
    return (Tree)
  }
}
# -------------------------------------------------------------------

# Ahora haremos el Bagging del arbol Bootstrap
# -------------------------------------------------------------------
bagging_bootstrap_tree <- function (datos, regresores, etiqueta, tamano, numero_arboles)
{
  bootstrap_tree_bag <- replicate (numero_arboles , bootstrap_tree(datos, regresores, etiqueta, tamano), simplify = FALSE)
  return(bootstrap_tree_bag)
}
# -------------------------------------------------------------------

# Ahora hacemos el Bagging del arbol FRW
# -------------------------------------------------------------------
bagging_frwb_tree <- function (datos, regresores, etiqueta, pesos, numero_arboles)
{
  frwb_tree_bag <- lapply(as.list(seq(1, numero_arboles)), function(x) frwb_tree(datos, regresores, etiqueta, pesos[x,]))
  return(frwb_tree_bag)
}
# -------------------------------------------------------------------

# Ya generado el arbol vamos a entrenarlo
# Hacemos un train test split
# -------------------------------------------------------------------
# Generamos una muestra del 20% para prueba y el resto para entrenamiento
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
pesos <- t(rdirichlet(1, rep(1, nrow(training))))
arbol <- frwb_tree(training, regresores, etiqueta, pesos)
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

# Probemos Bagging para arbol bootstrap
# -------------------------------------------------------------------
arboles <- bagging_bootstrap_tree(training, regresores, etiqueta, nrow(training), 10)
# Esta lista contiene a 10 arboles
arboles
# -------------------------------------------------------------------

# Probemos Bagging para arbol bootstrap
# -------------------------------------------------------------------
pesos <- t(rdirichlet(10, rep(1, nrow(training))))
arboles <- bagging_frwb_tree(training, regresores, etiqueta, pesos, 10)
# Esta lista contiene a 10 arboles
arboles
# -------------------------------------------------------------------
