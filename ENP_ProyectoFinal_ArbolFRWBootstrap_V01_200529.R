
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

# Con esta funcion obtenemos una muestra bootstrap de tamano count
bootstrap <- function (datos, tamano, reemplazo)
{
  resample <- datos[sample(nrow(datos), tamano, replace = reemplazo),]
  return (resample)
}

# Arbol con muestra bootstrap
bootstrap_tree <- function (datos, regresores, etiqueta, tamano)
{
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
    print(regresores)
    Tree <- rpart(reformulate(termlabels=regresores, response=etiqueta), data=muestra, method="class")
    return (Tree)
  }
}

# Arbol con muestra bootstrap
frwb_tree <- function (datos, regresores, etiqueta, peso)
{
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
    Tree <- rpart(reformulate(termlabels=regresores, response=etiqueta), data=datos, weights=pesos, method="class")
    return (Tree)
  }
}

# Ya generado el arbol vamos a entrenarlo
# Hacemos un train test split

# Generamos una muestra del 20% para prueba y el resto para entrenamiento
train <- sample(1:n, n * 0.8)

# Separamos los datos totales
training <- datos[train,]
testing <- datos[-train,]

regresores <- names(datos)[-6]
etiqueta <- "isFraud"
pesos <- t(rdirichlet(1, rep(1, nrow(training))))

arbol <- frwb_tree(training, regresores, etiqueta, pesos)
# Informacion del regresor
arbol

# Prediccion de variables
prediccion <- predict(arbol, testing, type="class")

table(prediccion)

table(prediccion, testing$isFraud)

