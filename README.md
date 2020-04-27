# enp_bootstrap_fraude

## Integrantes del equipo

Norma García Salazar
Javier Montiel González
Karla Mayra Pérez Muñoz
Iker Antonio Olarra Maldonado

## Propuesta de proyecto

Implementación de un modelo con Fractional-Random-Weight Bootstrap para detección de fraude.

Este modelo será basado en el paper __Applications of the Fractional-Random-Weight Bootstrap__ que se encuentra en la siguiente liga: https://piazza.com/class_profile/get_resource/k4yt760zltv2ji/k91qc67fa9j6t7. Además el modelo se probará en el dataset __paysim.csv__.

### Motivación

El fraude en tarjetas de crédito se estima que alcanzarán $35 mil millones al año en 2020. Aunque la detección usando algoritmos de aprendizaje de máquina y otro tipo de modelos de riesgo es un tema que se ha vuelto recurrente en los últimos años no hemos observado que el enfoque no paramétrico haya sido estudiado a profundidad.

### Alcance

Un modelo que use Fractional-Random-Weight Bootstrap para establecer una muestra que después pueda servir para entrenar un modelo de clasificación binaria (e.g. regresión logística) que logre separar con una precisión adecuada las operaciones frudulentas de las legítimas.

El modelo será probado en el dataset __paysim.csv__ que se encuentra en https://www.kaggle.com/ntnu-testimon/paysim1 y contiene más de 6 millones de transacciones de las cuales una minoría representan una operación fraudulenta. 

### Resultados esperados

El resultado será un modelo que identifique las transacciones fraudulentas con una precisión mayor al 70%. Para hacer la validación del modelo planeamos usar una separación en conjunto de entrenamiento y de prueba o usar jacknife o Cross-validation dependiendo de el conjunto de datos arrojado por Bootstrap.

### Actividades

1. Investiagación profunda del método __Fractional-Random-Weight Bootstrap__.
2. Investigación de métodos de clasificación binaria.
3. Análisis exploratorio del set de datos.
4. Creación de dataset balnceado usando Bootstrap.
5. Selección de métodos de clasificación binaria.
6. Entrenamiento del modelo.
7. Análisis de resultados.
8. Regresar al punto 4 (si es necesario).
9. Conclusiones y presentación del modelo.

### Metodología

Usaremos Fractional-Random-Weight Bootstrap para balancear la muestra y un modelo de clasificación binaria para identificar transacciones fraudulentas. La implementación del modelo será en R y/o Python. 


