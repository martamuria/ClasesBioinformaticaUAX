# TRABAJO R MARTA MURIA CABRERO

########PREGUNTA 1########

# Cargo los datos
datos <- read.table("datos.txt", header = TRUE, sep = "\t")
datos

# Examino los datos
# Para mostrar las primeras filas
head(datos)

# Resumen estadístico de las variables
summary(datos)

# Dimensiones del conjunto de datos (vemos número de filas y columnas)
dim(datos)

# Estructura de los datos
str(datos)

## ¿Cuántas variables hay? Hay 2 variables "Variable1" y "Variable2". 
# Esto se puede determinar a partir de la salida de head(datos)

## ¿Cuántos tratamientos? Hay 5 tratamientos.
# Esto se puede determinar observando los valores únicos en la columna "Tratamiento".

########PREGUNTA 2########

# Creo un boxplot para Variable1
boxplot(Variable1~Tratamiento,data=datos, col = "red")

# Le agrego un título
title("Boxplot Variable1 vs Tratamiento")

# Creo un boxplot para Variable2
boxplot(Variable2~Tratamiento,data=datos, col = "blue")

# Le agrego un título
title("Boxplot Variable2 vs Tratamiento")

########PREGUNTA 3########

# Creo un gráfico de dispersión con colores por tratamiento
# pch = 1 se refiere a un tipo específico de marcador en este caso circunferencias.
plot(datos$Variable1, datos$Variable2, col = datos$Tratamiento, 
     pch = 1, xlab = "Variable1", ylab = "Variable2",
     main = "Gráfico de Dispersión por Tratamiento")

########PREGUNTA 4########

# Agrego una leyenda en la esquina inferior derecha
# Uso "bottomright" para colocar la leyenda en esquina inferior derecha del gráfico.
legend("bottomright", legend = unique(datos$Tratamiento), 
       col = unique(datos$Tratamiento), pch = 1, title = "Tratamiento")

########PREGUNTA 5########

# Histograma para la Variable1
hist(datos$Variable1, col = "red", main = "Histograma Variable1", xlab = "Variable1", ylab = "Frecuencia")

# Histograma para la Variable2
hist(datos$Variable2, col = "blue", main = "Histograma Variable2", xlab = "Variable2", ylab = "Frecuencia")

########PREGUNTA 6########

# Creo un factor a partir de la columna "Tratamiento" y lo guardo en una nueva variable
datos$TratamientoFactor <- factor(datos$Tratamiento)
datos$TratamientoFactor
head(datos$TratamientoFactor)

########PREGUNTA 7########

# Usando aggregate

# Calculo la media y desviación estándar para Variable1 por tratamiento
media_variable1 <- aggregate(Variable1 ~ TratamientoFactor, datos, mean)
desviacion_variable1 <- aggregate(Variable1 ~ TratamientoFactor, datos, sd)

# Resultados para Variable1
print("Media de Variable1 por Tratamiento:")
print(media_variable1)
print("Desviación Estándar de Variable1 por Tratamiento:")
print(desviacion_variable1)

# Calculo la media y desviación estándar para Variable2 por tratamiento
media_variable2 <- aggregate(Variable2 ~ TratamientoFactor, datos, mean)
desviacion_variable2 <- aggregate(Variable2 ~ TratamientoFactor, datos, sd)

# Resultados para Variable2
print("Media de Variable2 por Tratamiento:")
print(media_variable2)
print("Desviación Estándar de Variable2 por Tratamiento:")
print(desviacion_variable2)

########PREGUNTA 8########

# Uso table() para contar el número de elementos por tratamiento
elementos_por_tratamiento <- table(datos$TratamientoFactor)
elementos_por_tratamiento # Muestra la cantidad de elementos por tratamiento. 
# Vemos que dada tratamiento tiene 10 elementos

########PREGUNTA 9########

# Extraigo los datos para el tratamiento 1 y los guardo en una variable
datos_tratamiento_1 <- datos[datos$TratamientoFactor == "1", ]
datos_tratamiento_1

# Extraigo los datos para el tratamiento 4 y los guardo en otra variable
datos_tratamiento_4 <- datos[datos$TratamientoFactor == "4", ]
datos_tratamiento_4

########PREGUNTA 10########

# Nuestra hipótesis nula es que las medias de tratamiento 1 y tratamiento 4 para la Variable 1 son iguales
### ¿Puedes comprobarlo? Para ello, necesitarás comprobar primero si los datos se distribuyen de forma normal
# Uso el comando shapiro.test() para ver si los datos siguen una distribución normal.

shapiro.test(datos_tratamiento_1$Variable1)  # Prueba de normalidad para tratamiento 1
# Tiene un p value de 0.06434

shapiro.test(datos_tratamiento_4$Variable1)  # Prueba de normalidad para tratamiento 4
# Tiene un p value de 0.1564

# Ambos p values son mayores de 0.05, por lo que NO se rechaza la hipótesis nula.
# Los datos no siguen una distribución normal.


### En función del resultado de la prueba de normalidad, ¿qué test usarías? 
# Se usaría la prueba de t-student si nuestras variables fueran iguales.
# Como no siguen una distribución normal, uso el test de Wilcoxon-Mann-Whitney para tratamiento 1 y tratamiento 4
# Con este test comprobamos si el tratamiento 1 y 4 en base a la variable 1 son iguales o no.

wilcox.test(datos_tratamiento_1$Variable1, datos_tratamiento_4$Variable1,exact=FALSE)

# El resultado (p value= 0.0001727) indica que no son iguales ya que es menor de 0,05
# Se rechaza la hipótesis nula. Los tratamientos son estadísticamente diferentes.


### En general, asumimos que las muestras son independientes, pero ¿son sus varianzas iguales? 
# Para hacer la prueba de igualdad de varianzas uso el comando var.test()

var.test(datos_tratamiento_1$Variable1, datos_tratamiento_4$Variable1)

# EL resultado es un p-value de 4.595e-07. Como el pvalue es menor de 0.05 las varianzas son diferentes.
# Las varianzas son estadísticamente diferentes.