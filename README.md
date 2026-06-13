# Estadística en la Investigacion 2026

Material del curso del Posgrado de Estadística en la Investigación Científica DCMRN EJ 2026


# Producto Integrador del Aprendizaje  
## Unidad de Aprendizaje: Diseños Experimentales

**Alumno:** Miguel Salvador Salinas  
**Programa:** Doctorado en Ciencias con Orientación en Manejo de Recursos Naturales  
**Institución:** Universidad Autónoma de Nuevo León  
**Facultad:** Facultad de Ciencias Forestales  
**Curso:** Diseños Experimentales  
**Docente:** Dr. Marco Aurelio González Tagle  
**Periodo:** Enero 2026  

---

## Descripción general

Este repositorio integra las evidencias desarrolladas durante la unidad de aprendizaje de Diseños Experimentales. El propósito es organizar los scripts elaborados en R, tareas, laboratorios y evidencias de aprendizaje relacionadas con el análisis estadístico, la programación en RStudio y el uso de GitHub como herramienta de documentación académica.

El repositorio funciona como evidencia digital del trabajo realizado durante el curso y permite consultar de manera ordenada los avances, prácticas y ejercicios desarrollados.

---
# Contenido del curso 

Repositorio_Analisis_Estadistico/
│
├── Scripts/
├── Tareas/
├── Laboratorios/
├── Evidencias/
└── README.md

## Scripts del curso

# Script_Semana 1
# Miguel Salvador Salinas Gutiérrez

#      Posgrado Estadistica 2026

#     +Material del curso de Posgrado del programa DMRN
#     +Estadística en la investigación científica

#     # Contenido del curso (8 semanas)

#     **Semana 1**: Inicio del curso 26/01/26
#     + Crear repositorio
#     + Sincronizar nube y computadora
#     + Credenciales Git
#     + Sinc

# Miguel Salvador Salinas Gutiérrez
# 1530268
# 16/02/2026

#

# Importar datos ----------------------------------------------------------

# Función read.csv
IE <- read.csv("Datos/vivero.csv", header = T)
IE$Fertilizante <- as.factor(IE$Fertilizante)


tapply(IE$IE, IE$Fertilizante, var)

# Gráfica -----------------------------------------------------------------


boxplot(IE$IE)

boxplot(IE$IE ~ IE$Fertilizante,
        col = "lightblue",
        xlab = "Fertilizante",
        ylab = "Indice de esbeltez",
        main ="Vivero FCF",
        ylim = c(0.4, 1.2))

# Pruebas de normalidad ---------------------------------------------------

shapiro.test(IE$IE)

# el valor de p en shapiro de .1777 dice que acepto hipo nula y dice que los datos son normales.

bartlett.test(IE$IE ~ IE$Fertilizante)

# p value de 0.05305 demuestra que las variables son... Entonces, con p = 0.05305:
# a un nivel de 5%, no hay evidencia suficiente para rechazar H0.

hist(IE$IE,
     col = "red",
     ylim = c(0,12),
     main = "",
     ylab = "Variable IE",
     xlab = "Frecuencia")


# 09 de marzo -------------------------------------------------------------

# Función read.csv
IE <- read.csv("Datos/vivero.csv", header = T)
IE$Fertilizante <- as.factor(IE$Fertilizante)


tapply(IE$IE, IE$Fertilizante, var)
tapply(IE$IE, IE$Fertilizante, mean)

# prueba T

t.test(IE$IE ~ IE$Fertilizante, var.equal = T,
       alternative = "greater")

t.test(IE$IE ~ IE$Fertilizante, var.equal = T,
       alternative = "less")

t.test(IE$IE ~ IE$Fertilizante, var.equal = T,
       alternative = "two.sided")

t.test(IE$IE ~ IE$Fertilizante, var.equal = F,
       alternative = "two.sided")


# Prueba de T de una muestra. Caso de estudio, si quiero que el IE sea 0.85 (mu), me pagas la planta?
mean(IE$IE)

t.test(mu = 0.85, IE$IE)


# Mismo grupo de individuos, antes y después de usar fertilizante
# Datos dependientes

t.test(IE$IE ~ IE$Fertilizante, paires = T)

Ctrl <- subset(IE$IE, IE$Fertilizante == "Ctrl")
Fert <- subset(IE$IE, IE$Fertilizante != "Ctrl")

t.test(Ctrl, Fert, paired = TRUE)

# revisar los casos  que existen, son 3, los analizados previamente. Less, greater y two sided.

#Miguel Salvador Salinas Gutiérrez
# 1530268
# 23/02/2026


# Correlación de Spearman -------------------------------------------------


resp <- data.frame(
  Tiempo = c(12, 15, 17, 18, 20, 21, 22, 26),
  Edad = c(14, 25, 20, 35, 45, 30, 60, 95)
)
resp

#Crear nuevas columnas con los rangos (1 a 8)
resp$Rango_Tiempo <- rank(resp$Tiempo, ties.method = "first")
resp$Rango_Edad <- rank(resp$Edad, ties.method = "first")

#Ver resultado
resp

plot(resp$Tiempo, resp$Edad)
plot(resp$Rango_Tiempo, resp$Rango_Edad)

# Graficar ----------------------------------------------------------------

cor.test(resp$Rango_Tiempo, resp$Rango_Edad, method = "spearman")

# Tau de Kendall ----------------------------------------------------------


#Correlación Tau de Kendall
#Los datos de ser ordinales, cualitativos entonces?

tau <- data.frame(
  A = c(1, 2, 3, 4, 5, 6),
  B = c(3, 1, 4, 2, 6, 5)
)
cor.test(tau$A, tau$B, method = "kendall")

#Interpretación del valor de r
#un valor r entre 0 y 0.1 indica que no hay correlación. 
#Un valor entre 0.7 y 1 indica que hay correlación alta.


# Punto Biserial ----------------------------------------------------------


#Correlación Punto Biserial
#Es una variaciión de la correlación de Pearson
#Correlación entre una variable dicotómica y una métrica.

#Ejemplo

set.seed(123)

# Número de observaciones
 n <- 20
 
#Generar horas de estudio (entre 1 y 10)
 Horas_estudio <-  sample(1:10, n, replace = TRUE)
 
 # Asignar probabilidad de aprobar en función de horas de estudio
 # A más horas, más alta la probabilidad
 
 Resultado <- sapply(Horas_estudio, function(horas) {
   ifelse(runif(1) < (horas / 10), "Aprobado", "Reprobado") 
   
 })

 # Crear data frame
 Estudio <- data.frame(
   Estudiante = 1:n,
   Horas_estudio,
   Resultado
 )
 
Estudio$Resultado_bin <- ifelse(Estudio$Resultado == "Aprobado", 1, 0)
head(Estudio)
 
cor.test(Estudio$Horas_estudio, Estudio$Resultado_bin, method = "pearson")

# Miguel Salvador Salinas
# 1530268
# Sesión 2 de marzo  de 2026

x3 <- c(10.0, 8.0, 13.0, 9.0, 11.0, 14.0, 6.0, 4.0, 12.0, 7.0, 5.0)
y3 <- c(7.46, 6.77, 12.74, 7.11, 7.81, 8.84, 6.08, 5.39, 8.15, 6.42, 5.73)

# Correlación
r <- cor(x3, y3, method = "pearson")
r

# p-value
resultado <- cor.test(x3, y3, method = "pearson")

resultado
# Media de X
mean(x3)

# Media de Y
mean(y3)

# Gráfica de dispersión
plot(x3, y3,
     col = "red",
     main = "Diagrama de dispersión X vs Y",
     xlab = "Variable X",
     ylab = "Variable Y")

# Regresión lineal  -------------------------------------------------------

# Modelo que permite describir cómo influte una variable X sobre una variable Y.
# X es la variable independiente o explicativa o exógena
# Y es la variable dependiente o respuesta o endógena
# El objetivo es obtener estimaciones reales de Y para distintos valores de X a partir de una muestra de n para pares de valores (x1, y1)....(Xn, Yn)... etc.

#Ejemplos; Crecimiento de árboles, hidrología, pesca, etc.

#tipos de relación:
## determinista: Conocido el valor de X, el valor de Y queda perfectamente establecido. y= f(x)+
## no determinista
## lineal
## no lineal
##ausencia de relación entre X y Y

# Regresión lineal simple Y=β0+β1X1+ε

##Supuestos del modelo de regresión lineal
# Linealidad
# Homogeneidad
# Independencia
# Normalidad

# Estimadores de mínimos cuadrados

# Gauss con el método de mínimos cuadrados para obtener valores de beta0, beta1, que mejorse ajustan a los datos.
# El método consiste en minimizar la suma de los cuadrados de las distancias verticales entre los datos y las estimaciones, minimiza la suma de los residuos al cuadrado.

#Ejercicio 1
# Producción de trigo (X)
x <- c(30, 28, 32, 25, 25, 25, 22, 24, 35, 40)

# Precio de la harina (Y)
y <- c(25, 30, 27, 40, 42, 40, 50, 45, 30, 25)
x
y
length(x)
length(y)

datos <- data.frame(Produccion = x, Precio = y)
datos

cor.test(datos$Produccion, datos$Precio)

plot(datos$Produccion, datos$Precio)

fit.lm <- lm(datos$Precio ~ datos$Produccion)
summary(fit.lm)
# En los resultados, RESIDUALS min significa el error mínimo que hubo (-8.50), max el error máximo (5.66), osea que en total son como 14 la diferencia total.
# COEFFICIENTS me da los valores alfa y beta, me dice que ambos son significativos ** o ***
# ADJUSTED R-SQUARED es de 0.6824 (me dice que el 68% de los datos y son explicados por x), p-value es 0.001978 (dice que la relacion es altamente significativo) 

anova(fit.lm)
#Debemos conocer el valor de P, aparece como Pr(>F) indica que es significativo.
# El valor de SSE es: 207.93
# Varianza de los errores: 25.99
# La desviación estándar residual es 5.09

# residuales en el modelo lineal fit.lm
fit.lm$residuals
# valores ajustados y orima en el modelo lineal
fit.lm$fitted.values

# Gráfica de y-prima 
plot(fit.lm$fitted.values, fit.lm$residuals)

#se observa en el plot que los residuales estan relativamente dispersos anrriba y abajo de 0, pero no forman una banda perfectamente uniforme u homogenea.
# 

library(lmtest)
# Datos
x <- c(30, 28, 32, 25, 25, 25, 22, 24, 35, 40)

# Precio de la harina (Y)
y <- c(25, 30, 27, 40, 42, 40, 50, 45, 30, 25)
# Modelo
m <- lm(y ~ x)

# Breusch-Pagan (varianza ~ x)
bptest(fit.lm)

# Lo que importa es el P-value (0.5641) me dice que cumplo con el criterio de homosedasticidad. No hay elementos para decir que son no son dif las varianzas o no varían. Si fuera menor a 0.5641 puede ser.

# Agregar una columna con la recta usando b0 y b1
datos$recta <- 74.116 - 1.3537*datos$Produccion
# la formula es y(prima)= alfa + beta*primer valor de xi
datos$residuales <- datos$Precio - datos$recta
datos

#Estimar la varianza de los residuales
sum(datos$residuales^2)

# Estinar la varianza de los residuales
sum(datos$residuales^2)/8

# Estimar la desviación estandar residual
sqrt(sum(datos$residuales^2)/8)

# cóomm poner la línea de regresión
plot(datos$Produccion, datos$Precio,
     col = "red",
     xlab = "Procucción de trigo (Ton/ha)",
     ylab = "Precio de harina (Euros)",
     pch = 19,
     cex =1.2)
abline(fit.lm, col = "blue")

# Estadistica en la Investigación 2026
# Repositorio del programa de Doctorado Estadística en la Investigación DCMRN 

# Miguel Salvador Salinas Gutierrez

# Matrícula: 1530268

#**Sesión 5**: Fecha 09/03/26

# EJERCICIOS --------------------------------------------------------

# Comparación de medias
# Tipos de muestras 

# Muestras ind

# Muestras dep



# Prueba TStudent compara 2 medias

# características de las pruebas
# datos normales, homogeneidad, n=30


# Importar datos --------------------------------------------------------

# Función read.csv

IE <-read.csv("03_Datos/vivero.csv", header = T)
IE$Tratamiento <-as.factor(IE$Tratamiento)

View(IE)

tapply(IE$IE, IE$Tratamiento, mean)
tapply(IE$IE, IE$Tratamiento, sd)
tapply(IE$IE, IE$Tratamiento, var)


# Diseño de gráfico -------------------------------------------------------


boxplot(IE$IE)

boxplot(IE$IE ~ IE$Tratamiento,
        col = "lightblue",
        xlab = "Fertilizante",
        ylab = "IE",
        main = "Vivero FCF",
        ylim = c(0.4, 1.2))
# ~ significa en función de o sea cuando tienes varias variables

# Pruebas de normalida ----------------------------------------------------
# Saphiro y Kolmogorov

shapiro.test(IE$IE)

# Homogeneidad de varianza ------------------------------------------------

bartlett.test(IE$IE ~ IE$Tratamiento)


hist(IE$IE,
     col = "red",
     ylim = c(0,12),
     main = "",
     ylab ="Frecuencia",
     xlab = "Variable IE")
###############################################################################
# pruebas para independientes
t.test(IE$IE ~ IE$Tratamiento, var.equal =T)


# Intervalo de confianza

#var.equal =T (no importa si alguno de tus datos es mayor o no  que el otro)


t.test(IE$IE ~ IE$Tratamiento, var.equal =T,
       alternative= "greater")
#con este comando es para 1 sola cola cuando ya estas diciendo en tu hipotesis que es mayor que


t.test(IE$IE ~ IE$Tratamiento, var.equal =T,
       alternative= "less")

#diferencia greater y less (depende de como lo vayas a plantear en la hipótesis)

# ----

# Prueba de T una muestra
mean(IE$IE)


t.test(mu = 0.85, IE$IE)
#mu media teoretica

#prueba dependiente
Ctrl <- subset(IE$IE, IE$Tratamiento == "Ctrl")
Fert <- subset(IE$IE, IE$Tratamiento != "Ctrl")

t.test(Ctrl, Fert, paired = T)





