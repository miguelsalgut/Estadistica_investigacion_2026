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