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

