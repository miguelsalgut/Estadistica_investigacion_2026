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

