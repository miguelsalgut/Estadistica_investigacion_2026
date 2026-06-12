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


