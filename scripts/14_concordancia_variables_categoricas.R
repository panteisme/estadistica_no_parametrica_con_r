# ==============================================================================
# Curso de Estadística no paramétrica con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Tema 14: Concordancia: Variables categóricas
# ==============================================================================

# Cargar librerías necesarias
library(knitr)
library(irr)
library(psych)
library(boot)

# ==============================================================================
# 1. DATOS
# ==============================================================================
# Contexto: Comparación de dos métodos diagnósticos de embarazo
# - Método 1: ELISA
# - Método 2: Ultrasonido (US)
# n = 1673 mujeres

# Crear tabla de contingencia
# Filas: ELISA | Columnas: Ultrasonido
tabla_embarazo <- matrix(c(596, 61,
                           29, 987),
                         nrow = 2, byrow = TRUE)

dimnames(tabla_embarazo) <- list(
  ELISA = c("Positivo", "Negativo"),
  US = c("Positivo", "Negativo")
)

# Mostrar tabla cruzada
kable(tabla_embarazo, 
      caption = "Tabla de contingencia: Concordancia entre ELISA y Ultrasonido")

# ==============================================================================
# 2. KAPPA DE COHEN
# ==============================================================================
# Mide el acuerdo entre dos evaluadores/métodos más allá del azar.
#
# Interpretación (Landis & Koch, 1977):
# - < 0.00: Sin acuerdo
# - 0.00 - 0.20: Insignificante
# - 0.21 - 0.40: Discreto
# - 0.41 - 0.60: Moderado
# - 0.61 - 0.80: Sustancial
# - 0.81 - 1.00: Casi perfecto

# Calcular Kappa de Cohen  desde la tabla de contingencia con psych
kappa <- cohen.kappa(tabla_embarazo)
kappa

# Convertir tabla a formato para irr
datos_kappa <- matrix(c(rep(1, 596), rep(1, 61), rep(2, 29), rep(2, 987),
                        rep(1, 596), rep(2, 61), rep(1, 29), rep(2, 987)),
                      ncol = 2)

# Calcular Kappa de Cohen con irr
kappa_resultado <- kappa2(datos_kappa, weight = "unweighted")
kappa_resultado

# ==============================================================================
# 3. KAPPA PONDERADO (Variables ordinales)
# ==============================================================================
# Para variables con categorías ordenadas, donde los desacuerdos parciales
# tienen menos peso que los desacuerdos totales.
#
# Selección del tipo de ponderación:
# |---------------------|---------------------------------------------------|
# | Ponderación         | Usar cuando...                                    |
# |---------------------|---------------------------------------------------|
# | Lineal (equal)      | Todos los desacuerdos tienen importancia similar  |
# | Cuadrática (squared)| Desacuerdos extremos son mucho más graves         |
# |---------------------|---------------------------------------------------|
#
# En contextos clínicos se prefiere la ponderación cuadrática porque penaliza
# más fuertemente las discrepancias grandes (ej: clasificar "Bajo peso" como
# "Obesidad" es peor que clasificarlo como "Normal").

# ------------------------------------------------------------------------------
# 3.1 Datos: Clasificación de IMC en gestantes (n = 102)
# ------------------------------------------------------------------------------
# Comparación de dos métodos de clasificación:
# - Método 1: Criterio de Atalah
# - Método 2: Criterio de Rosso-Mardones

IMC_atalah <- c(
  3,3,4,4,3,3,3,4,3,4,3,3,4,3,3,3,2,3,3,3,
  3,2,3,3,2,3,2,3,2,2,2,2,2,2,3,3,2,2,3,2,
  2,2,2,2,2,3,2,2,2,2,3,2,2,2,2,2,2,2,2,2,
  2,3,2,3,2,3,2,2,2,2,2,3,2,2,3,2,3,2,3,2,
  2,2,2,2,2,2,3,2,1,1,1,2,2,2,1,1,2,2,2,2,
  2,1
)

IMC_rosso_mardones <- c(
  3,4,4,3,4,4,4,4,3,4,4,4,4,4,2,3,2,4,4,3,
  3,3,3,3,2,3,2,3,2,2,3,2,2,2,4,3,2,2,3,2,
  2,2,1,2,2,3,2,2,3,2,3,1,2,2,2,3,2,2,2,2,
  2,4,3,4,4,3,2,2,2,2,3,2,2,3,2,4,2,3,2,2,
  2,2,2,2,3,2,1,2,1,2,2,2,1,1,1,2,2,2,2,1,
  2,2
)

# Etiquetas para los niveles del IMC
etiquetas_imc <- c("Bajo peso", "Normal", "Sobrepeso", "Obesidad")

# Conversión a factores ordenados
IMC_atalah <- factor(IMC_atalah, levels = 1:4, labels = etiquetas_imc, ordered = TRUE)
IMC_rosso_mardones <- factor(IMC_rosso_mardones, levels = 1:4, labels = etiquetas_imc, ordered = TRUE)

# ------------------------------------------------------------------------------
# 3.2 Tabla cruzada
# ------------------------------------------------------------------------------
tabla_cruzada <- table(IMC_atalah, IMC_rosso_mardones)
print(tabla_cruzada)

# ------------------------------------------------------------------------------
# 3.3 Kappa ponderado cuadrático
# ------------------------------------------------------------------------------
kappa_ponderado <- kappa2(cbind(as.integer(IMC_atalah), as.integer(IMC_rosso_mardones)), 
                          weight = "squared")
print(kappa_ponderado)

# ------------------------------------------------------------------------------
# 3.4 Intervalo de confianza por bootstrap
# ------------------------------------------------------------------------------
set.seed(123)

# Función para bootstrap
kappa_boot <- function(data, indices) {
  d <- data[indices, ]
  k <- kappa2(cbind(d[,1], d[,2]), weight = "squared")
  return(k$value)
}

# Preparar datos
data_kappa <- data.frame(as.integer(IMC_atalah), as.integer(IMC_rosso_mardones))

# Ejecutar bootstrap (1000 repeticiones)
boot_result <- boot(data_kappa, statistic = kappa_boot, R = 1000)

# IC 95%
boot_ci <- boot.ci(boot_result, type = "perc")
print(boot_ci)

# ==============================================================================
# 4. KAPPA DE FLEISS (3 o más observadores)
# ==============================================================================
# Extensión del Kappa de Cohen para múltiples evaluadores.
# Evalúa el acuerdo entre 3 o más observadores que clasifican sujetos
# en categorías mutuamente excluyentes.
#
# Interpretación (Landis & Koch, 1977):
# - < 0.00: Sin acuerdo
# - 0.00 - 0.20: Insignificante
# - 0.21 - 0.40: Discreto
# - 0.41 - 0.60: Moderado
# - 0.61 - 0.80: Sustancial
# - 0.81 - 1.00: Casi perfecto

library(irr)

# ------------------------------------------------------------------------------
# 4.1 Datos: Clasificación de severidad por 10 observadores (n = 25 pacientes)
# ------------------------------------------------------------------------------
# Escala de severidad: 0 = Ninguna, 1 = Mínima, 2 = Leve, 3 = Moderada, 
#                      4 = Severa, 5 = Muy severa

datos_fleiss <- data.frame(
  obs1  = c(0, 1, 2, 3, 4, 5, 0, 0, 1, 2, 3, 4, 5, 0, 0, 5, 2, 0, 4, 0, 3, 4, 5, 5, 5),
  obs2  = c(0, 1, 2, 3, 4, 5, 0, 0, 1, 2, 3, 4, 5, 0, 0, 5, 2, 0, 5, 0, 3, 4, 5, 5, 5),
  obs3  = c(0, 1, 2, 3, 4, 5, 0, 0, 1, 2, 3, 4, 5, 0, 0, 5, 2, 0, 5, 0, 4, 4, 5, 5, 5),
  obs4  = c(0, 1, 2, 3, 4, 5, 0, 0, 1, 2, 3, 4, 5, 0, 0, 5, 2, 0, 4, 0, 3, 4, 5, 5, 5),
  obs5  = c(0, 1, 2, 3, 4, 5, 0, 0, 1, 2, 3, 4, 5, 0, 0, 5, 2, 0, 4, 0, 3, 4, 5, 5, 5),
  obs6  = c(0, 1, 2, 3, 4, 5, 0, 0, 1, 2, 3, 4, 5, 0, 0, 5, 2, 0, 5, 0, 3, 4, 5, 5, 5),
  obs7  = c(0, 1, 2, 3, 4, 5, 0, 0, 1, 2, 3, 4, 5, 0, 0, 5, 2, 0, 5, 0, 4, 4, 5, 5, 5),
  obs8  = c(1, 1, 2, 3, 4, 5, 0, 0, 1, 2, 3, 4, 5, 0, 0, 5, 2, 0, 5, 0, 4, 4, 5, 5, 5),
  obs9  = c(0, 1, 2, 3, 4, 5, 0, 0, 1, 2, 3, 4, 5, 0, 0, 5, 2, 0, 5, 0, 4, 4, 5, 5, 5),
  obs10 = c(0, 1, 2, 3, 4, 5, 0, 0, 1, 2, 3, 4, 5, 0, 0, 5, 2, 0, 5, 0, 4, 4, 5, 5, 5)
)

# ------------------------------------------------------------------------------
# 4.2 Kappa de Fleiss
# ------------------------------------------------------------------------------
kappa_fleiss <- kappam.fleiss(datos_fleiss)
print(kappa_fleiss)

# ------------------------------------------------------------------------------
# 4.3 Intervalo de confianza por bootstrap
# ------------------------------------------------------------------------------
set.seed(123)

# Función para bootstrap
fleiss_boot <- function(data, indices) {
  d <- data[indices, ]
  k <- kappam.fleiss(d)
  return(k$value)
}

# Ejecutar bootstrap (1000 repeticiones)
boot_fleiss <- boot(datos_fleiss, statistic = fleiss_boot, R = 1000)

# IC 95%
boot.ci(boot_fleiss, type = "perc")

# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================