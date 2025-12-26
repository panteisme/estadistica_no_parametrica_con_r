# ==============================================================================
# Curso de Estadística no paramétrica con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Tema 12: Tablas de contingencia estratificadas
# ==============================================================================

# Cargar librerías necesarias
library(dplyr)
library(forcats)
library(ggplot2)
library(rstatix)
library(janitor)
library(ggstatsplot)

# ==============================================================================
# 1. DATOS
# ==============================================================================

# Vector madre_lee_y_escribe (n = 446)
madre_lee_escribe <- c(0,1,1,1,1,1,1,1,0,1,1,1,1,0,1,0,1,1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,0,0,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,0,1,0,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,0,1,1,1,1,0,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,0,0,1,0,0,1,1,1,1,0,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,0,1,1,0,0,1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,1,0,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,0,1,0,1,0,1,0,1,1,1,1,0,1,1,0,0,1,1,1,0,0,1,1,1,0,1,1,1,0,1,1,1,1,0,1,1,1,1,0,0,1,1,1,1,1,0,1,0,1,1,1,1,1,0,1,0,1,1,1,0,1,1,1,1,1,1,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,0)

# Vector desnutricion_severa (n = 446)
desnutricion_severa <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,0,0,1,0,0,0)

# Vector tipo_lactancia (n = 446)
tipo_lactancia <- c(1,1,1,2,2,3,2,1,1,1,2,2,1,1,3,1,1,1,1,3,2,1,1,1,3,2,1,2,1,1,1,1,1,2,1,1,1,1,1,1,2,1,1,1,1,2,1,1,3,3,1,2,2,1,2,1,1,2,2,1,1,1,1,1,1,1,1,1,1,2,2,2,1,1,1,2,2,1,1,3,1,1,1,1,1,2,3,1,1,1,1,1,3,1,2,1,1,1,1,1,1,1,1,2,1,1,1,1,2,1,2,1,1,1,1,1,3,1,2,2,1,2,1,1,3,1,1,1,1,1,1,3,1,1,1,2,1,1,1,2,1,2,1,3,3,3,3,2,3,1,2,3,1,2,1,1,1,1,1,1,1,1,1,1,2,2,1,1,1,3,3,1,1,1,1,1,1,1,1,1,2,1,2,1,1,1,1,1,2,3,1,3,3,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,2,3,2,1,1,2,1,1,1,2,1,1,2,2,2,1,1,1,2,1,2,2,1,1,1,1,3,1,1,1,1,2,1,1,1,1,1,1,1,1,2,1,1,1,3,1,2,1,3,3,1,1,2,1,1,1,3,1,2,2,1,1,1,1,1,2,1,2,1,1,3,3,1,1,2,1,1,1,3,1,2,1,1,1,1,1,1,1,2,2,1,3,2,3,3,2,1,2,3,2,1,2,2,1,2,1,2,1,2,2,3,2,2,3,1,1,1,2,2,3,3,1,2,2,1,1,2,1,2,1,3,2,1,1,2,2,2,1,1,1,1,2,1,1,1,3,2,2,1,3,1,1,1,1,1,1,1,1,1,1,3,3,1,1,2,2,3,2,1,1,3,3,2,2,1,2,2,1,1,1,1,1,2,1,3,3,1,2,1,1,2,1,2,3,3,2,1,2,1,1,2,2,1,1,1,1,1,1,1,1,1,1,2,3,2,2,3,2,2,2,3,1,3,2,2,2,2,1,3,1,1,3,1,3,1,1,2)

# Crear tibble
data_lactancia <- tibble(
  tipo_lactancia = tipo_lactancia,
  desnutricion = desnutricion_severa,
  madre_alfabeta = madre_lee_escribe
) %>%
  mutate(
    tipo_lactancia = fct_recode(
      factor(tipo_lactancia),
      "Materna" = "1",
      "Mixta" = "2",
      "Formula" = "3"
    ),
    desnutricion = fct_recode(
      factor(desnutricion),
      "No" = "0",
      "Si" = "1"
    ),
    madre_alfabeta = fct_recode(
      factor(madre_alfabeta),
      "No" = "0",
      "Si" = "1"
    )
  )

# Verificar estructura
glimpse(data_lactancia)

# ==============================================================================
# 2. EXPLORACIÓN DE DATOS
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 Tabla de contingencia estratificada
# ------------------------------------------------------------------------------

# Tabla de frecuencias con xtabs
tabla_estratificada <- xtabs(~ tipo_lactancia + desnutricion + madre_alfabeta, 
                             data = data_lactancia)

# Visualizar tabla plana
ftable(tabla_estratificada)

# ------------------------------------------------------------------------------
# 2.2 Tablas por estratos
# ------------------------------------------------------------------------------

# Estrato: Madres que NO saben leer y escribir
data_no_alfabeta <- data_lactancia %>% 
  filter(madre_alfabeta == "No")

tabla_no_alfabeta <- table(data_no_alfabeta$tipo_lactancia, 
                           data_no_alfabeta$desnutricion)
addmargins(tabla_no_alfabeta)

# Estrato: Madres que SÍ saben leer y escribir
data_alfabeta <- data_lactancia %>% 
  filter(madre_alfabeta == "Si")

tabla_alfabeta <- table(data_alfabeta$tipo_lactancia, 
                        data_alfabeta$desnutricion)
addmargins(tabla_alfabeta)

# ------------------------------------------------------------------------------
# 2.3 Visualización con ggplot2
# ------------------------------------------------------------------------------

data_lactancia %>%
  ggplot(aes(x = tipo_lactancia, fill = desnutricion)) + 
  facet_wrap(~ madre_alfabeta) +  
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("No" = "#3498db", "Si" = "#e74c3c")) +
  labs(
    title = "Desnutrición Severa según Tipo de Lactancia",
    subtitle = "Estratificado por Alfabetización Materna",
    x = "Tipo de Lactancia", 
    y = "Proporción",
    fill = "Desnutrición Severa"
  ) +
  theme_minimal()

# ==============================================================================
# 3. PRUEBA DE MANTEL-HAENSZEL
# ==============================================================================

# Ho: La proporción de desnutrición severa no varía según tipo de lactancia
#     cuando se estratifica por alfabetización materna
# Ha: La proporción de desnutrición severa varía según tipo de lactancia
#     cuando se estratifica por alfabetización materna

# ------------------------------------------------------------------------------
# 3.1 Crear array tridimensional
# ------------------------------------------------------------------------------

# Convertir tablas a matrices
matriz_no_alfabeta <- as.matrix(tabla_no_alfabeta)
matriz_alfabeta <- as.matrix(tabla_alfabeta)

# Crear array 3D (filas, columnas, estratos)
array_estratificado <- array(c(matriz_no_alfabeta, matriz_alfabeta),
                             dim = c(3, 2, 2),
                             dimnames = list(
                               "Tipo_lactancia" = c("Materna", "Mixta", "Formula"),
                               "Desnutricion" = c("No", "Si"),
                               "Alfabeta" = c("No", "Si")
                             ))

# Visualizar array
array_estratificado

# ------------------------------------------------------------------------------
# 3.2 Prueba de Mantel-Haenszel
# ------------------------------------------------------------------------------

mantelhaen.test(array_estratificado, correct = TRUE)

# ------------------------------------------------------------------------------
# 3.3 Resumen de la tabla estratificada
# ------------------------------------------------------------------------------

summary(tabla_estratificada)

# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================