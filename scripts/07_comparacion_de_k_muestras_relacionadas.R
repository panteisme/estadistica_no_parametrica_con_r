# ==============================================================================
# Curso de Estadística no paramétrica con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Tema 7: Pruebas no paramétricas para más de dos muestras relacionadas
# ==============================================================================

# Cargar librerías necesarias
library(dplyr)
library(forcats)
library(ggplot2)
library(rstatix)
library(ggstatsplot)
library(tidyr)

# ==============================================================================
# 1. DATOS
# ==============================================================================

EVA_postoperatorio_inmediato <- c(4, 5, 3, 4, 5, 4, 4, 4, 5, 4, 3, 3, 4, 3, 4, 3, 3, 3, 4, 3, 3, 4, 4, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 4, 4, 4, 3, 4, 4, 5, 5, 3, 5, 5, 4, 5, 5, 5, 5, 4, 5, 4, 4, 3, 3, 3, 4, 4, 5, 4, 4, 4, 5, 4, 3, 5, 5, 4, 4, 5, 5, 5, 4, 4, 5, 4, 3, 4, 4, 4, 3, 4, 4, 3, 3, 4)

EVA_6_horas <- c(2, 2, 1, 2, 3, 3, 3, 1, 3, 1, 1, 2, 2, 3, 3, 1, 3, 2, 2, 1, 2, 1, 1, 2, 3, 2, 4, 2, 2, 3, 2, 2, 2, 2, 1, 3, 3, 3, 1, 2, 2, 3, 2, 2, 3, 3, 2, 3, 3, 3, 1, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, 3, 3, 2, 3, 1, 3, 2, 1, 2, 3, 3, 3, 2, 3, 2, 3, 1, 3, 2, 3, 2, 2, 3, 2, 1, 3)

EVA_12_horas <- c(2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 2, 2, 2, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1, 3, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 2, 1, 1, 1, 1, 1)

tipo_de_artroscopia <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

sexo <- c("femenino", "masculino", "masculino", "masculino", "masculino", "masculino", "femenino", "femenino", "masculino", "femenino", "femenino", "femenino", "masculino", "masculino", "femenino", "femenino", "femenino", "femenino", "masculino", "masculino", "masculino", "femenino", "femenino", "femenino", "femenino", "femenino", "masculino", "femenino", "femenino", "masculino", "masculino", "femenino", "masculino", "femenino", "masculino", "femenino", "masculino", "masculino", "masculino", "femenino", "femenino", "masculino", "femenino", "femenino", "femenino", "femenino", "femenino", "masculino", "masculino", "femenino", "femenino", "masculino", "masculino", "femenino", "femenino", "femenino", "masculino", "femenino", "masculino", "masculino", "masculino", "femenino", "masculino", "masculino", "masculino", "femenino", "masculino", "femenino", "masculino", "femenino", "femenino", "femenino", "femenino", "masculino", "femenino", "masculino", "masculino", "masculino", "femenino", "femenino", "femenino", "masculino", "masculino", "masculino", "masculino", "femenino", "masculino", "femenino")

# Crear tibble
data_artroscopia <- tibble(
  EVA_postoperatorio = EVA_postoperatorio_inmediato,
  EVA_6h = EVA_6_horas,
  EVA_12h = EVA_12_horas,
  tipo_artroscopia = tipo_de_artroscopia,
  sexo = sexo
) %>%
  mutate(
    tipo_artroscopia = fct_recode(
      factor(tipo_artroscopia),
      "Abierta" = "0",
      "Cerrada" = "1"
    ),
    sexo = factor(sexo)
  )

# Verificar estructura
glimpse(data_artroscopia)

# ==============================================================================

# ==============================================================================
# 2. EXPLORACIÓN DE DATOS
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 Preparar datos en formato largo
# ------------------------------------------------------------------------------

data_artroscopia_largo <- data_artroscopia %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = c(EVA_postoperatorio, EVA_6h, EVA_12h),
    names_to = "tiempo_postoperatorio",
    values_to = "EVA"
  ) %>%
  mutate(
    tiempo_postoperatorio = factor(
      tiempo_postoperatorio,
      levels = c("EVA_postoperatorio", "EVA_6h", "EVA_12h"),
      labels = c("Postoperatorio inmediato", "6 horas", "12 horas")
    )
  )

# ------------------------------------------------------------------------------
# 2.2 Estadísticos descriptivos por tiempo
# ------------------------------------------------------------------------------

data_artroscopia_largo %>%
  group_by(tiempo_postoperatorio) %>%
  summarize(
    n = n(),
    Mediana = median(EVA),
    Q1 = quantile(EVA, probs = 0.25),
    Q3 = quantile(EVA, probs = 0.75),
    Media = mean(EVA),
    DE = sd(EVA)
  )

# ------------------------------------------------------------------------------
# 2.3 Visualización exploratoria
# ------------------------------------------------------------------------------

data_artroscopia_largo %>%
  ggplot(aes(x = tiempo_postoperatorio, y = EVA, fill = tiempo_postoperatorio)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  scale_fill_manual(values = c(
    "Postoperatorio inmediato" = "#e74c3c",
    "6 horas" = "#f39c12",
    "12 horas" = "#27ae60"
  )) +
  labs(
    title = "Evolución del Dolor Postoperatorio",
    subtitle = "Escala Visual Análoga (EVA)",
    x = "Tiempo Postoperatorio",
    y = "EVA"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ==============================================================================
# 3. PRUEBA DE FRIEDMAN
# ==============================================================================

# Ho: La mediana de las puntuaciones EVA no varía según el tiempo postoperatorio
# Ha: La mediana de las puntuaciones EVA varía según el tiempo postoperatorio

# ------------------------------------------------------------------------------
# 3.1 Prueba de Friedman con rstatix
# ------------------------------------------------------------------------------

data_artroscopia_largo %>%
  friedman_test(EVA ~ tiempo_postoperatorio | id)

# Tamaño del efecto
data_artroscopia_largo %>%
  friedman_effsize(EVA ~ tiempo_postoperatorio | id)

# ------------------------------------------------------------------------------
# 3.2 Prueba de Friedman con base R
# ------------------------------------------------------------------------------

friedman.test(EVA ~ tiempo_postoperatorio | id, data = data_artroscopia_largo)

# ------------------------------------------------------------------------------
# 3.3 Gráfica con ggstatsplot
# ------------------------------------------------------------------------------

data_artroscopia_largo %>% 
  ggwithinstats(
    x = tiempo_postoperatorio,
    y = EVA,
    xlab = "Tiempo Postoperatorio",
    ylab = "EVA",
    type = "nonparametric",
    title = "Prueba de Friedman: Evolución del Dolor Postoperatorio"
  )

# ==============================================================================
# 4. COMPARACIONES MÚLTIPLES POST-HOC
# ==============================================================================

# ------------------------------------------------------------------------------
# 4.1 Prueba de Wilcoxon pareada con corrección de Holm
# ------------------------------------------------------------------------------

pairwise.wilcox.test(
  data_artroscopia_largo$EVA,
  data_artroscopia_largo$tiempo_postoperatorio,
  paired = TRUE,
  p.adjust.method = "holm"
)

# ------------------------------------------------------------------------------
# 4.2 Comparaciones pareadas con rstatix
# ------------------------------------------------------------------------------

data_artroscopia_largo %>%
  wilcox_test(EVA ~ tiempo_postoperatorio, paired = TRUE, p.adjust.method = "holm")

# ==============================================================================
# 5. PRUEBA DE PAGE (TEST DE TENDENCIA ORDENADA)
# ==============================================================================

# Evalúa si existe un patrón ordenado monotónico en los rangos
# Más específica que Friedman cuando hay una hipótesis direccional a priori

# ------------------------------------------------------------------------------
# 5.1 Hipótesis a priori
# ------------------------------------------------------------------------------

# Hipótesis esperada: EVA_postoperatorio > EVA_6h > EVA_12h
# (El dolor disminuye progresivamente con el tiempo)

# Ho: m₁ = m₂ = m₃ (no hay diferencias en los rangos medios)
# Ha: m₁ ≥ m₂ ≥ m₃ (patrón ordenado decreciente, al menos una desigualdad estricta)

# ------------------------------------------------------------------------------
# 5.2 Prueba de Page
# ------------------------------------------------------------------------------

library(crank)

# Preparar matriz de datos (cada fila = paciente, cada columna = tiempo)
matriz_eva <- data_artroscopia %>%
  select(EVA_postoperatorio, EVA_6h, EVA_12h) %>%
  as.matrix()

# Prueba de Page
page.trend.test(matriz_eva)

# ------------------------------------------------------------------------------
# 5.3 Visualización del patrón ordenado
# ------------------------------------------------------------------------------

data_artroscopia_largo %>%
  group_by(tiempo_postoperatorio) %>%
  summarize(
    Mediana = median(EVA),
    Media_Rangos = mean(rank(EVA))
  ) %>%
  ggplot(aes(x = tiempo_postoperatorio, y = Mediana, group = 1)) +
  geom_line(color = "#e74c3c", linewidth = 1.2) +
  geom_point(size = 4, color = "#e74c3c") +
  labs(
    title = "Tendencia Ordenada del Dolor Postoperatorio",
    subtitle = "Prueba de Page",
    x = "Tiempo Postoperatorio",
    y = "Mediana EVA"
  ) +
  theme_minimal()

# ==============================================================================
# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================