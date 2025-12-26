# ==============================================================================
# Curso de Estadística no paramétrica con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Tema 11: Pruebas de muestras relacionadas para variables categóricas
# ==============================================================================

# Cargar librerías necesarias
library(dplyr)
library(forcats)
library(ggplot2)
library(rstatix)
library(janitor)
library(vegan)
library(tidyr)
library(ggstatsplot)
library(RVAideMemoire)

# ==============================================================================
# 1. PRUEBA DE McNEMAR (2 mediciones, variable dicotómica)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1.1 Datos
# ------------------------------------------------------------------------------

Antes <- c(1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 
           1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0,
           0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 
           1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 
           1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 
           0, 0, 1, 1, 0, 1)

Despues <- c(0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 
             0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
             1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1,
             0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 
             1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0,
             1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

# Crear tibble con factores
data_mcnemar <- tibble(
  antes = Antes,
  despues = Despues
) %>%
  mutate(
    antes = fct_recode(
      factor(antes),
      "No" = "0",
      "Si" = "1"
    ),
    despues = fct_recode(
      factor(despues),
      "No" = "0",
      "Si" = "1"
    )
  )

# Verificar estructura
glimpse(data_mcnemar)

# ------------------------------------------------------------------------------
# 1.2 Tabla de contingencia
# ------------------------------------------------------------------------------

data_mcnemar %>%
  tabyl(antes, despues) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

Tabla <- data_mcnemar %>%
  tabyl(antes, despues) %>%
  select(-antes) %>%
  as.matrix()

# ------------------------------------------------------------------------------
# 1.3 Prueba de McNemar
# ------------------------------------------------------------------------------

# Ho: La diferencia entre la proporción de pacientes antes y después es igual a cero
# Ha: La diferencia entre la proporción de pacientes antes y después es diferente de cero

# Con rstatix
mcnemar_test(Tabla)

# Con base R
prop.table(table(data_mcnemar$antes, data_mcnemar$despues), 1)
mcnemar.test(table(data_mcnemar$antes, data_mcnemar$despues))

# ==============================================================================
# 2. Q DE COCHRAN (≥3 mediciones, variable dicotómica)
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 Datos
# ------------------------------------------------------------------------------

paciente <- c(1:15)
pre <- c(0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0)
post1 <- c(0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0)
post2 <- c(0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0)
post3 <- c(0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0)

# Crear tibble
data_cochran <- tibble(
  paciente = paciente,
  pre = pre,
  post1 = post1,
  post2 = post2,
  post3 = post3
)

# Verificar estructura
glimpse(data_cochran)

# ------------------------------------------------------------------------------
# 2.2 Formato largo
# ------------------------------------------------------------------------------

data_cochran_largo <- data_cochran %>%
  pivot_longer(cols = c(pre, post1, post2, post3),
               names_to = "tratamiento",
               values_to = "efectos_adversos") %>%
  mutate(
    efectos_adversos = fct_recode(
      factor(efectos_adversos),
      "No" = "0",
      "Si" = "1"
    ),
    tratamiento = factor(tratamiento, levels = c("pre", "post1", "post2", "post3"))
  )

# ------------------------------------------------------------------------------
# 2.3 Q de Cochran
# ------------------------------------------------------------------------------

# Con rstatix
data_cochran_largo %>%
  cochran_qtest(efectos_adversos ~ tratamiento | paciente)

# Con RVAideMemoire
cochran.qtest(efectos_adversos ~ tratamiento | paciente, data = data_cochran_largo)

# ==============================================================================
# 3. W DE KENDALL (Concordancia entre ≥3 jueces, variable ordinal)
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1 Datos
# ------------------------------------------------------------------------------

juez1 <- c(1, 3, 4, 5, 4, 4, 3, 2, 4, 1)
juez2 <- c(1, 3, 3, 4, 4, 4, 4, 2, 4, 2)
juez3 <- c(1, 2, 4, 5, 5, 5, 3, 2, 3, 2)

# Crear tibble
data_kendall <- tibble(
  juez1 = juez1,
  juez2 = juez2,
  juez3 = juez3
)

# Verificar estructura
glimpse(data_kendall)

# ------------------------------------------------------------------------------
# 3.2 Visualización
# ------------------------------------------------------------------------------

data_kendall %>%
  pivot_longer(cols = c(juez1, juez2, juez3),
               names_to = "juez",
               values_to = "puntuacion_likert") %>%
  ggplot(aes(x = juez, y = puntuacion_likert, fill = juez)) + 
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_fill_manual(values = c("juez1" = "#3498db", "juez2" = "#e74c3c", "juez3" = "#27ae60")) +
  labs(
    title = "Puntuaciones por Juez",
    x = "Juez",
    y = "Puntuación Likert"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ------------------------------------------------------------------------------
# 3.3 W de Kendall
# ------------------------------------------------------------------------------

# Transponer datos (filas = ítems, columnas = jueces)
kendall.global(t(data_kendall))

# ------------------------------------------------------------------------------
# 3.4 Gráfica con ggstatsplot
# ------------------------------------------------------------------------------

data_kendall %>%
  pivot_longer(cols = c(juez1, juez2, juez3),
               names_to = "juez",
               values_to = "puntuacion_likert") %>%
  ggwithinstats(
    x = juez,
    y = puntuacion_likert,
    xlab = "Juez",
    ylab = "Puntuación Likert",
    type = "nonparametric",
    results.subtitle = FALSE,
    bf.message = FALSE,
    title = "W de Kendall: Concordancia entre Jueces"
  )

# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================