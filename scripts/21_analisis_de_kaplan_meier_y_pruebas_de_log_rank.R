# ==============================================================================
# Curso de Estadística no paramétrica con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Tema 21: Análisis de Kaplan–Meier y pruebas de log-rank
# ==============================================================================

# Cargar librerías necesarias
library(tidyverse)
library(survival)
library(survminer)
library(ggfortify) 
library(plotly) 
library(gridExtra)

# ==============================================================================
# 1. DATOS DEL ESTUDIO
# ==============================================================================

datos <- tibble(
  trr = c("hd", "dp", "hd", "dp", "hd", "dp", "hd", "hd", "dp", "tra", 
          "hd", "hd", "dp", "hd", "tra", "dp", "hd", "dp", "hd", "dp", 
          "hd", "dp", "hd", "hd", "hd", "hd", "hd", "hd", "hd", "dp", 
          "hd", "hd", "hd", "dp", "hd", "hd", "hd", "tra", "hd", "hd", 
          "dp", "dp", "dp", "hd", "hd", "tra", "hd", "dp", "hd", "dp", 
          "hd", "hd", "hd", "hd", "hd", "hd", "dp", "hd", "hd", "hd", 
          "hd", "hd", "hd", "hd", "hd", "hd", "hd", "dp", "dp", "hd", 
          "hd", "hd", "dp", "hd", "tra", "hd", "hd", "dp", "dp", "hd", 
          "dp", "hd", "dp", "dp", "hd", "dp", "hd", "tra", "dp", "hd", 
          "dp", "hd", "dp", "hd", "dp", "hd", "dp", "hd", "hd", "dp", 
          "hd", "dp", "hd", "hd", "dp", "dp", "hd", "hd", "hd", "hd", 
          "hd", "hd", "hd", "hd", "hd", "dp", "hd", "hd", "hd", "hd", 
          "dp", "hd", "hd", "hd", "hd", "dp", "hd", "hd", "hd", "hd", 
          "hd", "hd", "hd", "hd", "dp", "dp", "tra", "hd", "hd", "hd", 
          "hd", "hd", "hd", "dp", "dp", "hd", "dp", "hd", "dp", "hd", 
          "hd", "dp", "hd", "hd", "hd", "hd", "dp", "hd", "dp", "hd", 
          "dp", "dp", "tra", "hd", "dp", "hd", "hd", "hd", "hd", "dp", 
          "tra", "hd", "dp", "dp", "dp", "dp", "tra", "hd", "hd", "dp", 
          "hd", "dp", "tra", "hd", "hd", "tra", "hd", "tra", "hd", "hd", 
          "dp", "hd", "hd", "dp", "hd", "dp", "hd", "dp", "dp", "tra", 
          "tra", "hd", "dp", "hd", "dp", "hd", "hd", "dp", "tra", "dp", 
          "hd", "tra", "dp", "dp", "tra", "dp", "hd", "hd", "dp", "hd", 
          "hd", "dp", "hd", "dp", "hd", "hd", "dp", "tra", "dp", "hd", 
          "hd", "hd", "dp", "hd", "dp", "dp", "hd", "dp", "tra", "tra", 
          "hd", "hd", "dp", "hd", "hd", "dp", "dp", "hd", "tra", "dp", 
          "hd", "hd", "hd", "hd", "hd", "hd", "hd", "hd", "hd", "tra", 
          "dp", "hd", "dp", "hd", "dp", "hd", "hd", "dp", "tra", "hd", 
          "hd", "tra", "dp", "hd", "dp", "hd", "dp", "hd", "dp", "tra", 
          "dp", "hd", "hd", "dp", "hd", "dp", "hd", "dp", "hd", "hd", 
          "hd", "dp", "tra", "hd", "dp", "hd", "tra", "hd", "hd", "dp", 
          "dp", "hd", "hd", "dp", "hd", "dp", "hd", "dp", "dp", "tra", 
          "hd", "dp", "hd", "hd", "dp", "hd", "tra", "dp", "hd", "dp", 
          "dp", "hd", "tra", "hd", "dp", "hd", "dp", "hd", "dp", "hd", 
          "dp", "hd", "dp", "dp", "tra", "dp", "tra", "hd", "dp", "tra", 
          "hd", "dp", "hd", "hd", "dp", "dp", "hd", "hd", "dp", "dp", 
          "hd", "dp", "hd", "dp", "tra", "dp", "hd", "dp", "dp", "dp", 
          "tra", "tra", "hd", "hd", "tra", "dp", "hd", "dp", "dp", "tra"),
  
  sexo = c("m", "m", "m", "m", "m", "f", "m", "m", "m", "m", 
           "f", "f", "f", "f", "m", "f", "f", "f", "m", "m", 
           "m", "m", "m", "m", "f", "f", "f", "f", "m", "m", 
           "m", "m", "m", "f", "m", "f", "m", "m", "m", "f", 
           "f", "f", "f", "f", "m", "m", "f", "f", "f", "f", 
           "f", "m", "m", "m", "m", "m", "m", "m", "f", "m", 
           "f", "f", "f", "f", "m", "f", "f", "f", "f", "m", 
           "m", "m", "m", "m", "m", "m", "f", "f", "f", "f", 
           "m", "m", "m", "f", "m", "m", "m", "m", "m", "m", 
           "m", "m", "m", "m", "f", "f", "f", "m", "m", "m", 
           "f", "f", "m", "f", "f", "f", "f", "f", "f", "f", 
           "m", "m", "f", "f", "m", "m", "m", "m", "m", "m", 
           "f", "m", "m", "m", "m", "m", "m", "m", "m", "m", 
           "m", "m", "m", "f", "f", "f", "f", "f", "f", "f", 
           "m", "m", "f", "f", "f", "f", "f", "f", "f", "m", 
           "f", "f", "f", "f", "f", "f", "f", "f", "f", "f", 
           "m", "m", "f", "m", "m", "m", "m", "m", "m", "f", 
           "f", "f", "m", "m", "m", "m", "m", "m", "m", "m", 
           "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", 
           "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", 
           "m", "m", "m", "f", "m", "f", "m", "m", "m", "m", 
           "m", "m", "f", "m", "m", "m", "m", "f", "f", "f", 
           "f", "f", "f", "f", "m", "m", "f", "f", "f", "f", 
           "f", "f", "f", "f", "f", "f", "f", "f", "f", "m", 
           "m", "m", "f", "m", "m", "f", "f", "m", "f", "f", 
           "m", "m", "f", "f", "f", "f", "f", "f", "f", "m", 
           "f", "f", "f", "f", "f", "f", "f", "f", "f", "f", 
           "f", "f", "f", "f", "f", "f", "f", "m", "m", "f", 
           "m", "m", "m", "f", "f", "f", "m", "m", "m", "m", 
           "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", 
           "m", "m", "m", "f", "f", "f", "f", "m", "m", "f", 
           "f", "f", "f", "m", "m", "m", "m", "f", "f", "f", 
           "f", "m", "m", "m", "f", "m", "f", "f", "f", "f", 
           "f", "f", "f", "f", "f", "f", "f", "f", "f", "f", 
           "f", "f", "f", "f", "m", "m", "f", "f", "f", "m", 
           "m", "f", "f", "f", "f", "m", "m", "m", "f", "m", 
           "m", "m", "f", "m", "m", "m", "m", "f", "f", "f"),
  
  edad_rangos = c("5 a 9", "5 a 9", "5 a 9", "5 a 9", "5 a 9", 
                  "0 a 4", "10 a 14", "10 a 14", "10 a 14", "15 a 18", 
                  "5 a 9", "10 a 14", "15 a 18", "15 a 18", "5 a 9", 
                  "15 a 18", "15 a 18", "15 a 18", "10 a 14", "10 a 14", 
                  "5 a 9", "5 a 9", "5 a 9", "0 a 4", "10 a 14", 
                  "10 a 14", "10 a 14", "15 a 18", "10 a 14", "10 a 14", 
                  "10 a 14", "10 a 14", "10 a 14", "10 a 14", "10 a 14", 
                  "15 a 18", "15 a 18", "15 a 18", "15 a 18", "10 a 14", 
                  "10 a 14", "15 a 18", "15 a 18", "15 a 18", "15 a 18", 
                  "15 a 18", "5 a 9", "5 a 9", "10 a 14", "10 a 14", 
                  "10 a 14", "10 a 14", "15 a 18", "15 a 18", "5 a 9", 
                  "5 a 9", "15 a 18", "5 a 9", "15 a 18", "10 a 14", 
                  "10 a 14", "10 a 14", "10 a 14", "10 a 14", "10 a 14", 
                  "15 a 18", "10 a 14", "10 a 14", "15 a 18", "10 a 14", 
                  "10 a 14", "5 a 9", "10 a 14", "10 a 14", "10 a 14", 
                  "10 a 14", "15 a 18", "15 a 18", "15 a 18", "15 a 18", 
                  "10 a 14", "10 a 14", "10 a 14", "10 a 14", "10 a 14", 
                  "5 a 9", "15 a 18", "15 a 18", "15 a 18", "10 a 14", 
                  "10 a 14", "10 a 14", "15 a 18", "10 a 14", "15 a 18", 
                  "15 a 18", "10 a 14", "5 a 9", "15 a 18", "15 a 18", 
                  "10 a 14", "10 a 14", "10 a 14", "10 a 14", "10 a 14", 
                  "15 a 18", "10 a 14", "10 a 14", "5 a 9", "5 a 9", 
                  "10 a 14", "5 a 9", "10 a 14", "15 a 18", "10 a 14", 
                  "10 a 14", "10 a 14", "10 a 14", "10 a 14", "10 a 14", 
                  "15 a 18", "10 a 14", "15 a 18", "15 a 18", "15 a 18", 
                  "10 a 14", "15 a 18", "15 a 18", "15 a 18", "15 a 18", 
                  "15 a 18", "15 a 18", "15 a 18", "10 a 14", "10 a 14", 
                  "15 a 18", "15 a 18", "10 a 14", "5 a 9", "5 a 9", 
                  "15 a 18", "10 a 14", "5 a 9", "5 a 9", "15 a 18", 
                  "15 a 18", "15 a 18", "10 a 14", "15 a 18", "10 a 14", 
                  "15 a 18", "15 a 18", "15 a 18", "10 a 14", "15 a 18", 
                  "15 a 18", "15 a 18", "10 a 14", "10 a 14", "10 a 14", 
                  "15 a 18", "10 a 14", "15 a 18", "5 a 9", "5 a 9", 
                  "10 a 14", "10 a 14", "10 a 14", "10 a 14", "10 a 14", 
                  "5 a 9", "10 a 14", "10 a 14", "10 a 14", "10 a 14", 
                  "15 a 18", "15 a 18", "5 a 9", "5 a 9", "10 a 14", 
                  "10 a 14", "10 a 14", "10 a 14", "10 a 14", "5 a 9", 
                  "10 a 14", "15 a 18", "5 a 9", "5 a 9", "15 a 18", 
                  "15 a 18", "10 a 14", "10 a 14", "10 a 14", "15 a 18", 
                  "10 a 14", "10 a 14", "10 a 14", "15 a 18", "10 a 14", 
                  "10 a 14", "15 a 18", "10 a 14", "10 a 14", "5 a 9", 
                  "5 a 9", "10 a 14", "10 a 14", "10 a 14", "10 a 14", 
                  "5 a 9", "10 a 14", "15 a 18", "15 a 18", "10 a 14", 
                  "15 a 18", "15 a 18", "10 a 14", "5 a 9", "10 a 14", 
                  "15 a 18", "5 a 9", "5 a 9", "5 a 9", "5 a 9", 
                  "10 a 14", "10 a 14", "15 a 18", "15 a 18", "15 a 18", 
                  "10 a 14", "10 a 14", "10 a 14", "15 a 18", "15 a 18", 
                  "15 a 18", "10 a 14", "10 a 14", "10 a 14", "10 a 14", 
                  "10 a 14", "10 a 14", "15 a 18", "10 a 14", "5 a 9", 
                  "5 a 9", "15 a 18", "15 a 18", "15 a 18", "15 a 18", 
                  "10 a 14", "15 a 18", "10 a 14", "10 a 14", "10 a 14", 
                  "10 a 14", "10 a 14", "5 a 9", "15 a 18", "15 a 18", 
                  "5 a 9", "10 a 14", "10 a 14", "10 a 14", "10 a 14", 
                  "15 a 18", "10 a 14", "10 a 14", "10 a 14", "10 a 14", 
                  "15 a 18", "15 a 18", "15 a 18", "15 a 18", "5 a 9", 
                  "10 a 14", "10 a 14", "15 a 18", "15 a 18", "15 a 18", 
                  "15 a 18", "15 a 18", "10 a 14", "15 a 18", "10 a 14", 
                  "10 a 14", "10 a 14", "10 a 14", "10 a 14", "15 a 18", 
                  "10 a 14", "15 a 18", "10 a 14", "10 a 14", "15 a 18", 
                  "15 a 18", "15 a 18", "10 a 14", "10 a 14", "15 a 18", 
                  "15 a 18", "0 a 4", "0 a 4", "5 a 9", "0 a 4", 
                  "10 a 14", "10 a 14", "15 a 18", "15 a 18", "15 a 18", 
                  "10 a 14", "15 a 18", "15 a 18", "10 a 14", "10 a 14", 
                  "15 a 18", "15 a 18", "10 a 14", "10 a 14", "15 a 18", 
                  "15 a 18", "10 a 14", "5 a 9", "5 a 9", "10 a 14", 
                  "5 a 9", "5 a 9", "5 a 9", "5 a 9", "10 a 14", 
                  "10 a 14", "10 a 14", "10 a 14", "10 a 14", "10 a 14", 
                  "15 a 18", "15 a 18", "15 a 18", "5 a 9", "15 a 18", 
                  "15 a 18", "15 a 18", "10 a 14", "10 a 14", "10 a 14", 
                  "15 a 18", "10 a 14", "15 a 18", "10 a 14", "10 a 14", 
                  "10 a 14", "15 a 18", "10 a 14", "15 a 18", "10 a 14", 
                  "10 a 14", "10 a 14", "15 a 18", "15 a 18", "5 a 9", 
                  "5 a 9", "10 a 14", "5 a 9", "5 a 9", "5 a 9", 
                  "5 a 9", "10 a 14", "10 a 14", "10 a 14", "10 a 14"),
  
  tipo_evento = c(
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Fallecido", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Fallecido", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Fallecido", "Censurado", "Censurado", "Fallecido", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Fallecido", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Fallecido", "Censurado", "Fallecido", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Fallecido", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Fallecido", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Fallecido", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Fallecido", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Fallecido", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Fallecido", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Fallecido", "Censurado", "Fallecido", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Fallecido", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Fallecido", "Censurado", "Censurado", "Censurado", "Fallecido", "Censurado", "Censurado", "Fallecido", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Fallecido", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Fallecido", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Fallecido", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Fallecido", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Fallecido", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Fallecido", "Censurado", "Censurado", "Censurado", "Censurado", "Fallecido", "Censurado", 
    "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado", "Censurado"
  ),
  
  tiempo_meses = c(3, 2, 9, 15, 11, 4, 2, 3, 14, 26, 
             4, 19, 36, 36, 4, 23, 2, 33, 2, 24, 
             2, 4, 5, 2, 36, 1, 1, 1, 2, 4, 
             5, 5, 6, 36, 4, 4, 5, 21, 11, 2, 
             6, 11, 22, 3, 2, 34, 5, 14, 3, 5, 
             27, 3, 3, 3, 1, 3, 13, 4, 4, 4, 
             5, 3, 1, 4, 2, 3, 6, 7, 15, 2, 
             3, 3, 13, 7, 8, 2, 2, 28, 25, 8, 
             8, 2, 3, 7, 4, 11, 4, 20, 36, 2, 
             5, 3, 20, 7, 22, 8, 5, 3, 3, 15, 
             3, 30, 2, 5, 14, 36, 2, 3, 6, 3, 
             2, 1, 3, 2, 4, 25, 7, 3, 4, 2, 
             36, 2, 5, 4, 6, 16, 2, 2, 3, 1, 
             1, 1, 1, 2, 31, 36, 28, 4, 3, 10, 
             1, 1, 7, 3, 32, 9, 7, 2, 15, 4, 
             3, 10, 3, 4, 5, 6, 2, 3, 3, 32, 
             36, 8, 3, 3, 10, 21, 2, 4, 2, 3, 
             17, 1, 1, 36, 36, 36, 13, 11, 3, 3, 
             3, 2, 17, 14, 2, 24, 6, 29, 7, 2, 
             3, 4, 2, 11, 5, 8, 7, 21, 4, 1, 
             2, 5, 36, 3, 28, 27, 3, 15, 3, 5, 
             2, 11, 3, 6, 14, 22, 16, 2, 2, 18, 
             2, 3, 4, 2, 23, 13, 5, 33, 22, 9, 
             2, 3, 7, 2, 5, 31, 27, 7, 2, 8, 
             14, 2, 36, 2, 4, 6, 2, 2, 8, 11, 
             20, 11, 3, 16, 8, 36, 4, 14, 3, 2, 
             7, 3, 11, 2, 4, 3, 8, 22, 1, 13, 
             23, 17, 6, 26, 2, 6, 11, 9, 2, 3, 
             20, 34, 9, 26, 36, 2, 19, 36, 25, 2, 
             2, 5, 3, 20, 1, 6, 9, 31, 2, 2, 
             20, 16, 36, 5, 28, 19, 17, 5, 4, 18, 
             2, 13, 4, 14, 3, 19, 2, 2, 10, 2, 
             29, 5, 4, 7, 2, 14, 6, 4, 13, 1, 
             2, 16, 3, 5, 18, 12, 6, 17, 6, 11, 
             2, 13, 4, 2, 2, 1, 8, 3, 3, 7, 
             4, 2, 8, 7, 2, 23, 3, 10, 13, 10, 
             26, 14, 33, 2, 16, 23, 3, 10, 13, 4)
  
 )

# Verificar estructura
glimpse(datos)

# =============================================================================
# 2. ANÁLISIS EXPLORATORIO DEL TIEMPO
# =============================================================================

# Histograma y QQ-plot
par(mfrow = c(1, 2))
hist(datos$tiempo_meses, col = "steelblue", main = "Distribución del tiempo",
     xlab = "Tiempo (meses)", ylab = "Frecuencia")
qqnorm(datos$tiempo_meses, col = "steelblue", main = "QQ-Plot")
qqline(datos$tiempo_meses, col = "red")
par(mfrow = c(1, 1))

# Estadísticas descriptivas del tiempo
datos %>%
  summarise(
    n = n(),
    media = mean(tiempo_meses),
    mediana = median(tiempo_meses),
    de = sd(tiempo_meses),
    min = min(tiempo_meses),
    max = max(tiempo_meses)
  )

# =============================================================================
# 3. PREPARACIÓN DE VARIABLES
# =============================================================================

# Crear variable numérica para el evento y convertir a factores
datos <- datos %>%
  mutate(
    muerte = if_else(tipo_evento == "Fallecido", 1, 0),
    trr = factor(trr, 
                 levels = c("hd", "dp", "tra"), 
                 labels = c("Hemodiálisis", "Diálisis Peritoneal", "Trasplante")),
    sexo = factor(sexo, 
                  levels = c("m", "f"), 
                  labels = c("Masculino", "Femenino")),
    edad_rangos = factor(edad_rangos, 
                         levels = c("0 a 4", "5 a 9", "10 a 14", "15 a 18")),
    id = row_number()
  )

# Verificar estructura actualizada
glimpse(datos)

# Resumen de eventos
datos %>%
  count(tipo_evento) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1))

# Tabla cruzada: eventos por tipo de terapia
datos %>%
  count(trr, tipo_evento) %>%
  pivot_wider(names_from = tipo_evento, values_from = n, values_fill = 0) %>%
  mutate(Total = Censurado + Fallecido,
         Mortalidad_pct = round(Fallecido / Total * 100, 1))

# =============================================================================
# 4. REPRESENTACIÓN GRÁFICA DE EVENTOS
# =============================================================================

# Gráfico de seguimiento por paciente
p_eventos <- datos %>%
  ggplot(aes(x = id, y = tiempo_meses)) +
  geom_linerange(aes(ymin = 0, ymax = tiempo_meses), color = "gray60") +
  geom_point(aes(shape = tipo_evento, color = tipo_evento), stroke = 1, size = 2) +
  scale_shape_manual(values = c(1, 4)) +
  scale_color_manual(values = c("steelblue", "red")) +
  labs(x = "Pacientes", 
       y = "Tiempo (meses)", 
       title = "Seguimiento de pacientes con ERC pediátrica",
       shape = "Evento", 
       color = "Evento") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "bottom")

p_eventos

# =============================================================================
# 5. ANÁLISIS DE KAPLAN-MEIER GLOBAL
# =============================================================================

# Crear objeto de supervivencia
Surv(datos$tiempo_meses, datos$muerte)

# Modelo Kaplan-Meier global
km_global <- survfit(Surv(tiempo_meses, muerte) ~ 1, data = datos)
km_global

# Resumen detallado
summary(km_global)

# Curva de supervivencia global
ggsurvplot(km_global, 
           data = datos,
           xlab = "Tiempo (meses)",
           ylab = "Probabilidad de supervivencia",
           title = "Curva de Supervivencia Global - ERC Pediátrica",
           conf.int = TRUE,
           risk.table = TRUE,
           ggtheme = theme_minimal(),
           palette = "Dark2",
           surv.median.line = "hv")

# =============================================================================
# 6. COMPARACIÓN DE CURVAS POR TIPO DE TERAPIA (TRR)
# =============================================================================

# Modelo Kaplan-Meier por tipo de terapia
km_trr <- survfit(Surv(tiempo_meses, muerte) ~ trr, data = datos)
km_trr

# Resumen por grupo
summary(km_trr)

# Curvas de supervivencia por TRR
ggsurvplot(km_trr, 
           data = datos,
           xlab = "Tiempo (meses)",
           ylab = "Probabilidad de supervivencia",
           title = "Supervivencia según Tipo de Terapia de Reemplazo Renal",
           conf.int = TRUE,
           risk.table = TRUE,
           pval = TRUE,
           ggtheme = theme_minimal(),
           palette = "Dark2",
           legend.title = "Terapia",
           legend.labs = c("Hemodiálisis", "Diálisis Peritoneal", "Trasplante"))

# Prueba Log-Rank
survdiff(Surv(tiempo_meses, muerte) ~ trr, data = datos)

# =============================================================================
# 7. COMPARACIÓN DE CURVAS POR SEXO
# =============================================================================

# Modelo Kaplan-Meier por sexo
km_sexo <- survfit(Surv(tiempo_meses, muerte) ~ sexo, data = datos)
km_sexo

# Resumen por grupo
summary(km_sexo)

# Curvas de supervivencia por sexo
ggsurvplot(km_sexo, 
           data = datos,
           xlab = "Tiempo (meses)",
           ylab = "Probabilidad de supervivencia",
           title = "Supervivencia según Sexo",
           conf.int = TRUE,
           risk.table = TRUE,
           pval = TRUE,
           ggtheme = theme_minimal(),
           palette = c("#2E9FDF", "#E7B800"),
           legend.title = "Sexo",
           legend.labs = c("Masculino", "Femenino"))

# Prueba Log-Rank
survdiff(Surv(tiempo_meses, muerte) ~ sexo, data = datos)

# =============================================================================
# 8. COMPARACIÓN DE CURVAS POR GRUPO DE EDAD
# =============================================================================

# Modelo Kaplan-Meier por grupo de edad
km_edad <- survfit(Surv(tiempo_meses, muerte) ~ edad_rangos, data = datos)
km_edad

# Resumen por grupo
summary(km_edad)

# Curvas de supervivencia por grupo de edad
ggsurvplot(km_edad, 
           data = datos,
           xlab = "Tiempo (meses)",
           ylab = "Probabilidad de supervivencia",
           title = "Supervivencia según Grupo de Edad",
           conf.int = TRUE,
           risk.table = TRUE,
           pval = TRUE,
           ggtheme = theme_minimal(),
           palette = "jco",
           legend.title = "Edad (años)",
           legend.labs = c("0 a 4", "5 a 9", "10 a 14", "15 a 18"))

# Prueba Log-Rank
survdiff(Surv(tiempo_meses, muerte) ~ edad_rangos, data = datos)

# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================
