# Curso de Estadística no Paramétrica con R

[![YouTube](https://img.shields.io/badge/YouTube-Asesor%C3%ADa%20Estad%C3%ADstica%20y%20Tesis-red?style=flat&logo=youtube)](https://www.youtube.com/@AsesoriaEstadisticayTesis)
[![R](https://img.shields.io/badge/R-4.0%2B-blue?style=flat&logo=r)](https://www.r-project.org/)
[![Tidyverse](https://img.shields.io/badge/Tidyverse-2.0%2B-orange?style=flat)](https://www.tidyverse.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)

Repositorio oficial con materiales y scripts del curso **"Estadística No Paramétrica con R"** del canal de YouTube [Asesoría Estadística y Tesis](https://www.youtube.com/@AsesoriaEstadisticayTesis). Análisis de datos aplicados a ciencias de la salud con enfoque en reproducibilidad y buenas prácticas de programación.

---

## 📋 Descripción

Este curso está orientado al desarrollo de competencias para la implementación, análisis e interpretación de métodos estadísticos no paramétricos cuando no se cumplen los supuestos de la estadística paramétrica. Se enfatiza el análisis reproducible mediante scripts documentados y flujos de trabajo estructurados en RStudio, con ejemplos aplicados a investigaciones en ciencias de la salud.

---

## 🎯 Objetivos

### Objetivo General
Desarrollar competencias para la implementación, análisis e interpretación de métodos estadísticos no paramétricos mediante el software R y el ecosistema Tidyverse.

### Objetivos Específicos
1. **Aplicar** procedimientos estadísticos no paramétricos para el análisis de datos cuantitativos y cualitativos en contextos de investigación aplicada.
2. **Utilizar** herramientas del ecosistema Tidyverse y paquetes especializados de R para la ejecución de pruebas estadísticas y visualización de resultados.
3. **Interpretar** resultados de análisis no paramétricos considerando supuestos metodológicos, tamaños del efecto y significancia práctica.
4. **Desarrollar** habilidades de programación reproducible mediante la elaboración de scripts documentados y flujos de trabajo estructurados en RStudio.

---

## 📚 Contenido del Curso

### Módulo 1: Nivel Básico (Temas 1-12)
1. Presentación del curso
2. Exploración y visualización de datos
3. Bondad de ajuste y métodos para una muestra
4. Comparación de 2 muestras independientes
5. Comparación de k muestras independientes
6. Comparación de 2 muestras relacionadas
7. Comparación de k muestras relacionadas
8. Pruebas de asociación para variables categóricas
9. Correlación no paramétrica
10. Tamaño del efecto para variables categóricas
11. Pruebas para evaluar homogeneidad de varianzas
12. Tablas de contingencia estratificadas

### Módulo 2: Nivel Avanzado (Temas 13-21)
13. Pruebas de muestras relacionadas para variables categóricas
14. Concordancia: Variables categóricas
15. Concordancia: Variables numéricas
16. ANOVA con bloques no paramétrico
17. ANOVA factorial no paramétrico
18. ANOVA factorial de medidas repetidas no paramétrico
19. Métodos no paramétricos de respuesta múltiple
20. Curvas ROC
21. Análisis de Kaplan-Meier y pruebas de log-rank

---

## 🛠️ Metodología

- **Manejo de datos**: Tidyverse (dplyr, ggplot2, tidyr, forcats), janitor, knitr
- **Pruebas estadísticas**: Base R, rstatix, DescTools, psych, coin, nparLD, pROC, survival, npmv
- **Casos de estudio**: Investigaciones reales en ciencias de la salud
- **Enfoque**: Implementación práctica → Condiciones de aplicabilidad → Fundamentos teóricos
- **Scripts**: Análisis exploratorio, tests estadísticos y alternativas

---

## 💻 Requisitos

### Software
- **R** (versión 4.0 o superior): [Descargar](https://cran.r-project.org/)
- **RStudio** (recomendado): [Descargar](https://posit.co/download/rstudio-desktop/)

### Paquetes Principales
```r
# Instalación de paquetes necesarios
install.packages(c(
  # Ecosistema Tidyverse
  "dplyr", "ggplot2", "tidyr", "forcats",
  
  # Análisis estadístico
  "rstatix", "DescTools", "coin", "psych",
  
  # Visualización
  "ggstatsplot", "ggpubr",
  
  # Tablas y reportes
  "janitor", "knitr", "modelsummary",
  
  # Métodos específicos
  "pROC", "survival", "nparLD", "npmv"
))
```

---

## 📁 Estructura del Repositorio

```
📦 estadistica_no_parametrica_con_r/
├── 📂 scripts/
│   ├── tema_01_introduccion.R
│   ├── tema_02_exploracion_visualizacion.R
│   ├── tema_03_bondad_ajuste.R
│   ├── tema_04_dos_muestras_independientes.R
│   ├── tema_05_k_muestras_independientes.R
│   └── ... (temas 6-21)
├── 📂 presentaciones/
│   └── Curso_Estadistica_no_parametrica_R.pptx
├── 📄 README.md
└── 📄 LICENSE
```

---

## 🚀 Cómo Usar Este Repositorio

1. **Clonar el repositorio**
   ```bash
   git clone https://github.com/tu-usuario/estadistica_no_parametrica_con_r.git
   ```

2. **Abrir RStudio** y establecer el directorio de trabajo
   ```r
   setwd("ruta/a/estadistica_no_parametrica_con_r")
   ```

3. **Instalar paquetes necesarios** (ver sección de Requisitos)

4. **Explorar los scripts** en orden numérico según los temas del curso

5. **Seguir las video-lecciones** en el [canal de YouTube](https://www.youtube.com/@AsesoriaEstadisticayTesis)

---

## 📖 Libros de Referencia

- Hollander, M., Wolfe, D. A., & Chicken, E. (2013). *Nonparametric Statistical Methods* (3rd ed.). Wiley.
- Siegel, S., & Castellan, N. J. (1988). *Nonparametric Statistics for the Behavioral Sciences* (2nd ed.). McGraw-Hill.
- Conover, W. J. (1999). *Practical Nonparametric Statistics* (3rd ed.). Wiley.
- Gibbons, J. D., & Chakraborti, S. (2011). *Nonparametric Statistical Inference* (5th ed.). CRC Press.

---

## 👨‍🏫 Autor

**Profesor Andre Chocó-Cedillos**
- 📺 YouTube: [Asesoría Estadística y Tesis](https://www.youtube.com/@AsesoriaEstadisticayTesis)
- 📧 Email: panteisme@yahoo.com
- 🏛️ Afiliación: Universidad de San Carlos de Guatemala

---

## 📝 Licencia

Este proyecto está bajo la Licencia MIT - consulta el archivo [LICENSE](LICENSE) para más detalles.

---

## 🤝 Contribuciones

Las contribuciones son bienvenidas. Por favor:
1. Haz un fork del proyecto
2. Crea una rama para tu característica (`git checkout -b feature/nueva-caracteristica`)
3. Commit tus cambios (`git commit -m 'Añadir nueva característica'`)
4. Push a la rama (`git push origin feature/nueva-caracteristica`)
5. Abre un Pull Request

---

## ⭐ Agradecimientos

Si este material te resulta útil, considera:
- ⭐ Dar una estrella al repositorio
- 📺 Suscribirte al [canal de YouTube](https://www.youtube.com/@AsesoriaEstadisticayTesis)
- 📢 Compartir con colegas y estudiantes

---

## 📞 Contacto y Soporte

Para consultas sobre el curso:
- 💬 Comentarios en los videos de YouTube
- 📧 Email: panteisme@yahoo.com
- 🐛 Issues en este repositorio para reportar errores o sugerencias

---

**Última actualización:** Diciembre 2025
