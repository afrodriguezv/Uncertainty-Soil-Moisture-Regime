# ============================================================
# ANÁLISIS ESTADÍSTICO DE CAPACIDAD DE AGUA DISPONIBLE (CAD)
# ============================================================

# 1. CARGAR LIBRERÍAS NECESARIAS
# --------------------------------------------
library(tidyverse)
library(gtsummary)
library(gt)

# 2. PREPARAR LOS DATOS
# --------------------------------------------
# 

df <- read.csv("Out/Prop_SCH_pfs.csv", sep=",")

df_analisis <- df %>%
  select(
    CAD_SCHS,
    CAD_mm_cm
  ) %>%
  rename(
    "CAD total (mm)" = CAD_SCHS,
    "CAD específica (mm/cm)" = CAD_mm_cm
  )

# 3. TABLA 1 CORREGIDA - ESTADÍSTICAS DESCRIPTIVAS BÁSICAS
# --------------------------------------------
cat("\n========================================\n")
cat("TABLA 1: ESTADÍSTICAS DESCRIPTIVAS (CORREGIDA)\n")
cat("========================================\n")

tabla1 <- df_analisis %>%
  select("CAD total (mm)", "CAD específica (mm/cm)") %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{N_obs} | {mean} ± {sd} | {median} ({p25}-{p75}) | {min}-{max}"
    ),
    digits = list(
      "CAD total (mm)" ~ c(0, 1, 1, 1, 1, 1),
      "CAD específica (mm/cm)" ~ c(0, 2, 2, 2, 2, 2)
    ),
    label = list(
      "CAD total (mm)" ~ "CAD total del perfil (mm)",
      "CAD específica (mm/cm)" ~ "CAD por unidad de espesor (mm/cm)"
    )
  ) %>%
  modify_header(stat_0 = "**Valor**") %>%
  modify_caption("**Tabla 1.** Estadísticas descriptivas de Capacidad de Agua Disponible") %>%
  bold_labels()

# Mostrar tabla
tabla1

# 4. OPCIONES ALTERNATIVAS DE TABLAS
# --------------------------------------------

# Opción 2: Tabla con formato más simple
tabla2 <- df_analisis %>%
  select("CAD total (mm)", "CAD específica (mm/cm)") %>%
  tbl_summary(
    statistic = all_continuous() ~ "{mean} ± {sd}",
    digits = list(
      "CAD total (mm)" ~ 1,
      "CAD específica (mm/cm)" ~ 2
    )
  ) %>%
  add_n() %>%
  modify_header(stat_0 = "**Media ± DE**") %>%
  modify_caption("**Tabla 2.** Estadísticas resumidas de CAD") %>%
  bold_labels()

tabla2

# Opción 3: Tabla con cuantiles
tabla3 <- df_analisis %>%
  select("CAD total (mm)", "CAD específica (mm/cm)") %>%
  tbl_summary(
    statistic = all_continuous() ~ "{median} ({p25}-{p75})",
    digits = list(
      "CAD total (mm)" ~ 1,
      "CAD específica (mm/cm)" ~ 2
    )
  ) %>%
  add_n() %>%
  modify_header(stat_0 = "**Mediana (Q1-Q3)**") %>%
  modify_caption("**Tabla 3.** Estadísticas de CAD por cuantiles") %>%
  bold_labels()

tabla3

# Opción 4: Tabla con rango
tabla4 <- df_analisis %>%
  select("CAD total (mm)", "CAD específica (mm/cm)") %>%
  tbl_summary(
    statistic = all_continuous() ~ "{min}-{max}",
    digits = list(
      "CAD total (mm)" ~ 1,
      "CAD específica (mm/cm)" ~ 2
    )
  ) %>%
  add_n() %>%
  modify_header(stat_0 = "**Rango (Min-Max)**") %>%
  modify_caption("**Tabla 4.** Rango de valores de CAD") %>%
  bold_labels()

tabla4

# 5. TABLA COMPLETA CON MÚLTIPLES ESTADÍSTICOS
# --------------------------------------------
cat("\n========================================\n")
cat("TABLA 5: ESTADÍSTICAS COMPLETAS\n")
cat("========================================\n")

tabla_completa <- df_analisis %>%
  select("CAD total (mm)", "CAD específica (mm/cm)") %>%
  tbl_summary(
    type = all_continuous() ~ "continuous2",  # ← AÑADE ESTA LÍNEA
    statistic = list(
      all_continuous() ~ c(
        "{N_obs}",
        "{mean} ± {sd}",
        "{median} ({p25}-{p75})",
        "{min}-{max}"
      )
    ),
    digits = list(
      "CAD total (mm)" ~ c(0, 1, 1, 1),
      "CAD específica (mm/cm)" ~ c(0, 2, 2, 2)
    ),
    label = list(
      "CAD total (mm)" ~ "CAD total del perfil (mm)",
      "CAD específica (mm/cm)" ~ "CAD por unidad de espesor (mm/cm)"
    )
  ) %>%
  modify_header(stat_0 = "**Estadístico**") %>%
  modify_caption("**Tabla 5.** Estadísticas completas de Capacidad de Agua Disponible") %>%
  modify_footnote(
    stat_0 ~ "N: número de observaciones; DE: Desviación Estándar"
  ) %>%
  bold_labels()

tabla_completa

# 6. TABLA PARA PUBLICACIÓN CON FORMATO MEJORADO
# --------------------------------------------
cat("\n========================================\n")
cat("TABLA 6: TABLA PARA PUBLICACIÓN\n")
cat("========================================\n")

tabla_publicacion <- df_analisis %>%
  select("CAD total (mm)", "CAD específica (mm/cm)") %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{N_obs} | {mean} ± {sd} | {median} ({p25}-{p75}) | {min}-{max}"
    ),
    digits = list(
      "CAD total (mm)" ~ c(0, 1, 1, 1, 1, 1),
      "CAD específica (mm/cm)" ~ c(0, 2, 2, 2, 2, 2)
    ),
    label = list(
      "CAD total (mm)" ~ "CAD total del perfil (mm)",
      "CAD específica (mm/cm)" ~ "CAD específica (mm/cm)"
    )
  ) %>%
  modify_header(stat_0 = "**n** | **Media ± DE** | **Mediana (Q1-Q3)** | **Mín-Máx**") %>%
  modify_caption("**Tabla 6.** Estadísticas descriptivas de Capacidad de Agua Disponible en la SCH") %>%
  modify_footnote(
    stat_0 ~ "DE: Desviación Estándar; Q1: Primer cuartil; Q3: Tercer cuartil"
  ) %>%
  bold_labels()

tabla_publicacion

# 7. EXPORTAR TABLAS
# --------------------------------------------
cat("\n========================================\n")
cat("EXPORTANDO TABLAS\n")
cat("========================================\n")

# Exportar como HTML
tabla_publicacion %>%
  as_gt() %>%
  gt::gtsave(filename = "tabla_CAD_publicacion.html")

# Exportar como RTF (para Word)
tabla_publicacion %>%
  as_gt() %>%
  gt::gtsave(filename = "tabla_CAD_publicacion.rtf")

