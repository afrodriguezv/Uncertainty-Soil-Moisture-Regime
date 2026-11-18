# =============================================================================
# Código para extraer las propiedades en la Sección Control de Humedad
#=============================================================================

# =============================================================================
# 1. LIBRERÍAS
# =============================================================================

library(dplyr)

# =============================================================================
# 2. FUNCIONES
# =============================================================================

# ----------------------------------------------------------------------------- 
# 2.1 Cálculo detallado de CAD por perfil
# -----------------------------------------------------------------------------

calcular_CAD_detallado_completo <- function(df_perfiles) {
  
  df_detalle <- df_perfiles %>%
    group_by(ID_perfil) %>%
    mutate(
      limite_sup = first(Limite_superior),
      limite_inf = first(Limite_inferior),
      
      top_interseccion = pmax(top, limite_sup),
      bottom_interseccion = pmin(bottom, limite_inf),
      espesor_interseccion = pmax(0, bottom_interseccion - top_interseccion),
      
      contribucion_CAD = AWD * BD *  (1 - FR / 100) * espesor_interseccion * 10 ,
      
      porcentaje_horizonte_en_zona = ifelse(bottom - top > 0, 
                                            espesor_interseccion / (bottom - top) * 100, 
                                            0),
      
      porcentaje_zona_cubierta = ifelse((limite_inf - limite_sup) > 0,
                                        espesor_interseccion / (limite_inf - limite_sup) * 100,
                                        0),
      
      BD_p   = BD   * porcentaje_zona_cubierta/100,
      sand_p = sand * porcentaje_zona_cubierta/100,
      clay_p = clay * porcentaje_zona_cubierta/100,
      silt_p = 100  - clay_p - sand_p
      
    ) %>%
    ungroup()
  
  df_resumen <- df_detalle %>%
    group_by(ID_perfil) %>%
    summarise(
      limite_superior = first(limite_sup),
      limite_inferior = first(limite_inf),
      CAD_SCHS = sum(contribucion_CAD, na.rm = TRUE),
      BD_SCHS = sum(BD_p, na.rm = TRUE),
      sand_SCHS = sum(sand_p, na.rm = TRUE),
      clay_SCHS = sum(clay_p, na.rm = TRUE),
      silt_SCHS = sum(silt_p, na.rm = TRUE),
      espesor_efectivo = sum(espesor_interseccion, na.rm = TRUE),
      num_horizontes = n(),
      horizontes_en_zona = sum(espesor_interseccion > 0),
      .groups = 'drop'
    ) %>%
    mutate(
      espesor_zona = limite_inferior - limite_superior,
      porcentaje_cobertura = ifelse(espesor_zona > 0,
                                    espesor_efectivo / espesor_zona * 100,
                                    0),
      CAD_mm_cm = ifelse(espesor_efectivo > 0,
                         CAD_SCHS / espesor_efectivo,
                         NA)
    )
  
  return(list(
    detalle_horizontes = df_detalle,
    resumen_perfiles = df_resumen
  ))
}

# ----------------------------------------------------------------------------- 
# 2.2 Verificación de datos
# -----------------------------------------------------------------------------

verificar_datos <- function(df_perfiles) {
  
  inconsistencias <- list()
  
  inconsistencias_limites <- df_perfiles %>%
    group_by(ID_perfil) %>%
    summarise(
      num_limites_sup = n_distinct(Limite_superior),
      num_limites_inf = n_distinct(Limite_inferior),
      .groups = 'drop'
    ) %>%
    filter(num_limites_sup > 1 | num_limites_inf > 1)
  
  if (nrow(inconsistencias_limites) > 0) {
    inconsistencias$limites_inconsistentes <- inconsistencias_limites
  }
  
  limites_invalidos <- df_perfiles %>%
    group_by(ID_perfil) %>%
    summarise(
      limite_sup = first(Limite_superior),
      limite_inf = first(Limite_inferior),
      .groups = 'drop'
    ) %>%
    filter(limite_sup >= limite_inf)
  
  if (nrow(limites_invalidos) > 0) {
    inconsistencias$limites_invalidos <- limites_invalidos
  }
  
  horizontes_solapados <- df_perfiles %>%
    group_by(ID_perfil) %>%
    arrange(top) %>%
    mutate(
      solapamiento = ifelse(row_number() > 1, 
                            top < lag(bottom), 
                            FALSE)
    ) %>%
    filter(solapamiento == TRUE) %>%
    select(ID_perfil, top, bottom)
  
  if (nrow(horizontes_solapados) > 0) {
    inconsistencias$horizontes_solapados <- horizontes_solapados
  }
  
  return(inconsistencias)
}

# ----------------------------------------------------------------------------- 
# 2.3 Exportar resultados
# -----------------------------------------------------------------------------

exportar_resultados_CAD <- function(resultados, nombre_archivo = "propierties_SCHS.csv") {
  write.csv(resultados, nombre_archivo, row.names = FALSE)
  cat("Resultados exportados a:", nombre_archivo, "\n")
}

# =============================================================================
# 3. DATOS DE EJEMPLO
# =============================================================================

set.seed(123)

perfiles_ejemplo <- data.frame(
  ID_perfil = c("P1", "P1", "P1", "P2", "P2", "P3", "P3", "P3"),
  top = c(0, 30, 60, 0, 40, 0, 25, 60),
  bottom = c(30, 60, 100, 40, 90, 25, 60, 110),
  AWD = c(12.5, 15.2, 10.8, 14.3, 11.7, 13.0, 16.5, 9.8),
  BD =c(1.3, 1.4, 1.2, 1.22, 1.23, 1.33, 1.45, 1.3),
  sand = c(20,20,40,40,60,30, 10, 10),
  clay = c(40,40,40, 40,39,40, 40, 40),
  silt = c(40,40,20,20,1,30,50,50),
  Limite_superior = c(20, 20, 20, 15, 15, 10, 10, 10),
  Limite_inferior = c(80, 80, 80, 70, 70, 90, 90, 90),
  stringsAsFactors = FALSE
)

perfiles_ejemplo$AWD <- perfiles_ejemplo$AWD / 100

print(perfiles_ejemplo)

# =============================================================================
# 4. DATOS REALES
# =============================================================================

df_tesis <- read.csv("Data/data_prop_pfs.csv", sep=";")

df_htz <- df_tesis %>%
  select(id, top, bottom, BD, sand, clay, silt, FR, FC, PMP) %>%
  rename(ID_perfil = id)

df_htz$AWD <- ( df_htz$FC - df_htz$PMP ) / 100

df_pfs_schs <- read.csv("Out/Pfs_sch.csv", sep = ",")

df_pfs_schs <- df_pfs_schs %>%
  rename(
    ID_perfil = id,
    Limite_superior = top_scms,
    Limite_inferior = bottom_scms
  )

df_full <- df_htz %>%
  left_join(df_pfs_schs, by = "ID_perfil")

# =============================================================================
# 5. CÁLCULOS
# =============================================================================

resultado_detallado <- calcular_CAD_detallado_completo(df_full)

print(head(resultado_detallado$detalle_horizontes))
print(resultado_detallado$resumen_perfiles)

# =============================================================================
# 6. VALIDACIÓN
# =============================================================================

inconsistencias <- verificar_datos(df_full)
print(inconsistencias)

# =============================================================================
# 7. RESUMEN ESTADÍSTICO
# =============================================================================

resumen_estadistico <- resultado_detallado$resumen_perfiles %>%
  summarise(
    n_perfiles = n(),
    CAD_mm_cm = mean(CAD_mm_cm, na.rm = TRUE),
    CAD_SCHS = mean(CAD_SCHS, na.rm = TRUE),
    CAD_min = min(CAD_SCHS, na.rm = TRUE),
    CAD_max = max(CAD_SCHS, na.rm = TRUE),
    rango_profundidad_promedio = mean(espesor_zona, na.rm = TRUE)
  )

print(resumen_estadistico)

# =============================================================================
# 8. EXPORTAR
# =============================================================================

exportar_resultados_CAD(resultado_detallado$resumen_perfiles, "Out/Prop_SCH_pfs.csv")
exportar_resultados_CAD(resultado_detallado$detalle_horizontes, "Out/Prop_SCH_hzts.csv")
