
library(dplyr)
library(aqp)


SCMS <- function(df) {
  
  # los valores de FC, PMP y FR deben estar en decimales"

  
  hz <- horizons(df)
 
  # Verificar si los campos 'FC'(Field Capacity) , 'PMP' , 'BD' (Bulk Densitiy) y 'FR' (Fragment Roks) están en el data frame
  required_columns <- c("FC", "PMP", "BD", "FR")
  
  missing_columns <- required_columns[!required_columns %in% colnames(hz)]
  
  if (length(missing_columns) > 0) {
    stop(paste("Faltan los siguientes campos: ", paste(missing_columns, collapse = ", ")))
  }
  
  # Obtener los nombres de las columnas de profundidad
  depthnames <- horizonDepths(df)
  
  # Crear el data frame 'res' con los cálculos necesarios
  res <- data.frame(profile_id(df), hzID(df), top = hz$top, bottom = hz$bottom, FC = hz$FC, PMP = hz$PMP, BD = hz$BD, FR = hz$FR,
                    thk = (hz[[depthnames[[2]]]] - hz[[depthnames[1]]]))
  
  # Calcular la capacidad de agua disponible (AWC)
  res$AWC_cm_cm <- (res$FC - res$PMP) / 100 * (1 - (res$FR/100)) * res$BD
  res$AWC_cm_h <- res$AWC_cm_cm * res$thk
  
  # Calcular el AWC acumulado
  res$AWC_cm_h_cum <- cumsum(res$AWC_cm_h)
  
  # Inicializar la columna 'upper'
  res$upper <- NA
  res$lower <- NA
  
  valor <- 2.5
  
  if (max(res$AWC_cm_h_cum, na.rm = TRUE) >= 2.5) {
    # Iterar sobre las filas del data frame
    for (i in 1:nrow(res)) {
      # Verificar si 'AWC_cm_h_cum' tiene valores válidos (no NA)
      if (!is.na(res$AWC_cm_h_cum[i]) & res$BD[i] > 0) {
        # Si es la primera fila, se calcula directamente
        if (i == 1) {
          if (valor - res$AWC_cm_h_cum[i] > 0) {
            res$upper[i] <- valor - res$AWC_cm_h_cum[i]  # Si el valor es positivo
          } else {
            res$upper[i] <- (valor * res$bottom[i]) / res$AWC_cm_h[i]  # Si el valor es negativo
          }
        } else {
          # Para las demás filas, calcular con la fila anterior
          if (valor - res$AWC_cm_h_cum[i - 1] > 0) {
            if (valor - res$AWC_cm_h_cum[i] > 0) {
              res$upper[i] <- res$bottom[i]  # Si el valor es positivo
            } else {
              res$upper[i] <- (valor - res$AWC_cm_h_cum[i - 1]) * (res$thk[i] / res$AWC_cm_h[i]) + res$bottom[i - 1]  # Operación con la fila anterior
            }
          } else {
            res$upper[i] <- 0  # Asignar 0 si la condición no se cumple
          }
        }
      } else {
        
        # Asignar 0 si 'AWC_cm_h_cum' es NA
        res$upper[i] <- 0
      }
    }
  } else {
    res$upper <- 0
  }
  
  if (max(res$AWC_cm_h_cum, na.rm = TRUE) >= 7.5) {
    
    valor = 7.5
    # Iterar sobre las filas del data frame
    for (i in 1:nrow(res)) {
      # Verificar si 'AWC_cm_h_cum' tiene valores válidos (no NA)
      if (!is.na(res$AWC_cm_h_cum[i]) & res$BD[i] > 0) {
        # Si es la primera fila, se calcula directamente
        if (i == 1) {
          if (valor - res$AWC_cm_h_cum[i] > 0) {
            res$lower[i] <- valor - res$AWC_cm_h_cum[i]  # Si el valor es positivo
          } else {
            res$lower[i] <- (valor * res$bottom[i]) / res$AWC_cm_h[i]  # Si el valor es negativo
          }
        } else {
          # Para las demás filas, calcular con la fila anterior
          if (valor - res$AWC_cm_h_cum[i - 1] > 0) {
            if (valor - res$AWC_cm_h_cum[i] > 0) {
              res$lower[i] <- res$bottom[i]  # Si el valor es positivo
            } else {
              res$lower[i] <- (valor - res$AWC_cm_h_cum[i - 1]) * (res$thk[i] / res$AWC_cm_h[i]) + res$bottom[i - 1]  # Operación con la fila anterior
            }
          } else {
            res$lower[i] <- 0  # Asignar 0 si la condición no se cumple
          }
        }
      } else {
        
        # Asignar 0 si 'AWC_cm_h_cum' es NA
        res$lower[i] <- 0
      }
    }
    
  } else {
    res$lower <- 0
  }
  
  
  result_df <- data.frame(
    id = unique(res$profile_id.df.),
    top = max(res$upper, na.rm = TRUE),
    bottom = max(res$lower, na.rm = TRUE)
  )
  
  return(result_df)
}

#SCMS_limits <- profileApply(pfs,SCMS, frameify  = TRUE)
#SCMS_limits <- SCMS_limits[!(SCMS_limits$top == 0 & SCMS_limits$bottom == 0), ]


