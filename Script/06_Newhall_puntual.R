############################################################
# Newhall Model point
# Input data: Precipitacion , Temperature
# Output: Newhall results in xml, pdf & database results csv
############################################################

library(jNSMR)
library(terra)
library(sf)
library(lubridate)
library(rJava)
library(XML)
library(dplyr)

#function2
library(ggplot2)
library(gridExtra)
library(knitr)
library(kableExtra)
library(extrafont)
library(grid)


# data functions ###############################################

# export to XML (open in jsnm java)

  exportNewhallToXML <- function(results, 
                               dataset, 
                               output_file, 
                               to_metric = TRUE) 
  {
  
  # Helper function para redondear
  round_value <- function(value, decimal_places) {
    round(value, decimal_places)
  }
  
  # Helper function para extraer List a vector (usa java.util.List en lugar de ArrayList)
  extractJavaList <- function(java_list) {
    if (is.null(java_list)) return(numeric(0))
    
    size <- .jcall(java_list, "I", "size")
    if (size == 0) return(numeric(0))
    
    result <- numeric(size)
    
    for (i in 1:size) {
      idx <- as.integer(i - 1)
      
      # Para List, obtener el elemento como Object
      element <- .jcall(java_list, "Ljava/lang/Object;", "get", idx)
      
      if (!is.null(element)) {
        if (.jinstanceof(element, "java.lang.Double")) {
          result[i] <- .jcall(element, "D", "doubleValue")
        } else if (.jinstanceof(element, "java.lang.Integer")) {
          result[i] <- .jcall(element, "I", "intValue")
        } else if (.jinstanceof(element, "java.lang.Character")) {
          char_val <- .jcall(element, "C", "charValue")
          # Para character, convertir a string primero
          result[i] <- as.numeric(char_val)
        }
      }
    }
    return(result)
  }
  
  # Función para extraer ArrayList (para los calendarios)
  extractArrayList <- function(java_arraylist) {
    if (is.null(java_arraylist)) return(numeric(0))
    
    size <- .jcall(java_arraylist, "I", "size")
    if (size == 0) return(numeric(0))
    
    result <- numeric(size)
    
    for (i in 1:size) {
      idx <- as.integer(i - 1)
      element <- .jcall(java_arraylist, "Ljava/lang/Object;", "get", idx)
      
      if (!is.null(element)) {
        if (.jinstanceof(element, "java.lang.Double")) {
          result[i] <- .jcall(element, "D", "doubleValue")
        } else if (.jinstanceof(element, "java.lang.Integer")) {
          result[i] <- .jcall(element, "I", "intValue")
        } else if (.jinstanceof(element, "java.lang.Character")) {
          char_val <- .jcall(element, "C", "charValue")
          result[i] <- char_val
        }
      }
    }
    return(result)
  }
  
  # Metadata
  metadata_obj <- tryCatch({
    .jcall(dataset, "Lorg/psu/newhall/sim/NewhallDatasetMetadata;", "getMetadata")
  }, error = function(e) NULL)
  
  # document XML
  doc <- newXMLDoc()
  root <- newXMLNode("model", doc = doc)
  
  # ========== METADATA ==========
  metadata <- newXMLNode("metadata", parent = root)
  
  # stninfo
  stninfo <- newXMLNode("stninfo", parent = metadata)
  
  # Station information
  stnname <- newXMLNode("stnname", 
                        text = .jcall(dataset, "S", "getName"), 
                        parent = stninfo)
  
  stnelev <- newXMLNode("stnelev", 
                        text = as.character(.jcall(dataset, "D", "getElevation")), 
                        parent = stninfo)
  
  country <- newXMLNode("country", 
                        text = .jcall(dataset, "S", "getCountry"), 
                        parent = stninfo)
  
  # Metadata information (optional)
  if (!is.null(metadata_obj)) {
    nettype <- newXMLNode("nettype", 
                          text = tryCatch(.jcall(metadata_obj, "S", "getNetwork"), 
                                          error = function(e) ""), 
                          parent = stninfo)
    
    stnid <- newXMLNode("stnid", 
                        text = tryCatch(.jcall(metadata_obj, "S", "getStationId"), 
                                        error = function(e) ""), 
                        parent = stninfo)
    
    stateprov <- newXMLNode("stateprov", 
                            text = tryCatch(.jcall(metadata_obj, "S", "getStationStateProvidence"), 
                                            error = function(e) ""), 
                            parent = stninfo)
  }
  
  # mlra (Optional)
  if (!is.null(metadata_obj)) {
    mlra <- newXMLNode("mlra", parent = metadata)
    mlraname <- newXMLNode("mlraname", 
                           text = tryCatch(.jcall(metadata_obj, "S", "getMlraName"), 
                                           error = function(e) ""), 
                           parent = mlra)
    
    mlraid <- newXMLNode("mlraid", 
                         text = tryCatch(as.character(.jcall(metadata_obj, "I", "getMlraId")), 
                                         error = function(e) ""), 
                         parent = mlra)
  }
  
  # Contribuyent information (optional)
  if (!is.null(metadata_obj)) {
    cntinfo <- newXMLNode("cntinfo", parent = metadata)
    cntper <- newXMLNode("cntper", parent = cntinfo)
    
    newXMLNode("firstname", 
               text = tryCatch(.jcall(metadata_obj, "S", "getContribFirstName"), 
                               error = function(e) ""), 
               parent = cntper)
    
    newXMLNode("lastname", 
               text = tryCatch(.jcall(metadata_obj, "S", "getContribLastName"), 
                               error = function(e) ""), 
               parent = cntper)
    
    newXMLNode("title", 
               text = tryCatch(.jcall(metadata_obj, "S", "getContribTitle"), 
                               error = function(e) ""), 
               parent = cntper)
    
    newXMLNode("cntorg", 
               text = tryCatch(.jcall(metadata_obj, "S", "getContribOrg"), 
                               error = function(e) ""), 
               parent = cntinfo)
  }
  
  # rundate
  rundate <- newXMLNode("rundate", 
                        text = format(Sys.Date(), "%Y%m%d"), 
                        parent = metadata)
  
  # nsmversion 
  nsmversion <- newXMLNode("nsmver", text = "1.0", parent = metadata)
  
  srcunitsys <- newXMLNode("srcunitsys", 
                           text = if(!is.null(metadata_obj)) 
                             tryCatch(.jcall(metadata_obj, "S", "getUnitSystem"), 
                                      error = function(e) "metric") else "metric", 
                           parent = metadata)
  
  # ========== INPUT ==========
  input <- newXMLNode("input", parent = root)
  
  # location
  location <- newXMLNode("location", parent = input)
  
  # Get Latitude
  lat_val <- .jcall(dataset, "D", "getLatitude")
  ns_hemisphere <- .jcall(dataset, "C", "getNsHemisphere")
  
  if (ns_hemisphere == 'N') {
    lat_text <- as.character(round_value(lat_val, 4))
  } else {
    lat_text <- as.character(round_value(lat_val * -1, 4))
  }
  
  # Get Longitude
  lon_val <- .jcall(dataset, "D", "getLongitude")
  ew_hemisphere <- .jcall(dataset, "C", "getEwHemisphere")
  
  if (ew_hemisphere == 'E') {
    lon_text <- as.character(round_value(lon_val, 4))
  } else {
    lon_text <- as.character(round_value(lon_val * -1, 4))
  }
  
  lat <- newXMLNode("lat", text = lat_text, parent = location)
  lon <- newXMLNode("lon", text = lon_text, parent = location)
  usercoordfmt <- newXMLNode("usercoordfmt", text = "DD", parent = location)
  
  # record time
  recordpd <- newXMLNode("recordpd", parent = input)
  pdtype <- newXMLNode("pdtype", text = "normal", parent = recordpd)
  pdbegin <- newXMLNode("pdbegin", 
                        text = as.character(.jcall(dataset, "I", "getStartYear")), 
                        parent = recordpd)
  pdend <- newXMLNode("pdend", 
                      text = as.character(.jcall(dataset, "I", "getEndYear")), 
                      parent = recordpd)
  
  # precipitation months
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  precips <- newXMLNode("precips", parent = input)
  
  #  List Precipitation
  precip_list <- .jcall(dataset, "Ljava/util/List;", "getPrecipitation")
  precip_data <- extractJavaList(precip_list)
  
  is_metric <- .jcall(dataset, "Z", "isMetric")
  
  for (i in 1:12) {
    precip_val <- if (i <= length(precip_data)) precip_data[i] else NA
    
    # Conversion units
    if (!is.na(precip_val)) {
      if (to_metric && !is_metric) {
        # Convert inches to mm
        precip_val <- precip_val * 25.4
      } else if (!to_metric && is_metric) {
        # Convert mm to inches
        precip_val <- precip_val * 0.0393700787
      }
    }
    
    precip <- newXMLNode("precip", 
                         attrs = list(id = months[i]),
                         text = if(!is.na(precip_val)) 
                           as.character(round_value(precip_val, 2)) else "NA",
                         parent = precips)
  }
  
  # airtemps (Temperature Months)
  airtemps <- newXMLNode("airtemps", parent = input)
  
  # Temperature List
  temp_list <- .jcall(dataset, "Ljava/util/List;", "getTemperature")
  temp_data <- extractJavaList(temp_list)
  
  for (i in 1:12) {
    temp_val <- if (i <= length(temp_data)) temp_data[i] else NA
    
    # Conversion units
    if (!is.na(temp_val)) {
      if (to_metric && !is_metric) {
        # Convert F to C
        temp_val <- (temp_val - 32) * 5/9
      } else if (!to_metric && is_metric) {
        # Convert C to F
        temp_val <- (temp_val * 9/5) + 32
      }
    }
    
    airtemp <- newXMLNode("airtemp", 
                          attrs = list(id = months[i]),
                          text = if(!is.na(temp_val)) 
                            as.character(round_value(temp_val, 2)) else "NA",
                          parent = airtemps)
  }
  
  # smcsawc (Water Holding Capacity - results)
  smcsawc <- newXMLNode("smcsawc", 
                        text = as.character(.jcall(results, "D", "getWaterHoldingCapacity")), 
                        parent = input)
  
  # soilairrel (optional)
  if (!is.null(metadata_obj)) {
    soilairrel <- newXMLNode("soilairrel", parent = input)
    
    ampltd <- newXMLNode("ampltd", 
                         text = tryCatch(
                           as.character(.jcall(metadata_obj, "D", "getAmplitude")), 
                           error = function(e) ""), 
                         parent = soilairrel)
    
    maatmast_val <- tryCatch(
      .jcall(metadata_obj, "D", "getSoilAirOffset"), 
      error = function(e) 0)
    
    if (to_metric && !is_metric) {
      # Convert F to C
      maatmast_val <- maatmast_val * 5/9
    } else if (!to_metric && is_metric) {
      # Convert C to F
      maatmast_val <- maatmast_val * 9/5
    }
    
    maatmast <- newXMLNode("maatmast", 
                           text = as.character(round_value(maatmast_val, 2)), 
                           parent = soilairrel)
  }
  
  # ========== OUTPUT ==========
  output <- newXMLNode("output", parent = root)
  
  # Clasifications
  smrclass <- newXMLNode("smrclass", 
                         text = .jcall(results, "S", "getMoistureRegime"), 
                         parent = output)
  
  strclass <- newXMLNode("strclass", 
                         text = .jcall(results, "S", "getTemperatureRegime"), 
                         parent = output)
  
  subdiv <- newXMLNode("subgrpmod", 
                       text = paste(.jcall(results, "S", "getRegimeSubdivision1"),
                                    .jcall(results, "S", "getRegimeSubdivision2")), 
                       parent = output)
  
  # Balances hídricos
  awb <- newXMLNode("awb", 
                    text = as.character(round_value(.jcall(results, "D", "getAnnualWaterBalance"), 2)), 
                    parent = output)
  
  swb <- newXMLNode("swb", 
                    text = as.character(round_value(.jcall(results, "D", "getSummerWaterBalance"), 2)), 
                    parent = output)
  
  # smcstates
  smcstates <- newXMLNode("smcstates", parent = output)
  
  cumdays <- newXMLNode("cumdays", parent = smcstates)
  
  yrdry <- newXMLNode("yrdry", 
                      text = as.character(.jcall(results, "I", "getNumCumulativeDaysDry")), 
                      parent = cumdays)
  
  yrmd <- newXMLNode("yrmd", 
                     text = as.character(.jcall(results, "I", "getNumCumulativeDaysMoistDry")), 
                     parent = cumdays)
  
  yrmst <- newXMLNode("yrmst", 
                      text = as.character(.jcall(results, "I", "getNumCumulativeDaysMoist")), 
                      parent = cumdays)
  
  bio5dry <- newXMLNode("bio5dry", 
                        text = as.character(.jcall(results, "I", "getNumCumulativeDaysDryOver5C")), 
                        parent = cumdays)
  
  bio5md <- newXMLNode("bio5md", 
                       text = as.character(.jcall(results, "I", "getNumCumulativeDaysMoistDryOver5C")), 
                       parent = cumdays)
  
  bio5mst <- newXMLNode("bio5mst", 
                        text = as.character(.jcall(results, "I", "getNumCumulativeDaysMoistOver5C")), 
                        parent = cumdays)
  
  consdays <- newXMLNode("consdays", parent = smcstates)
  
  yrmst2 <- newXMLNode("yrmst", 
                       text = as.character(.jcall(results, "I", "getNumConsecutiveDaysMoistInSomeParts")), 
                       parent = consdays)
  
  bio8mst <- newXMLNode("bio8mst", 
                        text = as.character(.jcall(results, "I", "getNumConsecutiveDaysMoistInSomePartsOver8C")), 
                        parent = consdays)
  
  smrdry <- newXMLNode("smrdry", 
                       text = as.character(.jcall(results, "I", "getDryDaysAfterSummerSolstice")), 
                       parent = consdays)
  
  wtrmst <- newXMLNode("wtrmst", 
                       text = as.character(.jcall(results, "I", "getMoistDaysAfterWinterSolstice")), 
                       parent = consdays)
  
  # pets (Potentatial Evapotranspiration)
  pets <- newXMLNode("pets", parent = output)
  
  # Obtener PET como List
  pet_list <- tryCatch({
    .jcall(results, "Ljava/util/List;", "getMeanPotentialEvapotranspiration")
  }, error = function(e) NULL)
  
  if (!is.null(pet_list)) {
    pet_data <- extractJavaList(pet_list)
    
    for (i in 1:12) {
      pet_val <- if (i <= length(pet_data)) pet_data[i] else NA
      
      if (!is.na(pet_val) && !to_metric) {
        # Convert mm to inches (PETs siempre están en métrico en Results)
        pet_val <- pet_val * 0.0393700787
      }
      
      pet <- newXMLNode("pet", 
                        attrs = list(id = months[i]),
                        text = if(!is.na(pet_val)) 
                          as.character(round_value(pet_val, 2)) else "NA",
                        parent = pets)
    }
  }
  
  # calendars
  calendars <- newXMLNode("calendars", parent = output)
  
  # Temperature calendar
  tempCalElement <- newXMLNode("tempcal", parent = calendars)
  
  temp_cal_list <- tryCatch({
    .jcall(results, "Ljava/util/ArrayList;", "getTemperatureCalendar")
  }, error = function(e) NULL)
  
  if (!is.null(temp_cal_list)) {
    temp_cal <- extractArrayList(temp_cal_list)
    
    if (length(temp_cal) > 0) {
      # Procesar calendario de temperatura
      last_char <- temp_cal[1]
      last_pos <- 0
      
      for (i in 2:length(temp_cal)) {
        this_char <- temp_cal[i]
        
        if (this_char == last_char && i != length(temp_cal)) {
          next
        } else {
          block_name <- switch(as.character(last_char),
                               "-" = "stlt5",
                               "5" = "st5to8", 
                               "8" = "stgt8",
                               "unknown")
          
          blockToAdd <- newXMLNode(block_name, parent = tempCalElement)
          beginday <- newXMLNode("beginday", text = as.character(last_pos + 1), parent = blockToAdd)
          
          end_day <- i
          if (i == length(temp_cal)) {
            end_day <- length(temp_cal)
          }
          
          endday <- newXMLNode("endday", text = as.character(end_day), parent = blockToAdd)
          
          last_char <- this_char
          last_pos <- i
        }
      }
    }
  }
  
  # Moisture calendar
  moistCalElement <- newXMLNode("moistcal", parent = calendars)
  
  moist_cal_list <- tryCatch({
    .jcall(results, "Ljava/util/ArrayList;", "getMoistureCalendar")
  }, error = function(e) NULL)
  
  if (!is.null(moist_cal_list)) {
    moist_cal <- extractArrayList(moist_cal_list)
    
    if (length(moist_cal) > 0) {
      # Procesar calendario de humedad
      last_val <- moist_cal[1]
      last_pos <- 0
      
      for (i in 2:length(moist_cal)) {
        this_val <- moist_cal[i]
        
        if (this_val == last_val && i != length(moist_cal)) {
          next
        } else {
          block_name <- switch(as.character(last_val),
                               "1" = "dry",
                               "2" = "moistdry", 
                               "3" = "moist",
                               "unknown")
          
          blockToAdd <- newXMLNode(block_name, parent = moistCalElement)
          beginday <- newXMLNode("beginday", text = as.character(last_pos + 1), parent = blockToAdd)
          
          end_day <- i
          if (i == length(moist_cal)) {
            end_day <- length(moist_cal)
          }
          
          endday <- newXMLNode("endday", text = as.character(end_day), parent = blockToAdd)
          
          last_val <- this_val
          last_pos <- i
        }
      }
    }
  }
  
  # Save XML
  saveXML(doc, file = output_file)
  
  cat("XML exportado exitosamente a:", output_file, "\n")
  return(TRUE)
  }
  

  # inform in PDF
  
  exportNewhallToPDF <- function(results, 
                                 dataset, 
                                 station_info = NULL, 
                                 output_file, 
                                 to_metric = TRUE) 
    {
    
    # Helper function para redondear
    round_value <- function(value, decimal_places) {
      round(value, decimal_places)
    }
    
    # Helper function para extraer List a vector
    extractJavaList <- function(java_list) {
      if (is.null(java_list)) return(numeric(0))
      
      size <- .jcall(java_list, "I", "size")
      if (size == 0) return(numeric(0))
      
      result <- numeric(size)
      
      for (i in 1:size) {
        idx <- as.integer(i - 1)
        element <- .jcall(java_list, "Ljava/lang/Object;", "get", idx)
        
        if (!is.null(element)) {
          if (.jinstanceof(element, "java.lang.Double")) {
            result[i] <- .jcall(element, "D", "doubleValue")
          } else if (.jinstanceof(element, "java.lang.Integer")) {
            result[i] <- .jcall(element, "I", "intValue")
          }
        }
      }
      return(result)
    }
    
    # Helper function para extraer ArrayList
    extractArrayList <- function(java_arraylist) {
      if (is.null(java_arraylist)) return(numeric(0))
      
      size <- .jcall(java_arraylist, "I", "size")
      if (size == 0) return(numeric(0))
      
      result <- numeric(size)
      
      for (i in 1:size) {
        idx <- as.integer(i - 1)
        element <- .jcall(java_arraylist, "Ljava/lang/Object;", "get", idx)
        
        if (!is.null(element)) {
          if (.jinstanceof(element, "java.lang.Double")) {
            result[i] <- .jcall(element, "D", "doubleValue")
          } else if (.jinstanceof(element, "java.lang.Integer")) {
            result[i] <- .jcall(element, "I", "intValue")
          } else if (.jinstanceof(element, "java.lang.Character")) {
            char_val <- .jcall(element, "C", "charValue")
            result[i] <- char_val
          }
        }
      }
      return(result)
    }
    
    # Get Basic Data
    station_name <- .jcall(dataset, "S", "getName")
    country <- .jcall(dataset, "S", "getCountry")
    lat <- .jcall(dataset, "D", "getLatitude")
    lon <- .jcall(dataset, "D", "getLongitude")
    elev <- .jcall(dataset, "D", "getElevation")
    start_year <- .jcall(dataset, "I", "getStartYear")
    end_year <- .jcall(dataset, "I", "getEndYear")
    
    # Get Monthly data
    #months <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun","Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
    
    months <- c( "Oct", "Nov", "Dic","Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                "Jul", "Ago", "Sep")  #change for year combined
    
    # Precipitation
    precip_list <- .jcall(dataset, "Ljava/util/List;", "getPrecipitation")
    precip_data <- extractJavaList(precip_list)
    
    # Temperature
    temp_list <- .jcall(dataset, "Ljava/util/List;", "getTemperature")
    temp_data <- extractJavaList(temp_list)
    
    # PET
    pet_list <- tryCatch({
      .jcall(results, "Ljava/util/List;", "getMeanPotentialEvapotranspiration")
    }, error = function(e) NULL)
    
    pet_data <- if (!is.null(pet_list)) extractJavaList(pet_list) else rep(NA, 12)
    
    # Calendary
    temp_cal_list <- tryCatch({
      .jcall(results, "Ljava/util/ArrayList;", "getTemperatureCalendar")
    }, error = function(e) NULL)
    
    temp_cal <- if (!is.null(temp_cal_list)) extractArrayList(temp_cal_list) else rep(NA, 365)
    
    moist_cal_list <- tryCatch({
      .jcall(results, "Ljava/util/ArrayList;", "getMoistureCalendar")
    }, error = function(e) NULL)
    
    moist_cal <- if (!is.null(moist_cal_list)) extractArrayList(moist_cal_list) else rep(NA, 365)
    
    #  dataframe Montly data
    monthly_data <- data.frame(
      Mes = months,
      Precipitacion = precip_data,
      Temperatura = temp_data,
      PET = pet_data,
      stringsAsFactors = FALSE
    )
    
    #  Monthly water balance.
    monthly_data$Balance <- monthly_data$Precipitacion - monthly_data$PET
    
    # PDF
    pdf(output_file, width = 11, height = 8.5, paper = "letter")
    
    # ========== Cover page ==========
    grid.newpage()
    pushViewport(viewport(width = 1, height = 1))
    
    grid.rect(gp = gpar(fill = "white", col = NA))
    
    # title
    grid.text("INFORME DE SIMULACIÓN NEWHALL", 
              x = 0.5, y = 0.8,
              gp = gpar(fontsize = 24, fontface = "bold", fontfamily = "Helvetica"))
    
    # Sub title
    grid.text("Modelo de Régimen de Humedad del Suelo", 
              x = 0.5, y = 0.72,
              gp = gpar(fontsize = 12, fontfamily = "Helvetica"))
    
    # Station information
    grid.text(paste("Estación:", station_name), 
              x = 0.5, y = 0.6,
              gp = gpar(fontsize = 12, fontfamily = "Helvetica"))
    
    grid.text(paste("Ubicación: Lat", round(lat, 4), "° Lon", round(lon, 4), "°"), 
              x = 0.5, y = 0.55,
              gp = gpar(fontsize = 12, fontfamily = "Helvetica"))
    
    grid.text(paste("País:", country, "| Elevación:", round(elev, 1), "m"), 
              x = 0.5, y = 0.5,
              gp = gpar(fontsize = 12, fontfamily = "Helvetica"))
    
    grid.text(paste("Período de datos:", start_year, "-", end_year), 
              x = 0.5, y = 0.45,
              gp = gpar(fontsize = 12, fontfamily = "Helvetica"))
    
    grid.text(paste("Fecha del informe:", format(Sys.Date(), "%d/%m/%Y")), 
              x = 0.5, y = 0.4,
              gp = gpar(fontsize = 12, fontfamily = "Helvetica"))
    
    # Logo
    grid.text("NEWHALL MODEL v1.0", 
              x = 0.5, y = 0.1,
              gp = gpar(fontsize = 10, col = "gray70", fontfamily = "Helvetica"))
    
    # ========== Executive Summary ==========
    grid.newpage()
    pushViewport(viewport(width = 1, height = 1, x = 0.5, y = 0.5))
    
    grid.text("RESUMEN EJECUTIVO", 
              x = 0, y = 0.95,
              just = "left",
              gp = gpar(fontsize = 14, fontface = "bold", fontfamily = "Helvetica"))
    
    # Results 
    moisture_regime <- .jcall(results, "S", "getMoistureRegime")
    temperature_regime <- .jcall(results, "S", "getTemperatureRegime")
    annual_rainfall <- .jcall(results, "D", "getAnnualRainfall")
    annual_water_balance <- .jcall(results, "D", "getAnnualWaterBalance")
    water_holding <- .jcall(results, "D", "getWaterHoldingCapacity")
    
    # Crear tabla de resumen
    summary_data <- data.frame(
      Parámetro = c(
        "Régimen de Humedad",
        "Régimen de Temperatura",
        "Precipitación Anual (mm)",
        "Balance Hídrico Anual (mm)",
        "Capacidad de Retención (mm)",
        "Días Secos Acumulados",
        "Días Húmedos Acumulados"
      ),
      Valor = c(
        moisture_regime,
        temperature_regime,
        round(annual_rainfall, 1),
        round(annual_water_balance, 1),
        round(water_holding, 1),
        .jcall(results, "I", "getNumCumulativeDaysDry"),
        .jcall(results, "I", "getNumCumulativeDaysMoist")
      )
    )
    
    # Draw Table
    y_pos <- 0.8
    for (i in 1:nrow(summary_data)) {
      grid.text(summary_data$Parámetro[i], 
                x = 0.1, y = y_pos,
                just = "left",
                gp = gpar(fontsize = 11, fontfamily = "Helvetica"))
      
      grid.text(as.character(summary_data$Valor[i]), 
                x = 0.6, y = y_pos,
                just = "left",
                gp = gpar(fontsize = 11, fontface = "bold", fontfamily = "Helvetica"))
      
      y_pos <- y_pos - 0.05
    }
    
    # Interpretation
    grid.text("INTERPRETACIÓN:", 
              x = 0, y = 0.35,
              just = "left",
              gp = gpar(fontsize = 10, fontface = "bold", fontfamily = "Helvetica"))
    
    interpretation <- paste(
      "El modelo Newhall indica un régimen de humedad", tolower(moisture_regime),
      "con temperatura", tolower(temperature_regime), ".","\n",
      "La capacidad de retención de agua del suelo es de", round(water_holding, 1), "mm.","\n",
      "El balance hídrico anual es", ifelse(annual_water_balance > 0, "positivo", "negativo"),
      "con", round(annual_water_balance, 1), "mm."
    )
    
    grid.text(interpretation, 
              x = 0, y = 0.25,
              just = "left",
              gp = gpar(fontsize = 10, fontfamily = "Helvetica", lineheight = 1.2),
              hjust = 0, vjust = 1)
    
    # ========== Months Data ==========
    grid.newpage()
    
    # Title
    grid.text("DATOS CLIMÁTICOS MENSUALES", 
              x = 0.5, y = 0.95,
              gp = gpar(fontsize = 18, fontface = "bold", fontfamily = "Helvetica"))
    
    # Crear tabla con kable (si estuviera en Rmarkdown, pero haremos manual)
    y_pos <- 0.85
    
    # Encabezado de tabla
    grid.text("Mes", x = 0.15, y = y_pos, gp = gpar(fontsize = 11, fontface = "bold"))
    grid.text("Precip (mm)", x = 0.3, y = y_pos, gp = gpar(fontsize = 11, fontface = "bold"))
    grid.text("Temp (°C)", x = 0.45, y = y_pos, gp = gpar(fontsize = 11, fontface = "bold"))
    grid.text("PET (mm)", x = 0.6, y = y_pos, gp = gpar(fontsize = 11, fontface = "bold"))
    grid.text("Balance (mm)", x = 0.75, y = y_pos, gp = gpar(fontsize = 11, fontface = "bold"))
    
    y_pos <- y_pos - 0.04
    
    # Datos de la tabla
    for (i in 1:nrow(monthly_data)) {
      grid.text(monthly_data$Mes[i], x = 0.15, y = y_pos, gp = gpar(fontsize = 10))
      grid.text(round(monthly_data$Precipitacion[i], 1), x = 0.3, y = y_pos, gp = gpar(fontsize = 10))
      grid.text(round(monthly_data$Temperatura[i], 1), x = 0.45, y = y_pos, gp = gpar(fontsize = 10))
      grid.text(round(monthly_data$PET[i], 1), x = 0.6, y = y_pos, gp = gpar(fontsize = 10))
      
      # Colorear balance
      balance_color <- ifelse(monthly_data$Balance[i] > 0, "darkgreen", "darkred")
      grid.text(round(monthly_data$Balance[i], 1), 
                x = 0.75, y = y_pos, 
                gp = gpar(fontsize = 10, col = balance_color))
      
      y_pos <- y_pos - 0.04
    }
    
    # Totals yearly
    y_pos <- y_pos - 0.04
    grid.text("ANUAL", x = 0.15, y = y_pos, gp = gpar(fontsize = 10, fontface = "bold"))
    grid.text(round(sum(monthly_data$Precipitacion, na.rm = TRUE), 1), 
              x = 0.3, y = y_pos, gp = gpar(fontsize = 10, fontface = "bold"))
    grid.text(round(mean(monthly_data$Temperatura, na.rm = TRUE), 1), 
              x = 0.45, y = y_pos, gp = gpar(fontsize = 10, fontface = "bold"))
    grid.text(round(sum(monthly_data$PET, na.rm = TRUE), 1), 
              x = 0.6, y = y_pos, gp = gpar(fontsize = 10, fontface = "bold"))
    
    total_balance <- sum(monthly_data$Balance, na.rm = TRUE)
    balance_color <- ifelse(total_balance > 0, "darkgreen", "darkred")
    grid.text(round(total_balance, 1), 
              x = 0.75, y = y_pos, 
              gp = gpar(fontsize = 10, fontface = "bold", col = balance_color))
    
    # ========== GRAPHIC ==========
    # Gr 1: Precipitation & Temperature
    grid.newpage()
    
    # Crear gráfico con ggplot (necesitamos usar print)
    p1 <- ggplot(monthly_data, aes(x = factor(Mes, levels = months))) +
      geom_bar(aes(y = Precipitacion), stat = "identity", fill = "steelblue", alpha = 0.7) +
      geom_line(aes(y = Temperatura * 10, group = 1), color = "red", size = 1.5) +
      geom_point(aes(y = Temperatura * 10), color = "red", size = 2) +
      scale_y_continuous(
        name = "Precipitación (mm)",
        sec.axis = sec_axis(~./10, name = "Temperatura (°C)")
      ) +
      labs(title = "Precipitación y Temperatura Mensual", x = "Mes") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
    print(p1, vp = viewport(x = 0.5, y = 0.7, width = 0.9, height = 0.4))
    
    # Gráfico 2: Balance Hídrico
    p2 <- ggplot(monthly_data, aes(x = factor(Mes, levels = months), y = Balance, fill = Balance > 0)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "darkred"), guide = FALSE) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      labs(title = "Balance Hídrico Mensual (Precip - PET)", x = "Mes", y = "Balance (mm)") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
    print(p2, vp = viewport(x = 0.5, y = 0.25, width = 0.9, height = 0.4))
    
    # ========== Indicators ==========
    grid.newpage()
    
    grid.text("RESUMEN DE INDICADORES NEWHALL", 
              x = 0.5, y = 0.95,
              gp = gpar(fontsize = 18, fontface = "bold", fontfamily = "Helvetica"))
    
    # Lista de todos los indicadores en orden vertical
    all_indicators <- c(
      "BALANCES HÍDRICOS:",
      paste("  • Balance Anual:", round(.jcall(results, "D", "getAnnualWaterBalance"), 1), "mm"),
      paste("  • Balance Estival:", round(.jcall(results, "D", "getSummerWaterBalance"), 1), "mm"),
      paste("  • Precipitación Anual:", round(.jcall(results, "D", "getAnnualRainfall"), 1), "mm"),
      paste("  • Capacidad Retención:", round(.jcall(results, "D", "getWaterHoldingCapacity"), 1), "mm"),
      "",
      "DÍAS ACUMULADOS:",
      paste("  • Días Secos:", .jcall(results, "I", "getNumCumulativeDaysDry")),
      paste("  • Días Húmedos:", .jcall(results, "I", "getNumCumulativeDaysMoist")),
      paste("  • Días Secos/Húmedos:", .jcall(results, "I", "getNumCumulativeDaysMoistDry")),
      paste("  • Días Secos (>5°C):", .jcall(results, "I", "getNumCumulativeDaysDryOver5C")),
      paste("  • Días Húmedos (>5°C):", .jcall(results, "I", "getNumCumulativeDaysMoistOver5C")),
      paste("  • Días Secos/Húmedos (>5°C):", .jcall(results, "I", "getNumCumulativeDaysMoistDryOver5C")),
      "",
      "DÍAS CONSECUTIVOS:",
      paste("  • Días Húmedos Consecutivos:", .jcall(results, "I", "getNumConsecutiveDaysMoistInSomeParts")),
      paste("  • Días Húmedos Consec. (>8°C):", .jcall(results, "I", "getNumConsecutiveDaysMoistInSomePartsOver8C")),
      "",
      "INDICADORES ESTACIONALES:",
      paste("  • Días Secos post-Solsticio Verano:", .jcall(results, "I", "getDryDaysAfterSummerSolstice")),
      paste("  • Días Húmedos post-Solsticio Invierno:", .jcall(results, "I", "getMoistDaysAfterWinterSolstice"))
    )
    
    # Dibujar lista vertical
    y_pos <- 0.85
    for (i in seq_along(all_indicators)) {
      line <- all_indicators[i]
      
      if (line == "") {
        y_pos <- y_pos - 0.015  # Espacio extra para líneas vacías
        next
      }
      
      # Estilo diferente para títulos de categoría
      if (grepl(":$", line)) {
        grid.text(line, 
                  x = 0.1, y = y_pos,
                  just = "left",
                  gp = gpar(fontsize = 12, fontface = "bold", fontfamily = "Helvetica", col = "darkblue"))
      } else {
        grid.text(line, 
                  x = 0.12, y = y_pos,  # Sangría para items
                  just = "left",
                  gp = gpar(fontsize = 10, fontfamily = "Helvetica"))
      }
      
      y_pos <- y_pos - 0.025
    }
    
    # ========== Recommendations (In Constructions) ==========
    grid.newpage()
    
    grid.text("RECOMENDACIONES Y CONSIDERACIONES", 
              x = 0.5, y = 0.95,
              gp = gpar(fontsize = 18, fontface = "bold", fontfamily = "Helvetica"))
    
    # Recomendaciones basadas en el régimen
    recommendations <- list(
      "RECOMENDACIONES AGRÍCOLAS" = c(
        "• Seleccionar cultivos adaptados al régimen de humedad identificado",
        "• Planificar épocas de siembra según el calendario de humedad",
        "• Considerar sistemas de riego complementario según el balance hídrico",
        "• Implementar prácticas de conservación de agua en el suelo"
      ),
      "MANEJO DEL SUELO" = c(
        "• Ajustar labranza según la capacidad de retención de agua",
        "• Implementar coberturas vegetales para reducir evaporación",
        "• Considerar enmiendas orgánicas para mejorar retención",
        "• Planificar rotaciones de cultivos según disponibilidad hídrica"
      ),
      "CONSIDERACIONES CLIMÁTICAS" = c(
        "• Monitorear cambios en el patrón de precipitación",
        "• Evaluar riesgos de sequía según días secos acumulados",
        "• Considerar variabilidad interanual en la planificación",
        "• Implementar sistemas de alerta temprana para eventos extremos"
      )
    )
    
    y_pos <- 0.85
    for (category in names(recommendations)) {
      grid.text(category, 
                x = 0.1, y = y_pos,
                just = "left",
                gp = gpar(fontsize = 12, fontface = "bold", fontfamily = "Helvetica"))
      
      y_pos <- y_pos - 0.03
      
      for (rec in recommendations[[category]]) {
        grid.text(rec, 
                  x = 0.12, y = y_pos,
                  just = "left",
                  gp = gpar(fontsize = 10, fontfamily = "Helvetica"))
        y_pos <- y_pos - 0.025
      }
      y_pos <- y_pos - 0.02
    }
    
    # Pie de página final
    grid.text("Informe generado automáticamente por el sistema Newhall Model v1.0", 
              x = 0.5, y = 0.05,
              gp = gpar(fontsize = 9, col = "gray50", fontfamily = "Helvetica"))
    
    grid.text(paste("Archivo:", output_file), 
              x = 0.5, y = 0.02,
              gp = gpar(fontsize = 8, col = "gray50", fontfamily = "Helvetica"))
    
    # Cerrar PDF
    dev.off()
    
    cat("PDF exportado exitosamente a:", output_file, "\n")
    return(TRUE)
  }



# Usos:

# exportNewhallToXMLF(resl_newhall, input_direct, output_file = "resultado_newhall.pdf"

# exportNewhallToPDF(resl_newhall, input_direct, output_file = "resultado_newhall.pdf")

###############################################################################

# Data input ##################################################

## Stations ###

stations_soil <- as.data.frame(st_read("Data/vector/sites_9377.shp",quiet = TRUE))
  
## data soil SCHS:
  df_schs <- data.frame(
    ID_perfil = c("ARBORADA", "CARIMAGUA", "LALIBERTAD", "MAVALLE", 
                  "NUBES", "SHEEFER", "TALUMA", "VEGAS", "YUCAO"),
    CAD_SCHS = c(50, 50, 50, 50,
                 50, 50, 50, 50, 
                 50)
  )
  
  
## Climatic ####

## Climate annual #option 1  ####

  # Precipitation
  
  ppt <- read.csv("Data/clima_P_2023_2024.csv", sep=";")
  
  ppt$year <- substr(ppt$periodo,1,4)
  ppt$month <- substr(ppt$periodo,6,7)
  
  # Temperature
  
  tmed <- read.csv("Data/clima_T_2023_2024.csv")

  ## 2023-10 -- 2024-10 #option 2 ####
  ppt <- read.csv("Data/clima_P_2023_2024.csv", sep=";")
  ppt$year <- substr(ppt$periodo,1,4)
  ppt$month <- substr(ppt$periodo,6,7)
  
  ppt$year <- as.integer(ppt$year)
  ppt$month <- as.integer(ppt$month)
  
  ppt <- ppt %>% 
    filter(
      (year == 2023 & month >= 10) |   # Desde octubre 2023
        (year == 2024 & month < 10)     # Hasta octubre 2024
    )
  
  tmed <- read.csv("Data/clima_T_2023_2024.csv")
  
  tmed <- tmed %>%
    filter(
      (year == 2023 & month >= 10) |   # Desde octubre 2023
        (year == 2024 & month < 10)     # Hasta octubre 2024
    )
  
  # unmarked line 605 in function exportpdf, for month 0
  
  
## Climatic Normal #option 3 ####

  # ppt (mm)
  
  ppt <- read.csv("Data/clima_P_1981_2020.csv", sep = ";")
  names(ppt)[names(ppt) == "var_monthly"] <- "precip_mensual"
  names(ppt)[names(ppt) == "Id"] <- "nombre"
  
  # T °C
  
  tmed <- read.csv("Data/clima_Tmed_1981_2020.csv", sep = ";")
  names(tmed)[names(tmed) == "var_monthly"] <- "T_med"


  
# Data configuration ####
  
  # newhall by year (for normal year, in table put the last year, eg. 1981-2020, year 2020)
  
  # year begin
  
  pdbegin   = 2020
  
  # year end 
  
  pdend     = 2020
  
  
  # for Newhall year combined # 1  (eg. 2023_2024, Option 2) for yes, default 0
  
  year_combined = 0
  
  
  # available water capacity
  
    # awc for all
  
     awc       <- 200  #default 200 
    
    # awc for sites
    
    # awc <- df_schs$CAD_SCHS
     
  
  # export results to xml: 1:yes, 0: no
  
  exp_xml = 0
  
  # export results to pdf 1:yes, 0: no
  
  exp_pdf = 0
  

# Run simulations ########################################################

  year <- pdend

  stationName <- unique(df_schs$ID_perfil)
  
  results_list <- list()
  
for (i in seq_along(stationName)) {
  
  namesatation <- stationName[i]
   if (length(awc) > 1){
     awc_l <- awc[i]
   } else {
     awc_l = awc
   }
  
  lat <- as.numeric(subset(stations_soil, Id == namesatation, "Latitude"))
  long <- as.numeric(subset(stations_soil, Id == namesatation, "Longitude"))
  
  cat(paste0("\nProcesando estación ", i, " de ", length(stationName), ": ", namesatation, "\n"))
  
  
  
  if ( year_combined == 0 ) {
    
    # dataset newhall
    
    input_direct <- NewhallDataset(
      stationName = namesatation,
      country = "Col",
      latDD = lat,
      lonDD = long,
      elev = 300,
      allPrecipsDbl = ppt[ppt$nombre == namesatation & ppt$year == year, "precip_mensual"],  #ajuste temp
      allAirTempsDbl = tmed[tmed$Id == namesatation & tmed$year == year, "T_med"],
      pdbegin = pdbegin,
      pdend = pdbegin,
      smcsawc = awc_l
    )
    
  } else {
    
    # order data
    ppt_order <- ppt[ppt$nombre == namesatation & (ppt$year == pdbegin |
                                               ppt$year == pdend), "precip_mensual"]
    #ppt_order <- ppt_order[c(4, 5, 6, 1:9)]   # 4 is 2024_01; 2023
    
    tmed_order <- tmed[tmed$Id == namesatation & (tmed$year == pdbegin |
                                                    tmed$year == pdend),"T_med"]
    #tmed_order <- tmed_order[c(4, 5, 6, 1:9)]
    
    # dataset newhall year combined
    input_direct <- NewhallDataset(
      stationName = namesatation,
      country = "Col",
      latDD = lat,
      lonDD = long,
      elev = 300,
      allPrecipsDbl = ppt_order,
      allAirTempsDbl = tmed_order,
      pdbegin = pdbegin,
      pdend = pdbegin,
      smcsawc = awc_l
    )
    
  }
 
  
  # Newhall model simulation
  resl_newhall <- newhall_simulation(input_direct,
                                     smcsawc = awc_l,  
                                     soilAirOffset = 2.5,
                                     amplitude = 0.66)
  
  # export to XML
  if (exp_xml == 1){
    exportNewhallToXML(resl_newhall,
                       input_direct, 
                       paste0("resultados_newhall_awc",round(awc_l,0),"mm_", year, "_", namesatation, ".xml"),
                       to_metric = TRUE)
    } else {
      print ("No XML")
      }
  
    
  if (exp_pdf == 1){
    
    exportNewhallToPDF(resl_newhall,
                       input_direct, 
                       output_file = paste0("resultados_newhall_awc_",round(awc_l,0),"mm_", year, "_", namesatation, ".pdf"),
                       to_metric = TRUE)
  } else {
    
    print ("No PDF")
    
    }
  
  
  #  dataframe for station
  
  station_df <- data.frame(
    station_id = namesatation,
    year = year,
    latitude = lat,
    longitude = long,
    annual_rainfall = .jcall(resl_newhall, "D", "getAnnualRainfall"),
    annual_water_balance = .jcall(resl_newhall, "D", "getAnnualWaterBalance"),
    summer_water_balance = .jcall(resl_newhall, "D", "getSummerWaterBalance"),
    water_holding_capacity = .jcall(resl_newhall, "D", "getWaterHoldingCapacity"),
    moisture_regime = .jcall(resl_newhall, "Ljava/lang/String;", "getMoistureRegime"),
    temperature_regime = .jcall(resl_newhall, "Ljava/lang/String;", "getTemperatureRegime"),
    regime_subdivision1 = .jcall(resl_newhall, "Ljava/lang/String;", "getRegimeSubdivision1"),
    regime_subdivision2 = .jcall(resl_newhall, "Ljava/lang/String;", "getRegimeSubdivision2"),
    num_cumulative_days_moist = .jcall(resl_newhall, "I", "getNumCumulativeDaysMoist"),
    num_cumulative_days_dry = .jcall(resl_newhall, "I", "getNumCumulativeDaysDry"),
    num_cumulative_days_moist_over5C = .jcall(resl_newhall, "I", "getNumCumulativeDaysMoistOver5C"),
    num_cumulative_days_dry_over5C = .jcall(resl_newhall, "I", "getNumCumulativeDaysDryOver5C"),
    num_cumulative_days_moist_dry = .jcall(resl_newhall, "I", "getNumCumulativeDaysMoistDry"),
    num_cumulative_days_moist_dry_over5C = .jcall(resl_newhall, "I", "getNumCumulativeDaysMoistDryOver5C"),
    num_consecutive_days_moist = .jcall(resl_newhall, "I", "getNumConsecutiveDaysMoistInSomeParts"),
    num_consecutive_days_moist_over8C = .jcall(resl_newhall, "I", "getNumConsecutiveDaysMoistInSomePartsOver8C"),
    dry_days_after_summer_solstice = .jcall(resl_newhall, "I", "getDryDaysAfterSummerSolstice"),
    moist_days_after_winter_solstice = .jcall(resl_newhall, "I", "getMoistDaysAfterWinterSolstice"),
    stringsAsFactors = FALSE
  )
  
  # Agregar el dataframe de esta estación a la lista
  
  results_list[[i]] <- station_df
  
  cat(paste0("Estación ", namesatation, " completada\n"))
  
}
  
# All Results 
 
newhall_df <- do.call(rbind, results_list)

  
  # Results
  
  cat(paste0("\n\nPROCESO COMPLETADO\n"))
  cat(paste0("Total estaciones: ", nrow(newhall_df), "\n"))
  cat(paste0("Año: ", year, "\n\n"))
  
  
  # save results
  
  # write.csv(newhall_df, paste0("newhall_results_awc_mm_xx", year, ".csv"), row.names = FALSE)
  
  # cat(paste0("newhall_results_awc_", year, ".csv\n"))
  
  

 