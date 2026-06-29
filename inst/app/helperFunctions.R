#' Esporta un DataFrame Meteo nel Formato RAWS (.wxs)
#'
#' @param df Un data.frame contenente i dati meteorologici dell'app. Deve contenere una colonna `datetime` 
#'           (oppure colonne separate year, month, day, hour) e le variabili ws, wd, temp, rh, prec.
#' @param output_path Stringa. Il percorso completo in cui salvare il file (es. "data/output_weather.wxs").
#' @param elevation Numerico. L'elevazione della stazione RAWS (default: 2).
#' @param units Stringa. Le unità di misura, tipicamente "Metric" o "English" (default: "Metric").
#'
#' @return Nessun valore di ritorno. Scrive direttamente il file sul disco.
export_weather_to_wxs <- function(df, output_path, elevation = 2, units = "Metric") {
  
  # 1. Normalizzazione dei nomi delle colonne per sicurezza
  colnames(df) <- tolower(trimws(colnames(df)))
  
  # 2. Se i dati usano la colonna unica 'datetime', estraiamo i singoli componenti
  if ("datetime" %in% colnames(df) && !all(c("year", "month", "day", "hour") %in% colnames(df))) {
    parsed_dates <- suppressWarnings(anytime::anytime(df$datetime, asUTC = FALSE))
    df$year  <- as.integer(format(parsed_dates, "%Y"))
    df$month <- as.integer(format(parsed_dates, "%m"))
    df$day   <- as.integer(format(parsed_dates, "%d"))
    df$hour  <- as.integer(format(parsed_dates, "%H"))
  }
  
  # 3. Mappatura e Fallback delle variabili meteorologiche necessarie per il formato RAWS
  # Se mancano o hanno nomi leggermente diversi (es. TMP invece di TEMP) le allineiamo
  df <- df %>% 
    dplyr::rename_with(~dplyr::case_when(
      . %in% c("temperature", "temp_c", "tmp") ~ "temp",
      . %in% c("relativehumidity", "humidity", "rhum") ~ "rh",
      . %in% c("windspeed", "wind_speed", "wspeed") ~ "ws",
      . %in% c("winddirection", "wind_dir", "wdir") ~ "wd",
      . %in% c("precipitation", "precip", "rain") ~ "prec",
      TRUE ~ .
    ))
  
  # Controllo variabili meteorologiche obbligatorie con valori di default se assenti
  required_cols <- list(temp = 20, rh = 50, prec = 0.0, ws = 0, wd = 0, cloudcov = 0)
  for (col in names(required_cols)) {
    if (!col %in% colnames(df)) {
      df[[col]] <- required_cols[[col]]
    } else {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
      df[[col]][is.na(df[[col]])] <- required_cols[[col]]
    }
  }
  
  # 4. Formattazione specifica per il file .wxs
  # - L'ora deve essere espressa a 4 cifre (es. 8 -> "0800", 13 -> "1300")
  # - La precipitazione oraria (HrlyPcp) richiede 2 cifre decimali fisse (es. "0.00")
  df_formatted <- df %>%
    dplyr::transmute(
      Year = as.integer(year),
      Mth  = as.integer(month),
      Day  = as.integer(day),
      Time = sprintf("%02d00", as.integer(hour)),
      Temp = round(temp),
      RH   = round(rh),
      HrlyPcp = sprintf("%.2f", prec),
      WindSpd = round(ws),
      WindDir = round(wd),
      CloudCov = as.integer(cloudcov)
    )
  
  # 5. Scrittura del File con Intestazioni e Metadati RAWS
  # Usiamo un file connection per scrivere riga per riga l'header personalizzato
  con <- file(output_path, "w", encoding = "UTF-8")
  
  tryCatch({
    # Scrittura dei metadati RAWS richiesti in testa al file
    writeLines(paste0("RAWS_UNITS: ", units), con)
    writeLines(paste0("RAWS_ELEVATION: ", elevation), con)
    writeLines(paste0("RAWS: ", nrow(df_formatted)), con)
    
    # Scrittura dell'intestazione delle colonne separata da spazi tabulati
    header_line <- "Year\tMth\tDay\tTime\tTemp\tRH\tHrlyPcp\tWindSpd\tWindDir\tCloudCov"
    writeLines(header_line, con)
    
    # Scrittura delle righe dati usando la tabulazione (\t) come separatore di colonna
    for (i in 1:nrow(df_formatted)) {
      data_line <- paste(df_formatted[i, ], collapse = "\t")
      writeLines(data_line, con)
    }
    
    message("File .wxs generato con successo in: ", output_path)
  }, finally = {
    # Chiusura sicura della connessione del file
    close(con)
  })
}


getDateTimeFromCSV<-function(x){
 
  if(!is.data.frame(x)){
    x <- read.csv(x ) 
  }
 
  x <- na.omit(x)
  header <- names(x) 
 
  datecol <- grep("date", header, ignore.case = TRUE) # Case-insensitive match
  
  if(length(datecol)==1){
    datecol <- datecol[1]  
    # Read just that column to save memory/time
    wf_col <- x[, datecol]
    clean_ts <- anytime::anytime(wf_col, asUTC = FALSE)
    
    # 3. FALLBACK: If anytime fails (e.g., 23/04/2026), hunt by explicit order
    if (anyNA(clean_ts)) {
      # Try Day-Month-Year variants (Common international format)
      clean_ts <- lubridate::dmy_hms(wf_col, quiet = TRUE)
      if (anyNA(clean_ts)) clean_ts <- lubridate::dmy_hm(wf_col, quiet = TRUE)
      if (anyNA(clean_ts)) clean_ts <- lubridate::dmy(wf_col, quiet = TRUE)
      
      # Try Year-Month-Day variants (ISO format)
      if (anyNA(clean_ts)) clean_ts <- lubridate::ymd_hms(wf_col, quiet = TRUE)
      if (anyNA(clean_ts)) clean_ts <- lubridate::ymd_hm(wf_col, quiet = TRUE)
      if (anyNA(clean_ts)) clean_ts <- lubridate::ymd(wf_col, quiet = TRUE)
      
      # Try Month-Day-Year variants (US format)
      if (anyNA(clean_ts)) clean_ts <- lubridate::mdy_hms(wf_col, quiet = TRUE)
      if (anyNA(clean_ts)) clean_ts <- lubridate::mdy_hm(wf_col, quiet = TRUE)
      if (anyNA(clean_ts)) clean_ts <- lubridate::mdy(wf_col, quiet = TRUE)
    }
      
    if(anyNA(clean_ts)) {
      showNotification(paste0(
        "Sorry, we did find a column named '",
        header[[datecol]] ,"' BUT we could not convert 
          its contents to a time stamp using various heuristics.
  <br> The following timestamp format is advised and recognized:
  <br>2023-07-07 16:00:00
  <br>2023-07-07 16:00
  <br>2023-07-07 16 
<br>We gave a generic 1 hour time lapse for each line."), duration=15 , id="datecolumnMixMatch"    
      )
      
      times <- 1:(nrow(wf)+2)
      
    } else {
      if(hour(clean_ts[[1]])==0){
        clean_ts <- clean_ts+3600*12
      }
      times <- c(clean_ts[[1]]-3600, 
                 clean_ts, 
                 clean_ts[[length(clean_ts)]]+3600  ) 
      
    }
    
  } else { 
    showNotification(paste0(
      "Sorry, we did NOT find a column with name 'date'.
<br>We gave a generic 1 hour time lapse for each line."), duration=15 , id="datecolumnMixMatch"    
    )
    times <- 1:(nrow(wf)+2)
  }
  times
  
}

#' center
#' @description
#' Reads raster and get center of raster
#' 
#'
#' @param r raster in terra format
#'
#' @returns
#' @export
#'
#' @examples
center <- function(r){
  e <- terra::ext(r)
 
  list(
    lng = (e$xmin + e$xmax) / 2,
    lat = (e$ymin + e$ymax) / 2
  )
}
add_suffix <- function(x, suffix) {
  sub("(\\.[^.]+)$", paste0(suffix, "\\1"), x)
}
is_all_caps <- function(x) {
  x == toupper(x)
}
popup_text <- function(name, value=NULL) {
  pp<-paste0("<b>", name, "</b>")
  if(!is.null(value)) pp<-paste0("<b>", name, ":</b> ", value)
  pp
}

checkAPI <- function(url){ 
  res <- httr::GET(url) 
  httr::status_code(res)
}

