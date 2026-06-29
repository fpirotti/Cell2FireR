#' Parse and Format Weather Data for Cell2Fire
#'
#' @description
#' Cleans, formats, and strictly types user-uploaded weather CSV data to prevent 
#' backend C++ engine crashes in the Cell2Fire simulator. 
#'
#' @param df A `data.frame` containing the raw weather data. Must contain a 
#' `datetime` column (or individual year, month, day, hour columns).
#' @param model_type A character string specifying the fire behavior model engine. 
#' Valid options are `"C"` (Canadian FBP System), `"S"` (Scott & Burgan 2005), 
#' or `"K"` (Kitral model). Defaults to `"C"`.
#'
#' @return A cleaned and strictly formatted `data.frame` containing only the necessary 
#' columns in the exact sequence expected by the selected Cell2Fire model.
#'  
#' @export
parse_weather_for_cell2fire <- function(df, model_type = c("C", "S", "K")) {
  # Match the single letter model type ("C", "S", or "K")
  if(!is.data.frame(df)){
    if(file.exists(df)){
      df <- read.csv(df)
    } else {
      stop(df, " does not seem to be a file")
    }
  }
  if(!is.data.frame(df)){ 
      stop(df, " does not seem to be readable") 
  }
  model_type <- match.arg(model_type)
 
  # 1. Normalize column names (lowercase and strip whitespace)
  # This makes 'Datetime', 'DATETIME', and 'datetime' all safely evaluate as 'datetime'
  colnames(df) <- tolower(trimws(colnames(df)))
  
    
  # 2. Map aliases for weather columns (also ensuring variants of datetime map to "datetime")
  df <- df |> 
    dplyr::rename_with(~dplyr::case_when(
      . %in% c("windspeed", "wind_speed", "wspeed") ~ "ws",
      . %in% c("winddirection", "wind_dir", "wind_direction", "wdir") ~ "wd",
      . %in% c("temperature", "temp_c") ~ "temp",
      . %in% c("relativehumidity", "humidity", "rhum") ~ "rh",
      . %in% c("precipitation", "precip", "rain") ~ "prec",
      . %in% c("date_time", "timestamp", "time", "date") ~ "datetime", 
      TRUE ~ .
    ))
  
  # 3. Handle Dates/Times: Extract Year, Month, Day, Hour from the 'datetime' column
  if ("datetime" %in% colnames(df)) {
    # Parse the text strings into valid datetime objects using lubridate
    parsed_dates <- suppressWarnings(
      lubridate::parse_date_time(df$datetime, orders = c("mdY HMS","mdy HM", "Ymd HMS", "Ymd HM", "dmy HMS", "dmy HM", "mdY HMS", "mdY HM", "Ymd", "dmy", "mdY"))
    )
    if(anyNA(parsed_dates)){
      warning("Could not read datetime column as dates")
      parsed_dates <- df$datetime
    }
  } else {
    parsed_dates <- 1:nrow(df)
  }
 
  
  base_cols <- c("scenario", "datetime")
  for(col in base_cols) {
    if(!col %in% colnames(df)) {
      df[[col]] <- switch(col, 
                          "scenario" = df$scenario, 
                          "datetime" = parsed_dates )
    }
  }
  # 4. Define Expected Columns and Safe Defaults depending on the Simulator
  if (model_type == "C") {
    # Canadian Model (C)
    weather_cols <- c("apcp", "temp", "rh","ws", "wd",  "ffmc", "dmc", "dc", "isi", "bui", "fwi")
    defaults <- list(apcp=0, temp=20, rh=50, ws=0, wd=0, ffmc=85, dmc=10, dc=15, isi=0, bui=0, fwi=0)
    
  } else if (model_type == "S") {
    # Scott & Burgan Model (S)
    weather_cols <- c("ws", "wd")
    defaults <- list(ws=0, wd=0)
    
  } else if (model_type == "K") {
    # Kitral Model (K)
    weather_cols <- c("ws", "wd", "temp", "rh" )
    defaults <- list(ws=0, wd=0, temp=20, rh=50 )
  }
  
  # 5. Strict Parsing for Meteorological & Fuel Columns
  for(col in weather_cols) {
    if(!col %in% colnames(df)) {
      df[[col]] <- defaults[[col]]
      message("Missing column ", col, " putting default values")
    } else {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
      df[[col]][is.na(df[[col]])] <- defaults[[col]]
    }
  }
  
  # 6. Final Construction: Bind explicitly in the sequence the C++ Engine expects
  final_order <- c(base_cols, weather_cols)
  final_df <- df[, final_order, drop = FALSE]
  
  return(final_df)
}