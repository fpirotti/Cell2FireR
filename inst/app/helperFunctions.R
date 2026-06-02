getDateTimeFromCSV<-function(x){
 
  if(!is.data.frame(x)){
    x <- read.csv(x ) 
  }
 
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
        names(wf)[[datecol]] ,"' BUT we could not convert 
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

