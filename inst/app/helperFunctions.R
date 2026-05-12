

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


parse_fire_log <- function(log_text) {
  
  # 1. Read all lines into a character vector
  lines <- log_text
  
  # 2. Find the row indices (line numbers) for the data we want
  sim_indices   <- grep("Simulation \\d+ Start:", lines)
  ignitionsN <- as.integer(gsub("\\D+", "", lines[sim_indices]))
  if(anyNA(ignitionsN)){
    stop(errorCondition("We have NAs in simulation start ignition iterations!"))
  }
 
  ign_cells   <- trimws(gsub("ignition cell: ", "", lines[sim_indices+2]))
 
  burnt_indices <- grep("^Simulation\\s+\\d+\\s+Results:", lines)
  burnt_ignitionsN <- as.numeric(str_extract(lines[burnt_indices ], "\\d+"))
 
  burnt_tables <- lapply(which(burnt_ignitionsN%in%ignitionsN), 
                         function(x){  
                           indices <- (burnt_indices[[x]]+3) : (burnt_indices[[x]]+7)
                           data_lines <- lines[ indices ]
                           data_lines <- gsub("^\t", "", data_lines)
                           data_lines_csv <- gsub(" {2,}", ",", data_lines)
                           df <- read.csv(text = data_lines_csv, header = FALSE)
                           colnames(df) <- c("Cell Status", "Count", "Percent")
                        
                           kable(df, format = "html", table.attr = "class='table table-striped table-condensed table-sm'")
                            
                         })
 
  
  # 4. Bind them into a data frame
  dd <- list(
    simulation = as.integer(ignitionsN),
    ignition_cell = as.integer(ign_cells),
    burnt_cells = as.character(burnt_tables)
  )
  
  if(length(unique(sapply(dd, length)))!=1 || length(dd$simulation)==0){
    showNotification(
      paste0(
      "
<br>Number of simulations: ", length(dd$simulation) ,". ",
      "
<br>Number of ignition cells: ", length(dd$ignition_cell), ". ", 
      "
<br>Number of burnt cells values: ", length(dd$burnt_cells), ". <br>"
       ), type="warning", duration=19 )
  }
  
  dd
}
