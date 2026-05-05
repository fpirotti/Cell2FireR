

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
  ign_indices   <- grep("ignition cell:", lines)
  burnt_indices <- grep("^\\s+Burnt\\s+", lines)
  # 3. Extract just the numbers from those specific lines
  #    Using simple regex just to pull the digits
  sim_ids      <- as.numeric(str_extract(lines[sim_indices  ], "\\d+"))
  ign_cells    <- as.numeric(str_extract(lines[ign_indices  ], "\\d+"))
  burnt_counts <- as.numeric(str_extract(lines[burnt_indices ], "\\d+"))
  
  # 4. Bind them into a data frame
  dd <- list(
    simulation = sim_ids,
    ignition_cell = ign_cells,
    burnt_cells = burnt_counts
  )
  
  if(length(unique(sapply(dd, length)))!=1 || length(dd$simulation)==0){
    stop(errorCondition(
      paste0(
      "
<br>Number of simulations: ", length(dd$simulation) ,". ",
      "
<br>Number of ignition cells: ", length(dd$ignition_cell), ". ", 
      "
<br>Number of burnt cells values: ", length(dd$burnt_cells), ". <br>"
      ) ))
  }
  
  as.data.frame(dd)
}
