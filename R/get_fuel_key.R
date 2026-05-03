#' get_fuel_key
#' @description
#' Map fuel strings to Cell2Fire single-character flags
#' 
#' @param fuel_string Character string representing the fuel model.
#' @returns A single character ("S", "K", "C", or "P"). Defaults to "S".
get_fuel_key <- function(fuel_string) {
  # 1. Handle missing or empty inputs early
  if (is.null(fuel_string) || is.na(fuel_string) || !nzchar(trimws(fuel_string))) {
    return("S") 
  }
  
  # 2. Try Exact Match First (Fastest route for standard UI inputs)
  fuel_map <- c(
    "0. Scott & Burgan" = "S",
    "1. Kitral" = "K",
    "2. Canada FBP" = "C",
    "3. Portugal" = "P"
  )
 
  mapped_val <- unname(fuel_map[fuel_string])

  if (is.null(mapped_val) || is.na(mapped_val)) {
    mapped_val <- which(fuel_map==fuel_string)
    if(length(mapped_val)==1)  mapped_val <- fuel_string
    else mapped_val <- NULL
  }
  
  if (!is.null(mapped_val) && !is.na(mapped_val)) {
    return(mapped_val)
  }
  
  # 3. Heuristic / Fuzzy Matching
  # Normalize the string: lowercase and trim trailing/leading spaces
  lower_str <- tolower(trimws(fuel_string))
  
  # Check for identifying keywords using regex
  if (grepl("kitral", lower_str)) {
    return("K")
  } else if (grepl("canada|fbp", lower_str)) {
    return("C")
  } else if (grepl("portugal", lower_str)) {
    return("P")
  } else if (grepl("scott|burgan|s&b", lower_str)) {
    return("S")
  }
  
  # 4. Fallback to "S" if no heuristics match, mirroring C++ default behavior
  return("S")
}