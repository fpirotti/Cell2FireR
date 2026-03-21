#' Dictionary
#' Reads fbp_lookup_table.csv and creates dictionaries for fuel types and colors
#' @param filename file path with lookup table
#'
#' @returns list with rows and colors
#' @importFrom  utils read.csv
Dictionary <- function(filename) {
  # Read the CSV
  data <- utils::read.csv(filename, stringsAsFactors = FALSE)

  # Initialize lists (acting as dictionaries)
  row_dict <- list()
  colors_dict <- list()

  for (i in 1:nrow(data)) {
    line <- as.character(data[i, ])
    key <- as.character(line[1])
    fuel_code <- gsub("-", "", line[4])
    fuel_code <- gsub("No", "NF", fuel_code)

    # Logic for O1a/O1b or others
    if (substr(fuel_code, 1, 3) %in% c("O1a", "O1b")) {
      row_dict[[key]] <- substr(fuel_code, 1, 3)
    } else {
      row_dict[[key]] <- substr(fuel_code, 1, 2)
    }

    # Colors normalized to 0-1
    colors_dict[[key]] <- c(
      as.numeric(line[5]) / 255.0,
      as.numeric(line[6]) / 255.0,
      as.numeric(line[7]) / 255.0,
      1.0
    )
  }
  return(list(row = row_dict, colors = colors_dict))
}

# Reads the Forest.asc grid structure
#' ForestGrid
#' Reads the ASCII file with the forest grid structure and returns an array with all the cells and grid dimensions nxm
#' Modified Feb 2018 by DLW to read the forest params (e.g. cell size) as well
#' @param filename file path with ASCII file with the forest grid structure
#' @param dict_obj  fbp_lookup table converted to a list
#'
#' @returns  list with all the cells and grid dimensions nxm
ForestGrid <- function(filename, dict_obj) {
  file_lines <- readLines(filename)

  # Get cellsize from line 5 (index 5 in R)
  cellsize_line <- file_lines[5]
  parts <- unlist(strsplit(trimws(cellsize_line), "\\s+"))
  if (parts[1] != "cellsize") {
    stop(paste("Expected cellsize on line 5 of", filename))
  }
  cellsize <- as.numeric(parts[2])

  gridcell3 <- c() # Numeric IDs
  gridcell4 <- c() # Fuel Type Strings

  # Data starts from line 7 (index 7 in R)
  for (i in 7:length(file_lines)) {
    line_vals <- unlist(strsplit(trimws(file_lines[i]), "\\s+"))
    for (val in line_vals) {
      if (!(val %in% names(dict_obj$row))) {
        gridcell3 <- c(gridcell3, NA)
        gridcell4 <- c(gridcell4, "NData")
      } else {
        gridcell3 <- c(gridcell3, as.integer(val))
        gridcell4 <- c(gridcell4, dict_obj$row[[val]])
      }
    }
  }

  # Calculate rows/cols based on input file structure
  # Assuming standard ASCII grid where row length is consistent
  num_rows <- length(file_lines) - 6
  num_cols <- length(gridcell3) / num_rows

  return(list(ftype_n = gridcell3, ftype = gridcell4, rows = num_rows, cols = num_cols, cellsize = cellsize))
}

# Reads ASCII files for Elevation, Slope, etc.
#' Title
#'
#' @param in_folder folder path with ESRI ASCII raster files
#' @param n_cells number of cells
#'
#' @returns a list of vectors with elevazion, slope etc...
DataGrids <- function(in_folder, n_cells) {
  filenames <- c("elevation.asc", "saz.asc", "slope.asc", "cur.asc")
  results <- list(Elevation = rep(NA, n_cells), SAZ = rep(NA, n_cells),
                  PS = rep(NA, n_cells), Curing = rep(NA, n_cells))

  for (name in filenames) {
    ff <- file.path(in_folder, name)
    if (file.exists(ff)) {
      file_lines <- readLines(ff)
      # Extract data from line 7 onwards
      data_str <- paste(file_lines[7:length(file_lines)], collapse = " ")
      vals <- as.numeric(unlist(strsplit(trimws(data_str), "\\s+")))

      if (name == "elevation.asc") results$Elevation <- vals
      if (name == "saz.asc") results$SAZ <- vals
      if (name == "slope.asc") results$PS <- vals
      if (name == "cur.asc") results$Curing <- vals
    } else {
      message(paste("No", name, "file, filling with NaN"))
    }
  }
  return(results)
}

#' GenerateDat
#' Generates the final Data.csv
#' @param g_fuel_type numeric vector with fuel type code
#' @param elev numeric vector with elevation
#' @param ps numeric vector with slope
#' @param saz numeric vector with saz (azimuth in degrees)
#' @param curing numeric vector with curing
#' @param in_folder path to input folder
#'
#' @returns dataframe with dictionaries for the fuel types and cells' colors
GenerateDat <- function(g_fuel_type, elev, ps, saz, curing, in_folder) {
  n <- length(g_fuel_type)

  # GFL dictionary
  GFLD <- list(
    "C1" = 0.75, "C2" = 0.8, "C3" = 1.15, "C4" = 1.2, "C5" = 1.2, "C6" = 1.2, "C7" = 1.2,
    "D1" = NA, "D2" = NA,
    "S1" = NA, "S2" = NA, "S3" = NA,
    "O1a" = 0.35, "O1b" = 0.35,
    "M1" = NA, "M2" = NA, "M3" = NA, "M4" = NA, "NF" = NA,
    "M1_5" = 0.1, "M1_10" = 0.2, "M1_15" = 0.3, "M1_20" = 0.4, "M1_25" = 0.5, "M1_30" = 0.6,
    "M1_35" = 0.7, "M1_40" = 0.8, "M1_45" = 0.8, "M1_50" = 0.8, "M1_55" = 0.8, "M1_60" = 0.8,
    "M1_65" = 1.0, "M1_70" = 1.0, "M1_75" = 1.0, "M1_80" = 1.0, "M1_85" = 1.0, "M1_90" = 1.0, "M1_95" = 1.0
  )

  # PDF dictionary
  PDFD <- list(
    "M3_5" = 5, "M3_10" = 10, "M3_15" = 15, "M3_20" = 20, "M3_25" = 25, "M3_30" = 30, "M3_35" = 35, "M3_40" = 40, "M3_45" = 45, "M3_50" = 50,
    "M3_55" = 55, "M3_60" = 60, "M3_65" = 65, "M3_70" = 70, "M3_75" = 75, "M3_80" = 80, "M3_85" = 85, "M3_90" = 90, "M3_95" = 95, "M4_5" = 5,
    "M4_10" = 10, "M4_15" = 15, "M4_20" = 20, "M4_25" = 25, "M4_30" = 30, "M4_35" = 35, "M4_40" = 40, "M4_45" = 45, "M4_50" = 50, "M4_55" = 55,
    "M4_60" = 60, "M4_65" = 65, "M4_70" = 70, "M4_75" = 75, "M4_80" = 80, "M4_85" = 85, "M4_90" = 90, "M4_95" = 95, "M3M4_5" = 5, "M3M4_10" = 10,
    "M3M4_15" = 15, "M3M4_20" = 20, "M3M4_25" = 25, "M3M4_30" = 30, "M3M4_35" = 35, "M3M4_40" = 40, "M3M4_45" = 45, "M3M4_50" = 50, "M3M4_55" = 55,
    "M3M4_60" = 60, "M3M4_65" = 65, "M3M4_70" = 70, "M3M4_75" = 75, "M3M4_80" = 80, "M3M4_85" = 85, "M3M4_90" = 90, "M3M4_95" = 95
  )

  # PCD dictionary
  PCD <- list(
    "M1_5" = 5, "M1_10" = 10, "M1_15" = 15, "M1_20" = 20, "M1_25" = 25, "M1_30" = 30, "M1_35" = 35, "M1_40" = 40, "M1_45" = 45,
    "M1_50" = 50, "M1_55" = 55, "M1_60" = 60, "M1_65" = 65, "M1_70" = 70, "M1_75" = 75, "M1_80" = 80, "M1_85" = 85, "M1_90" = 90,
    "M1_95" = 95, "M2_5" = 5, "M2_10" = 10, "M2_15" = 15, "M2_20" = 20, "M2_25" = 25, "M2_30" = 30, "M2_35" = 35, "M2_40" = 40,
    "M2_45" = 45, "M2_50" = 50, "M2_55" = 55, "M2_60" = 60, "M2_65" = 65, "M2_70" = 70, "M2_75" = 75, "M2_80" = 80, "M2_85" = 85,
    "M2_90" = 90, "M2_95" = 95, "M1M2_5" = 5, "M1M2_10" = 10, "M1M2_15" = 15, "M1M2_20" = 20, "M1M2_25" = 25, "M1M2_30" = 30,
    "M1M2_35" = 35, "M1M2_40" = 40, "M1M2_45" = 45, "M1M2_50" = 50, "M1M2_55" = 55, "M1M2_60" = 60, "M1M2_65" = 65, "M1M2_70" = 70,
    "M1M2_75" = 75, "M1M2_80" = 80, "M1M2_85" = 85, "M1M2_90" = 90, "M1M2_95" = 95
  )
  df <- data.frame(
    fueltype = g_fuel_type,
    mon = NA, jd = NA, M = NA, jd_min = NA,
    lat = 51.621244,
    lon = -115.608378,
    elev = elev,
    ffmc = NA, ws = NA, waz = NA, bui = NA,
    ps = ps,
    saz = saz,
    pc = NA, pdf = NA, gfl = NA,
    cur = curing,
    time = 20,
    pattern = NA
  )

  # Handle default curing for grass types
  if (all(is.na(df$cur))) {
    df$cur[df$fueltype %in% c("O1a", "O1b")] <- 60.0
  }

  # Vectorized mapping for GFL (more efficient in R than loops)
  df$gfl <- as.numeric(GFLD[df$fueltype])

  utils::write.csv(df, file.path(in_folder, "Data.csv"), row.names = FALSE, na = "")
  return(df)
}

#' GenDataFile
#' Main wrapper function
#' @param in_folder folder where the Data.csv file should be generated
#' @export
#' @returns data frame with generated data
GenDataFile <- function(in_folder) {
  fbp_lookup <- file.path(in_folder, "fbp_lookup_table.csv")
  dicts <- Dictionary(fbp_lookup)

  forest_path <- file.path(in_folder, "Forest.asc")
  f_grid <- ForestGrid(forest_path, dicts)

  n_cells <- length(f_grid$ftype)
  grids <- DataGrids(in_folder, n_cells)

  GenerateDat(f_grid$ftype, grids$Elevation, grids$PS, grids$SAZ, grids$Curing, in_folder)
}
