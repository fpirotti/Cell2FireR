#' Run Cell2Fire Simulation
#'
#' @param fuel Path to fuel raster.
#' @param fuel_model Character string specifying the fuel model (e.g., "0. Scott & Burgan").
#' @param input_folder Directory containing input files (like weathers).
#' @param out_folder Base directory for outputs.
#' @param elevation Path to elevation raster.
#' @param slope Path to slope raster in degreees.
#' @param saz Path to azimuth raster in degrees.
#' @param cur Path to curvature raster.
#' @param crown Logical; whether to use crown data.
#' @param cbh Path to Crown Base Height raster.
#' @param cbd Path to Crown Bulk Density raster.
#' @param ccf Path to Crown Canopy Fraction raster.
#' @param hm Path to Canopy Height Model raster.
#' @param ignition_mode Character string specifying ignition mode.
#' @param ignition_file Path to ignition probability map.
#' @param ignition_point Path to ignition points layer.
#' @param ignition_radius Numeric radius for ignition.
#' @param firebreaks Path to firebreaks raster.
#' @param weather_mode Character string specifying weather mode.
#' @param wea_file Path to single weather CSV.
#' @param weather_weights Path to weather weights file.
#' @param nsims Integer number of simulations.
#' @param rng_seed Integer seed for RNG.
#' @param sim_threads Integer number of threads.
#' @param fmc Numeric Fuel Moisture Content.
#' @param ldfmcs Character string for scenario.
#' @param outputs Character vector of output flags (e.g., c("grids", "stats")).
#' @param c2f_bin_path Path to the Cell2Fire executable. 
#' Defaults to `Cell2FireR::c2f_bin_pathEnv()` but user can specify a different path.
#' @param template_dir Path to the directory containing lookup tables and default weather.
#' @param dry Logical; if TRUE, only prepares files and returns the command arguments.
#' @param verbose Logical; if TRUE, provides many messages.
#' 
#' @returns A list object containing the process handle and metadata 
#' for a fire spread simulation from cell2fire.
#' @field process A \code{processx::process} object.
#' @field command Character string. Path to the binary.
#' @field args Character vector. The CLI arguments used.
#' @export
run_cell2fire <- function(
    fuel, fuel_model, input_folder, out_folder, 
    elevation = NULL, slope = NULL, saz=NULL, cur=NULL,
    crown = FALSE, cbh = NULL, cbd = NULL, ccf = NULL, hm = NULL,
    ignition_mode = NULL,
    ignition_file = NULL, ignition_point = NULL, ignition_radius = NULL,
    firebreaks = NULL,
    weather_mode = "0. Single weather file", wea_file = NULL, weather_weights = NULL,
    nsims = 1, rng_seed = 123, sim_threads = 1, fmc = 85, ldfmcs = "constant",
    outputs = NULL,
    c2f_bin_path  = c2f_bin_pathEnv(),
    template_dir,
    dry = FALSE,
    verbose = TRUE
) {
  

  
 
  tryCatch({
    
 
    # Basic validation
    if (is.missing(fuel) || is.null(fuel) || fuel == "") stop("Fuel raster is required.")
    if (is.missing(fuel_model, F) || is.null(fuel_model) || fuel_model == "") stop("Fuel model is required.")
    if (is.missing(input_folder) || is.null(input_folder) || input_folder == "") stop("Input folder is required.")
    if (is.missing(out_folder) || is.null(out_folder)) stop("Output folder is required.")
    
 
    input_folder <- normalizePath(input_folder)
    out_folder <- normalizePath(out_folder)
    # -------------------------------------------------------------
    # 1. SETUP DIRECTORIES
    # -------------------------------------------------------------
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    instance_dir <- file.path(out_folder, paste0("firesim_", timestamp))
    results_dir <- file.path(instance_dir, "results")
 
    if (!dry) drcres1 <- dir.create(instance_dir, recursive = TRUE, showWarnings = TRUE)
    if (!dry) drcres2 <- dir.create(results_dir, recursive = TRUE, showWarnings = TRUE)
    
    # actual_wd <- getwd()
    # full_path <- normalizePath(instance_dir, mustWork = FALSE)
    # exists_now <- dir.exists(instance_dir)
    # files_in_parent <- list.files(dirname(instance_dir))
    
    # Helper to copy and rename files
    copy_to_instance <- function(filepath, standard_name) { 
      FP <- NULL
      if (!is.null(filepath) && nzchar(filepath)) { 
        ext <- tools::file_ext(filepath)
        FP <- file.path(instance_dir, paste0(standard_name, ".", ext)) 
        
        if (standard_name == "fuels") {
          tf <- terra::rast(filepath)
          filepath.old <- filepath
          if (as.integer(substr(terra::datatype(tf), 4, 4)) < 4) { 
            # Note: add_suffix must be defined elsewhere in your package
            filepath <- paste0(tools::file_path_sans_ext(filepath), "_INT4U.", ext)
            if (!dry) {
              message("Fuel is not in 32 or 64 bits. Converting and copying. Please upload a dataset with fuel as 32 and 64 bits to avoid this warning.")
              if (!file.exists(filepath)) { 
                terra::writeRaster(tf, filename = filepath, datatype = "INT4U", overwrite=T)
                file.remove(filepath.old)                
              }
            } 
          } 
        }
        
        if (!dry) file.link(filepath, FP)  
      }
      return(normalizePath(FP, mustWork = FALSE))
    } 
    # -------------------------------------------------------------
    # 2. COPY LANDSCAPE INPUTS
    # -------------------------------------------------------------
    copy_to_instance(fuel, "fuels")
    copy_to_instance(elevation, "elevation") 
    copy_to_instance(slope, "slope") 
    copy_to_instance(saz, "saz") 
    copy_to_instance(cur, "cur") 
    
    if (crown) { 
      message("CROWN info available and used") 
      copy_to_instance(cbh, "cbh")
      copy_to_instance(cbd, "cbd")
      copy_to_instance(ccf, "ccf")
      copy_to_instance(hm, "hm")
    } else {
      message("CROWN info not used")
    }
    
    # SIMULATOR LUT
    # Note: get_fuel_key must be defined in your package
    # source("../../R/get_fuel_key.R")
    simulator <- get_fuel_key(fuel_model) 
    lookup_table <- switch(simulator,
                           "K" = "kitral_lookup_table.csv",
                           "S" = "spain_lookup_table.csv",
                           "C" = "fbp_lookup_table.csv",
                           "P" = "portugal_lookup_table.csv",
                           {
                             message(paste0("Simulation type not found for fuel model: ", fuel_model, ". Defaulting to Spain."))
                             "spain_lookup_table.csv"
                           })
    
    
    if (!dry) {
      copy.success <- file.copy(file.path(template_dir, lookup_table), 
                        file.path(instance_dir, lookup_table))
      if(!copy.success){
        stop(errorCondition(paste0("Could not copy template of ",lookup_table,"! ") ))
      }
    }
    if (ignition_mode == "1. Probability map distributed random ignition") {
      copy_to_instance(ignition_file, "probabilityMap")
    }
    
    # Handling Firebreaks
    if (!is.null(firebreaks) && nzchar(firebreaks)) {
      fb_rast <- terra::rast(firebreaks)
      fb_cells <- terra::cells(fb_rast, 1)
      if (!dry) utils::write.csv(data.frame(Cell = fb_cells), file.path(instance_dir, "firebreaks.csv"), quote=FALSE, row.names = FALSE)
    }
    
    # -------------------------------------------------------------
    # 3.1 WEATHER 
    # -------------------------------------------------------------
    message(paste0("WEATHER_MODE: ", weather_mode))
    
    if (weather_mode == "0. Single weather file") { 
      if (is.null(wea_file) || !nzchar(wea_file) || !file.exists(wea_file)) {
        message("No Weather file found or selected. Using a generic weather file (20 degrees Celsius, 20 km/h wind, south -> north).")
        if (!dry) file.copy(file.path(template_dir, "Weather.csv"), file.path(instance_dir, "Weather.csv"))
      } else { 
        if (!dry) file.copy(wea_file, file.path(instance_dir, "Weather.csv"))
      }
    } else { 
      wea_dir <- input_folder
      wea_out <- file.path(instance_dir, "Weathers")
      if (!dry && !dir.exists(wea_out)) dir.create(wea_out, showWarnings = FALSE)
      
      wea_files <- list.files(wea_dir, pattern = "^Weather[0-9]*\\.csv$", full.names = TRUE)
      if (length(wea_files) > 0) {
        if (!dry) file.copy(wea_files, wea_out)
      } else { 
        stop( errorCondition("Multiple weathers requires a directory with Weather[0-9]*.csv files!") )
      }
    }
    
    # -------------------------------------------------------------
    # 3.2 IGNITION 
    # -------------------------------------------------------------
    message(paste0("IGNITION_MODE: ", ignition_mode))
    
    if (ignition_mode == "2. Single points on a Layer") {
    
      if (is.null(ignition_point) || !nzchar(ignition_point) || !file.exists(ignition_point)) { 
        stop(errorCondition("No FILE with Ignitions.csv found! Did you select a CSV uploaded with the instance or created from the interactive map  with ignition points?") )
      } else {
        fuel_rast <- terra::rast(fuel)
        ign_pt <- sf::st_read(ignition_point, quiet = TRUE)
        if(nrow(ign_pt)==0) stop(errorCondition("No ignition points found found in file Ignitions.csv! Did you select a CSV uploaded with the instance or created from the interactive map  with ignition points?"))
        if (is.na(sf::st_is_longlat(ign_pt))) { 
          if (is.data.frame(ign_pt)) { 
            if (!dry) {
              message("Writing Ignition points!") 
              utils::write.csv(ign_pt, file.path(instance_dir, "Ignitions.csv"), quote=FALSE, row.names = FALSE)
            }
          } else { 
            stop("Cannot read Ignition points. Ensure a valid file/dataframe was provided.") 
          }
        } else {
          if (!dry) {
            ign_pt <- sf::st_transform(ign_pt, terra::crs(fuel_rast)) 
            cell_id <- terra::cellFromXY(fuel_rast, sf::st_coordinates(ign_pt)[, 1:2]) 
            writeLines(c("Year,Ncell", sprintf("%d,%d", 1:length(cell_id), cell_id)), file.path(instance_dir, "Ignitions.csv")) 
          }
        }
      }
    }
    
    # -------------------------------------------------------------
    # 4. BUILD COMMAND LINE ARGUMENTS
    # -------------------------------------------------------------
    args <- list(
      "--sim" = simulator,
      "--nsims" = nsims,
      "--seed" = rng_seed,
      "--nthreads" = sim_threads,
      "--fmc" = fmc,
      "--scenario" = ldfmcs
    )
    
    if (!is.null(weather_weights) && nzchar(weather_weights)) {
      args[["--weather-weights"]] <- copy_to_instance(weather_weights, "weatherweights") 
    }
    
    if (weather_mode == "0. Single weather file") {
      args[["--weather"]] <- "rows" 
    } else {
      args[["--weather"]] <- "random"
    }
    
    if (crown) args[["--cros"]] <- ""
    if (verbose) args[["--verbose"]] <- ""
    
    if (ignition_mode == "2. Single points on a Layer") {
      args[["--ignitions"]] <- ""
      args[["--IgnitionRad"]] <- ignition_radius
    }
    
    if (!is.null(firebreaks) && nzchar(firebreaks)) {
      args[["--FirebreakCells"]] <- normalizePath(
        file.path(instance_dir, "firebreaks.csv"), mustWork = F)
    }
    
    if (!is.null(outputs)) {
      for (opt in outputs) {
        args[[paste0("--", opt)]] <- ""
      }
    }
    
    # Always grids!
    args[["--grids"]] <- ""
    # if not set it will process only first line of ignitions in ignitions.csv!
    args[["--sim-years"]] <- "999"
    
    # Directories

    args[["--input-instance-folder"]] <-  instance_dir 
    args[["--output-folder"]] <-  results_dir 
    
    # Flatten list to a character vector for processx
    cli_args <- character()
    for (name in names(args)) {
      if (args[[name]] == "") {
        cli_args <- c(cli_args, name) 
      } else {
        cli_args <- c(cli_args, name, as.character(args[[name]]))
      }
    }
    
    # -------------------------------------------------------------
    # 5. EXECUTE CELL2FIRE
    # -------------------------------------------------------------

 
    if (!file.exists(c2f_bin_path)) {
      stop(paste("Cell2Fire executable not found at:", c2f_bin_path))
    }
    
    if (dry) {
      message("Dry run complete. Returning arguments..... " )
       
      message(paste(c(basename(c2f_bin_path), cli_args), collapse = " ") )
      return(paste(c(getwd(), " -- ", basename(c2f_bin_path), cli_args), collapse = " "))
    }
    
    cat(paste("Executing:", c2f_bin_path, paste(cli_args, collapse = "\n")), file="mylog4.log")
 
    # Start the process and return the object
    sim_process <- processx::process$new(
      c2f_bin_path,
      args = cli_args,
      stdout = "|",
      stderr = "|"
    ) 
     
    return( list(
      process = sim_process,
      command = c2f_bin_path,
      args = cli_args,
      outputFolder = results_dir,
      instanceFolder = instance_dir
    )
    )
    
  }, error = function(e) { 
    
    if (dry) {
      message("ERR Dry run complete. Returning arguments.")
      return( paste("Error preparing simulation:", e$message))
    } else { 
      stop(errorCondition(paste("Error preparing simulation:", e$message)))
    }
  }, warning = function(e) {  
    if (dry) {
      message("WW Dry run complete. Returning arguments.")
      return( paste("Warning preparing simulation:", e$message))
    } else { 
      warning(paste("Warning preparing simulation:", e$message))
    }
  })
}