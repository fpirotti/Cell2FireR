#' @title Cell2FireR R6 Class
#'
#' @description
#' A R6 class to orchestrate the Cell2Fire C++ wildfire simulation.
#' This class replicates the Python `Cell2FireC` orchestration logic,
#' managing pre-processing, system calls to the C++ core, and data loading.
#'
#' @field args A list containing simulation parameters (e.g., paths, thresholds, seeds).
#' @field n_cells Integer. Total number of cells in the forest grid.
#' @field rows Integer. Number of rows in the forest grid.
#' @field cols Integer. Number of columns in the forest grid.
#' @field f_type_cells Numeric vector. Stores the fuel types for each cell.
#' @field ftypes2 List. Mapping of fuel type strings (e.g., "m1") to integer codes.
#' @field log_file character logfile path
#'
#' @importFrom R6 R6Class
#' @importFrom terra rast ncell nrow ncol
#' @examples  my_args <- list(
#' `input-instance-folder` = "data/forest_instance",
#' `output-folder` = "output/test_1",
#' `sim-years` = 1,
#' nsims = 50,
#' `Fire-Period-Length` = 60,
#' WeatherOpt = "random",
#' nweathers = 1,
#' `ROS-CV` = 0.1,
#' IgRadius = 1,
#' seed = 42,
#' `ROS-Threshold` = 0.1,
#' `HFI-Threshold` = 0.1,
#' onlyProcessing = FALSE,
#' ignitions = TRUE,
#' verbose = TRUE,
#' grids = TRUE,
#' finalGrid = TRUE
#' )
#'
#'
#' # sim <- Cell2FireC$new(my_args)
#'
#'
#' # sim$get_data()
#' @export
Cell2FireR <- R6Class("Cell2FireC",
                      public = list(
                        args = NULL,
                        n_cells = 0,
                        rows = 0,
                        cols = 0,
                        f_type_cells = NULL,
                        ftypes2 = list(
                          "m1" = 0, "m2" = 1, "m3" = 2, "m4" = 3,
                          "c1" = 4, "c2" = 5, "c3" = 6, "c4" = 7, "c5" = 8, "c6" = 9, "c7" = 10,
                          "d1" = 11, "s1" = 12, "s2" = 13, "s3" = 14, "o1a" = 15, "o1b" = 16, "d2" = 17
                        ),
                        log_file = NULL,

                        #' @description
                        #' Redefines message call to write to log file as well
                        #' @param ... message text
                        #'
                        #' @returns nothing
                        msg = function(...) {
                          if(!is.null(self$log_file) && self$log_file!="") cat(
                            format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                            " | ",
                            paste(..., collapse = " "),
                            "\n",
                            file = self$log_file,
                            append = TRUE
                          )
                          base::message(...)
                        } ,
                        #' @description
                        #' Initialize the Cell2FireC object and optionally run the simulation.
                        #' @param args A named list of arguments.
                        initialize = function(args) {
                          self$args <- args
                          # Setup logging
                          log_dir <- if (!is.null(self$args$`output-folder`)) self$args$`output-folder` else self$args$`input-instance-folder`
                          if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
                          log_file <- file.path(log_dir, "LogFile.txt")
                          self$log_file <- log_file
                          # Pre-processing: generate Data.csv
                          self$generate_data()

                          # Main execution
                          if (isFALSE(self$args$onlyProcessing)) {
                            self$run()
                          } else {
                            self$msg("Running as a post-processing tool.")
                          }
                        },

                        #' @description
                        #' Execute the C++ Cell2Fire binary using a system call.
                        #' This method builds the command-line arguments and pipes output to a log file.
                        #' @return None.
                        run = function() {
                          # Path to the compiled C++ binary
                          exe_path <- normalizePath(file.path("..", "Cell2FireC", "Cell2Fire"), mustWork = FALSE)

                          if (.Platform$OS.type == "windows") exe_path <- paste0(exe_path, ".exe")

                          # Option 2: Check if it exists before running
                          if (!file.exists(exe_path)) {
                            stop("C++ Executable NOT found at: ", exe_path,
                                 "\nDid you compile it? Run 'make' in the Cell2FireC folder.")
                          }

                          browser()
                          # Build the arguments vector
                          f <- function(n){
                            arg <- self$args[[n]]
                            if(isFALSE(arg)){
                              return(NULL)
                            }
                            arg
                          }

                          cmd_args_vals <- Filter(Negate(is.null), sapply(names(self$args), f))
                          cmd_args <- as.character(rbind(sprintf("--%s", names(cmd_args_vals)), cmd_args_vals))

                          # cmd_args <- c(
                          #   "--input-instance-folder", self$args$`input-instance-folder`,
                          #   "--output-folder", ifelse(!is.null(self$args$`output-folder`), self$args$`output-folder`, ""),
                          #   if (isTRUE(self$args$ignitions)) "--ignitions" else NULL,
                          #   "--sim-years", as.character(self$args$`sim-years`),
                          #   "--nsims", as.character(self$args$nsims),
                          #   if (isTRUE(self$args$grids)) "--grids" else NULL,
                          #   if (isTRUE(self$args$finalGrid)) "--final-grid" else NULL,
                          #   "--Fire-Period-Length", as.character(self$args$input_PeriodLen),
                          #   if (isTRUE(self$args$OutMessages)) "--output-messages" else NULL,
                          #   "--weather", self$args$WeatherOpt,
                          #   "--nweathers", as.character(self$args$nweathers),
                          #   "--ROS-CV", as.character(self$args$ROS_CV),
                          #   "--IgnitionRad", as.character(self$args$IgRadius),
                          #   "--seed", as.character(as.integer(self$args$seed)),
                          #   "--ROS-Threshold", as.character(self$args$ROS_Threshold),
                          #   "--HFI-Threshold", as.character(self$args$HFI_Threshold),
                          #   if (isTRUE(self$args$BBO)) "--bbo" else NULL,
                          #   "--HarvestPlan", ifelse(!is.null(self$args$HCells), self$args$HCells, ""),
                          #   if (isTRUE(self$args$verbose)) "--verbose" else NULL
                          # )

                          self$msg("Starting C++ Core execution...")
                          if (isTRUE(self$args$verbose)){
                            self$msg("Arguments: ", paste(cmd_args, collapse = "\n"))
                          }

                          status <- system2(exe_path, args = cmd_args[1:2], stdout = self$log_file, stderr = self$log_file)

                          browser()
                          if (status != 0) {
                            self$msg(paste("C++ returned error code:", status, ". See log:", self$log_file))
                            self$msg("make sure your input arguments are correct!")
                            stop(paste("C++ returned error code:", status, ". See log:", self$log_file))
                          }
                          self$msg("End of Cell2Fire execution.")
                        },

                        #' @description
                        #' Generate the required `Data.csv` file if it does not already exist
                        #' in the input folder.
                        generate_data = function() {
                          data_name <- file.path(self$args$`input-instance-folder`, "Data.csv")
                          if (!file.exists(data_name)) {
                            self$msg("Generating Data.csv File...")
                            GenDataFile(self$args$`input-instance-folder`)
                          }
                        },

                        #' @description
                        #' Load forest grid metadata (rows, columns, total cells) from the
                        #' `Forest.asc` file using the `terra` package.
                        get_data = function() {
                          forest_file <- file.path(self$args$`input-instance-folder`, "Forest.asc")
                          if (file.exists(forest_file)) {
                            r <- terra::rast(forest_file)
                            self$rows <- terra::nrow(r)
                            self$cols <- terra::ncol(r)
                            self$n_cells <- terra::ncell(r)
                            self$msg("Metadata loaded: ", self$rows, "x", self$cols, " grid.")
                          } else {
                            warning("Forest.asc not found.")
                          }
                        }
                      )
)

