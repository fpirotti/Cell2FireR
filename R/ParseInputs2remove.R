#' make_parser
#' @import optparse
#' @returns list of options
#'
make_parser <- function() {
  option_list <- list(
    # Folders
    make_option("--input-instance-folder",
                help="The path to the folder contains all the files for the simulation",
                type="character", default=NULL),
    make_option("--output-folder",
                help="The path to the folder for simulation output files",
                type="character", default=NULL),

    # Integers
    make_option("--sim-years",
                help="Number of years per simulation (default 1)",
                type="integer", default=1),
    make_option("--nsims",
                help="Total number of simulations (replications)",
                type="integer", default=1),
    make_option("--seed",
                help="Seed for random numbers (default is 123)",
                type="integer", default=123),
    make_option("--nweathers",
                help="Max index of weather files to sample for the random version",
                type="integer", default=1),
    make_option("--nthreads",
                help="Number of threads to run the simulation",
                type="integer", default=1),
    make_option("--max-fire-periods",
                help="Maximum fire periods per year (default 1000)",
                type="integer", default=1000),
    make_option("--IgnitionRad",
                help="Adjacents degree for defining an ignition area",
                type="integer", default=0),
    make_option("--gridsStep",
                help="Grids are generated every n time steps",
                type="integer", default=60),
    make_option("--gridsFreq",
                help="Grids are generated every n episodes/sims",
                type="integer", default=-1),

    # Heuristic
    make_option("--heuristic",
                help="Heuristic version to run (-1 default no heuristic, 0 all)",
                type="integer", default=-1),
    make_option("--MessagesPath",
                help="Path with the .txt messages generated for simulators",
                type="character", default=NULL),
    make_option("--GASelection",
                help="Use the genetic algorithm instead of greedy selection",
                action="store_true", default=FALSE),
    make_option("--HarvestedCells",
                help="File with initial harvested cells (csv)",
                type="character", default=NULL),
    make_option("--msgheur",
                help="Path to messages needed for Heuristics",
                type="character", default=""),
    make_option("--applyPlan",
                help="Path to Heuristic/Harvesting plan",
                type="character", default=""),
    make_option("--DFraction",
                help="Demand fraction w.r.t. total forest available",
                type="double", default=1.0),
    make_option("--GPTree",
                help="Use the Global Propagation tree for calculating the VaR",
                action="store_true", default=FALSE),
    make_option("--customValue",
                help="Path to Heuristic/Harvesting custom value file",
                type="character", default=NULL),
    make_option("--noEvaluation",
                help="Generate the treatment plans without evaluating them",
                action="store_true", default=FALSE),
    
    # Genetic params
    make_option("--ngen",
                help="Number of generations for genetic algorithm",
                type="integer", default=500),
    make_option("--npop",
                help="Population for genetic algorithm",
                type="integer", default=100),
    make_option("--tsize",
                help="Tournament size",
                type="integer", default=3),
    make_option("--cxpb",
                help="Crossover prob.",
                type="double", default=0.8),
    make_option("--mutpb",
                help="Mutation prob.",
                type="double", default=0.2),
    make_option("--indpb",
                help="Individual prob.",
                type="double", default=0.5),
    
    # Booleans / Flags
    make_option("--weather",
                help="The 'type' of weather: constant, random, rows",
                type="character", default="rows"),
    make_option("--spreadPlots",
                help="Generate spread plots",
                action="store_true", default=FALSE),
    make_option("--finalGrid",
                help="Generate final grid",
                action="store_true", default=FALSE),
    make_option("--verbose",
                help="Output all the simulation log",
                action="store_true", default=TRUE),
    make_option("--ignitions",
                help="Activates predefined ignition points inside the Ignitions.csv file",
                action="store_true", default=FALSE),
    make_option("--grids",
                help="Generate grids",
                action="store_true", default=FALSE),
    make_option("--simPlots",
                help="generate simulation/replication plots",
                action="store_true", default=FALSE),
    make_option("--allPlots",
                help="generate spread and simulation plots",
                action="store_true", default=FALSE),
    make_option("--combine",
                help="Combine fire evolution diagrams with forest background",
                action="store_true", default=FALSE),
    make_option("--no-output",
                help="Activates no-output mode",
                action="store_true", default=FALSE),
    make_option("--gen-data",
                help="Generates the Data.csv file before simulation",
                action="store_true", default=FALSE),
    make_option("--output-messages",
                help="Generates a file with messages per cell",
                action="store_true", default=FALSE),
    make_option("--Prometheus-tuned",
                help="Activates predefined tuning parameters",
                action="store_true", default=FALSE),
    make_option("--trajectories",
                help="Save fire trajectories FI and FS",
                action="store_true", default=FALSE),
    make_option("--stats",
                help="Output statistics from the simulations",
                action="store_true", default=FALSE),
    make_option("--correctedStats",
                help="Normalize the number of grids outputs for hourly stats",
                action="store_true", default=FALSE),
    make_option("--onlyProcessing",
                help="Post-processing tool mode",
                action="store_true", default=FALSE),
    make_option("--bbo",
                help="Use factors in BBOFuels.csv file",
                action="store_true", default=FALSE),
    make_option("--fdemand",
                help="Finer demand/treatment fraction",
                action="store_true", default=FALSE),
    make_option("--pdfOutputs",
                help="Generate pdf versions of all plots",
                action="store_true", default=FALSE),

    # Floats / Doubles
    make_option("--Fire-Period-Length",
                help="Fire Period length in minutes. Default 60",
                type="double", default=60.0),
    make_option("--Weather-Period-Length",
                help="Weather Period length in minutes. Default 60",
                type="double", default=60.0),
    make_option("--ROS-Threshold",
                help="Minimum head ros (m/min) default 0.1.",
                type="double", default=0.1),
    make_option("--HFI-Threshold",
                help="Minimum HFI (Kw/m) default 0.1.",
                type="double", default=0.1),
    make_option("--ROS-CV",
                help="Coefficient of Variation for ROS, default is 0",
                type="double", default=0.0),
    make_option("--HFactor",
                help="Adjustment factor: HROS",
                type="double", default=1.0),
    make_option("--FFactor",
                help="Adjustment factor: FROS",
                type="double", default=1.0),
    make_option("--BFactor",
                help="Adjustment factor: BROS",
                type="double", default=1.0),
    make_option("--EFactor",
                help="Adjustment ellipse factor",
                type="double", default=1.0),
    make_option("--BurningLen",
                help="Burning length period",
                type="double", default=-1.0)
  )

  return(optparse::OptionParser(option_list = option_list,prog = "Cell2Fire"))
}

#' ParseInputs
#'
#' @param args character string of arguments to parse.
#' @param parser can pass a parser if set.
#' @returns vector of arguments
#'
ParseInputs <- function(args, parser=NULL) {

  if(is.null(parser)) parser <- make_parser()
  args <- tryCatch({
    optparse::parse_args(parser, args=args)
  }, error = function(e) {
    # If help was requested, optparse prints the help and then
    # throws an error to stop execution.
    if (grepl("help requested", e$message)) {
      # Exit gracefully without showing a "scary" error message
      return(NULL)
    } else {
      # If it's a real error (missing required arg, etc.), re-throw it
      stop(e)
    }
  })

  # If args is NULL, it means help was printed; we just stop here.
  if (is.null(args)) return(invisible(NULL))

  # ... rest of your simulation logic ...
  message("Starting simulation...")
  # args <- optparse::parse_args(parser, args=args)
  return(args)
}


#' InitCells
#'
#' @param NCells total number of cells to process
#' @param FTypes2 List. Mapping of fuel type strings (e.g., "m1") to integer codes.
#' @param ColorsDict  List. colors of .....
#' @param CellsGrid4  ...
#' @param CellsGrid3 ....
#'
#' @returns a list with cell values
#'
InitCells <- function(NCells, FTypes2, ColorsDict, CellsGrid4, CellsGrid3) {
  # 1. Initialize vectors
  FTypeCells  <- integer(NCells)
  StatusCells <- integer(NCells)
  RealCells   <- integer(NCells)

  # 2. Logic for FType and Status (Vectorized)
  # Check if lower-case CellsGrid4 is in the keys of FTypes2
  isValidType <- tolower(CellsGrid4) %in% names(FTypes2)

  # If not valid: Status = 4, Grid4 = "s1"
  StatusCells[!isValidType] <- 4
  CellsGrid4[!isValidType]  <- "s1"

  # If valid: FType = 2
  FTypeCells[isValidType] <- 2

  # 3. Handle RealCells (Incremental counter for valid cells only)
  # This mimics the Python 'cellcounter' logic
  valid_indices <- which(isValidType)
  RealCells[valid_indices] <- seq_along(valid_indices)

  # 4. Handle Colors (Mapping via Dictionary/Named List)
  # Default color is white (1,1,1,1)
  Colors <- rep(list(c(1.0, 1.0, 1.0, 1.0)), NCells)

  # Find which items in CellsGrid3 exist in our ColorsDict
  grid3_str <- as.character(CellsGrid3)
  color_exists <- grid3_str %in% names(ColorsDict)

  # Map the colors
  Colors[color_exists] <- ColorsDict[grid3_str[color_exists]]

  return(list(
    FTypeCells = FTypeCells,
    StatusCells = StatusCells,
    RealCells = RealCells,
    Colors = Colors
  ))
}
