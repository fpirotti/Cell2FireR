# 1. Define the arguments list (mapping the CLI flags)
cmd_args <- list(
  InFolder        = "../../data/Sub40x40/",
  OutFolder       = "results/Sub40x40",
  ignitions       = TRUE,
  sim_years       = 1,
  nsims           = 5,
  finalGrid       = TRUE,
  WeatherOpt      = "rows",
  nweathers       = 1,
  input_PeriodLen = 1.0,
  OutMessages     = TRUE,
  ROS_CV          = 0.0,
  seed            = 123,
  IgRadius        = 5,
  grids           = TRUE,

  # Flags for post-processing/stats
  stats           = TRUE,
  allPlots        = TRUE,
  combine         = TRUE,
  # Logic control
  onlyProcessing  = FALSE
)

# 2. Instantiate and run
# This triggers the initialize() -> run() sequence inside the R6 class
 # sim <- Cell2FireR::Cell2FireC$new(cmd_args)
