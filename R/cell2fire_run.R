#' Main Entry Point for Cell2FireR
#' @export
#' @param input_args A character vector of arguments.
#' @examples ## get help.
#' #cell2fire_run("-h")
#' ## run demo data
#' #cell2fire_run(c("--input-instance-folder", "../../data/Sub40x40/",
#' #"--output-folder", "output/Sub40x40/",
#' #"--ignitions", "1",
#' #"--sim-years", "1"))
cell2fire_run <- function(input_args = commandArgs(trailingOnly = TRUE)) {
  # Initialize and parse

  if(length(input_args)==0){
    message(ParseInputs("-h"))
    return(NULL)
  }

  tokens <-  trimws(unlist(strsplit(input_args, ' (?=(?:[^"]*"[^"]*")*[^"]*$)',perl = TRUE)))
  args <- gsub('^"|"$', '', tokens)

  args <- ParseInputs(args)
  # Logic matching main.py: Clean output directory
  if (!args$onlyProcessing && !is.null(args$`output-folder`)) {
    if (dir.exists(args$`output-folder`)) {
      unlink(args$`output-folder`, recursive = TRUE)
    }
  }

  # Initialize Environment/Engine (R6 Class)
  # Assuming your R6 class is defined in Cell2FireR.R
  env <- Cell2FireR$new(args)

  # Handle Statistics
  if (args$stats) {
    message("------ Generating Statistics --------")
    env$stats()
  }

  # Handle Heuristics
  if (args$heuristic != -1) {
    message("------ Generating outputs for heuristics --------")
    env$heur()
  }
}
