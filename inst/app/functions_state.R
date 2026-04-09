
restore_inputs <- function(session, state) {
  
  for (id in names(state)) {
    value <- state[[id]]
    
    # Skip NULLs
    if (is.null(value)) next
    
    # Handle by type (extend as needed)
    try({
      if (is.character(value) && length(value) == 1) {
        updateTextInput(session, id, value = value)
        
      } else if (is.numeric(value) && length(value) == 1) {
        updateNumericInput(session, id, value = value)
        
      } else if (is.logical(value) && length(value) == 1) {
        updateCheckboxInput(session, id, value = value)
        
      } else if (is.numeric(value) && length(value) == 2) {
        updateSliderInput(session, id, value = value)
        
      } else if (is.character(value)) {
        updateSelectInput(session, id, selected = value)
        
      }
    }, silent = TRUE)
  }
}

loadState <- function(){
  if(!isTruthy(isolate(input$inputfolder)) || 
     !file.exists(file.path(isolate(input$inputfolder), "state.rda") )){ 
    return(invisible())
  }
  
  load(file.path(isolate(input$inputfolder), "state.rda") )
  
}

saveState <- function(){
  if(isTruthy(isolate(input$inputfolder))){ 
    state <- isolate(reactiveValuesToList(input))
    cat("Session ending ", isolate(input$inputfolder) , "\n")
    cat("Session ending ", getwd() , "\n")
    save(state, file = file.path(isolate(input$inputfolder), "state.rda") )
    cat("Session ended\n")
  }
}