
restore_inputs <- function() { 
  for (id in names(state)) {
    value <- state[[id]]
    
    # Skip NULLs
    if (is.null(value) || !is_all_caps(id)) next
    
    # Handle by type (extend as needed)
    try({ 
       if (is.character(value) && length(value) == 1) {
        updateTextInput(session, id, value = value)
        message("Update updateTextInput ", id) 
      } else if (is.numeric(value) && length(value) == 1) {
        updateNumericInput(session, id, value = value)
        message("Update updateNumericInput ", id)  
      } else if (is.logical(value) && length(value) == 1) {
        updateCheckboxInput(session, id, value = value)
        message("Update updateCheckboxInput ", id)   
      } else if (is.numeric(value) && length(value) == 2) {
        updateSliderInput(session, id, value = value)
        message("Update updateSliderInput ", id)   
        
      } else if (is.character(value)) {
        updateSelectInput(session, id, selected = value) 
        message("Update char updateSelectInput ", id)   
      }
    }, silent = TRUE)
  }
}

loadState <- function(){
  if(!isTruthy(isolate(input$inputfolder)) || 
     !file.exists(file.path(isolate(input$inputfolder), "state.rda") )){ 
    return(invisible())
  } 
  load(file.path(isolate(input$inputfolder), "state.rda"), envir = .GlobalEnv ) 
  restore_inputs()
}

saveState <- function(){
  state <- isolate(reactiveValuesToList(input))
  if(isTruthy(isolate(input$inputfolder))){  
    tryCatch({
      save(state, file = file.path(this.path::this.dir(), isolate(input$inputfolder), "state.rda") )
    }, warning=function(e){
      browser() 
    }, error=function(e){
      browser()
      
    }) 
    cat("Session ended\n")
  }
}