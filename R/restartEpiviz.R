#' Restart epiviz app connection and workspace.
#'
#' @param file (character) The name of the file that holds the EpivizApp object to be restarted, ending in .rda.
#' @param open_browser (logical) browse to the epiviz URL before exiting function.
#' @param host (character) name of epiviz app host to open at restart
#' @param envir (environment) environment in which to evaluate expressions needed to reconstruct data sources
#' 
#' @return An object of class \code{\link{EpivizApp}}
#'
#' @examples
#' # see package vignette for example usage
#' app <- startEpiviz(non_interactive=TRUE, open_browser=TRUE)
#' file_name <- tempfile(fileext=".rda")
#' app$save(file=file_name)
#' app$stop_app()
#' 
#' app <- restartEpiviz(file=file_name, open_browser=FALSE)
#'
#' @export
restartEpiviz <- function(file, open_browser=TRUE, host=NULL, envir=parent.frame()) {
  
  if (!file.exists(file)) {
    stop("File does not exist")
  }

  load_env <- new.env()  
  load(file=file, envir=load_env)
  app <- get("app", load_env)
  
  if (open_browser) {
    app$server$start_server()    
    app$service()
  }
  
  if(!is.null(host)) {
    app$.url_parms$host <- host
  }
  
  ms_ids <- ls(envir=app$data_mgr$.ms_list)
  data_not_in_envir <- FALSE
  
  # If file is saved without data, this will
  # update data into Epiviz App if it exists
  # in environment
  for (id in ms_ids){
    ms_obj <- app$data_mgr$.get_ms_object(id)
    
    if (is.null(ms_obj$.object)) {
      new_obj <- tryCatch({
        eval(parse(text=ms_obj$get_source_name()), envir=envir)
      }, error = function(e) {
        # instead of stopping here, this warns 
        # of all datasources not in environment
        datasource_name <- ms_obj$get_source_name()
        warning("Couldn't evaluate the expression: ", datasource_name)     
        return(NULL)
      })
      
      if (is.null(new_obj)) {
        data_not_in_envir <- TRUE
      } else {
        ms_obj$update(new_obj)
      }
    } 
  }
  
  if (data_not_in_envir) {
    app$server$stop_server()
    stop("Load data in environment before restarting.")
  }
  
  if (open_browser) {
    app$.open_browser()
    
    callback <- function(response_data) {
      if (app$server$.verbose) {
        if(response_data$success){
          cat("UI is READY \n")       
        } else {
          cat("UI is NOT READY \n")
        }
      }
    }
    
    request_data <- list(action="uiStatus")
    app$server$send_request(request_data, callback)
  
    tryCatch({
      app$server$wait_to_clear_requests()
    }, error = function(e) {
      app$server$stop_server()
      stop(e)
    })
    
  }
  
  app$chart_mgr$redraw_charts()
  return(app)
}