.wait_until_connected <- function(server, timeout=60L) {
  ptm <- proc.time()
  while (!server$is_socket_connected() && (proc.time() - ptm < timeout)["elapsed"]) {
    Sys.sleep(0.001)
    server$service()
  }
  if (!server$is_socket_connected()) {
    stop("[epivizrStandalone] Error starting app. UI unable to connect to websocket server.")
  }
  invisible()
}

#' Restart epiviz app connection and workspace.
#'
#' @param file (character) The name of the file that holds the EpivizApp object to be restarted, ending in .rda.
#' @param open_browser (logical) browse to the epiviz URL before exiting function.
#' @return An object of class \code{\link{EpivizApp}}
#'
#' @examples
#' # see package vignette for example usage
#' app <- restartEpiviz(file="app.rda")
#'
#' @export
restartEpiviz <- function(file, open_browser=TRUE) {
    if (!file.exists(file)) {
      stop("File does not exist")
    }
    
    load(file=file)
    app <- .self
    app$server$start_server()    
    app$service()
    
    if (open_browser) {
      app$.open_browser()
      # Allows enough time to for browswer to
      # load before reconnecting charts to UI
      # TODO: Find alternative solution
      Sys.sleep(5)
    }
    
    chart_ids <- ls(envir=app$chart_mgr$.chart_list)
    data_not_in_environment <- NULL
    
    # If file is saved without data, this will
    # update data into Epiviz App if it exists
    # in environment
    for (id in chart_ids){
      ms_obj <- app$get_ms_object(id)
      
      if (is.null(ms_obj$.object)) {
        new_obj <- tryCatch({
          eval(parse(text=ms_obj$get_source_name()))
        }, error = function(e) {
          # instead of stopping here, this accumulates 
          # names of all datasources not in environment
          datasource_name <- ms_obj$get_source_name()
          data_not_in_environment <<- c(data_not_in_environment, datasource_name)
          return(NULL)
        })
        
        if (!is.null(new_obj)) {
          ms_obj$update(new_obj)
        }
        
      } 
    }
    
    if (!is.null(data_not_in_environment)) {
      datasource_names <- paste(data_not_in_environment, collapse=", ")
      app$server$stop_server()
      stop("Load data in environment before restarting: ", datasource_names)
    }
    
    app$server$wait_to_clear_requests()
    app$chart_mgr$redraw_charts()
    
    return(app)
  }
