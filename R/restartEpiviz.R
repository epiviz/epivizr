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
  saved_app <- .self
  
  saved_app$server$start_server()    
  saved_app$service()
  
  chart_ids <- ls(envir=saved_app$chart_mgr$.chart_list)
  chart_objs <- lapply(chart_ids, function(id){
    chart_obj <- saved_app$chart_mgr$.get_chart_object(id)
    saved_app$chart_mgr$rm_chart(chart_obj)
    chart_obj
  })
  
  datasource_names <- ls(envir=saved_app$data_mgr$.ms_list)
  datasource_origin_names <- NULL
  if (length(datasource_names) > 0) {
    datasource_origin_names <- lapply(datasource_names, function(name) {
      ms_obj <- saved_app$data_mgr$.get_ms_object(name);
      ms_obj$get_source_name()
    })
  }
  
  for (chart_obj in chart_objs) {
    origin_source_name <- chart_obj$get_source_name()

    if (!(origin_source_name %in% datasource_origin_names)) {
      chart_obj_data <- get0(origin_source_name, envir=globalenv(), inherits=TRUE)
      
      if (is.null(chart_obj_data)) {
        stop("Data for ", chart_obj$get_id(), " was not saved in file. Please load ", 
        origin_source_name, " before restarting.")
      }
      
       ms_object <- saved_app$data_mgr$add_measurements(chart_obj_data)
       ms_object$set_source_name(origin_source_name)
       ms_object$set_id(chart_obj$.datasource)
    }

    saved_app$chart_mgr$add_chart(chart_obj)
  }
  
  if (open_browser) {
    sink("/dev/null")  
    saved_app$.open_browser()
    sink()
  }
  
  return(saved_app)
}
