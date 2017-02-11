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
  if (open_browser) {
    saved_app$.open_browser()
  }
  saved_app$service()
  
  chart_ids <- ls(envir=saved_app$chart_mgr$.chart_list)
  
  for (id in chart_ids) {
    chart_obj <- saved_app$chart_mgr$.get_chart_object(id)
    saved_app$chart_mgr$rm_chart(chart_obj)
    saved_app$chart_mgr$add_chart(chart_obj)
  }
  
  return(saved_app)
}