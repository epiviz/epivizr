.colorMapListToVector(colorList) {
    colorMap <- sapply(colorList, "[[", "color")
    names(colorMap) <- sapply(colorList,"[[","name")
    return(colorMap)
}

.colorMapVectorToList(colorVector) {
    lapply(seq(along=colorVector), list(name=names(colorVector)[i], color=colorVector[i]))
}

EpivizChartSettings <- setRefClass("EpivizChartSettings",
                              fields=list(
                                  defs="data.frame",
                                  values="list",
                                  initialized="logical"
                              ),
                                   methods=list(
                                       initialize=function(...) {
                                           initialized <<- FALSE
                                           callSuper(...)
                                       },
                                       updateValues=function(newValues, newColorMap) {
                                           newValues$colorMap <- .colorMapListToVector(newColorMap)
                                           values <<- newValues
                                       },
                                  updateFromResponse=function(response) {
                                      getcol <- function(name) sapply(response$defs, "[[", name)
                                      df <- data.frame(id=getcol("id"),
                                                       type=getcol("type"),
                                                       defaultValue=getcol("defaultValue"),
                                                       label=getcol("label"))
                                      posVals <- sapply(response$defs, function(x) {
                                          paste(x$possibleValues,"",collapse=",")
                                      })
                                      df$possibleValues <- posVals
                                      defs <<- df
                                      values <<- response$vals

                                      values$colorMap <<- .colorMapListToVector(response$colorMap)
                                      tmp <- response$colorMap
                                      
                                      initialized <<- TRUE
                                  },
                                       set=function(newVals) {
                                           if (colorMap %in% names(newVals)) {
                                               oldColorMap <- values$colorMap
                                               colorMap <- newVals$colorMap

                                               names <- names(colorMap)
                                               allnames <- names(oldColorMap)
                                               extraNames <- setdiff(names, allnames)
                                               colorMap[extraNames] <- NULL

                                               if (length(colorMap) > 0) {
                                                   missingNames <- setdiff(allnames, names(colorMap))
                                                   colorMap[missingNames] <- oldColorMap[missingNames]
                                               }

                                               colorMap <- .colorMapVectorToList(colorMap)
                                           } else {
                                               colorMap <- list()
                                           }
                                           newVals$colorMap <- NULL

                                           ids <- names(newVals)
                                           allIds <- defs$id

                                           extraIds <- setdiff(ids, allIds)
                                           newVals[extraIds] <- NULL

                                           if (length(newVals) > 0) {
                                               missingIds <- setdiff(allIds, names(newVals))
                                               newVals[missingIds] <- values[missingIds]
                                           }

                                           if (length(newVales) == 0 && length(colorMap) == 0) {
                                               return(NULL)
                                           }

                                           out <- list(settings=newVals, colors=colorMap)
                                           return(out)
                                  }))
