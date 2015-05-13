\name{EpivizDeviceMgr-class}
\docType{class}

\alias{class:EpivizDeviceMgr}
\alias{EpivizDeviceMgr-class}
\alias{EpivizDeviceMgr}

\title{'EpivizDeviceMgr' objects}

\description{
  This class manages interactive visualization connections to the epiviz
  web app. It provides a mechanism to add measurements (from objects in the R/bioc
  workspace), charts (plots and tracks in the web app) or devices (a one-step combination of
  measurements and charts). It also provides navigation and other interactive functions.
  Setters and getters are defined for the pertinent fields, which should
  not be accessed directly. Objects of this class are usually created using the
  \link{startEpiviz} function.
}

\details{
  The most important aspect of interactive connections to the epiviz web app is the ability to add tracks and plots
  displaying data from an R/bioc session. This class is designed so that data and display are distinct, the former
  represented by \code{measurements} encapsulated in \code{\link{EpivizData-class}} objects, the latter by \code{charts}
  encapsulated by \code{\link{EpivizChart-class}} objects. 
  
  For a user to visualize data stored in an object that can be referenced by genomic location, e.g., a 
  \code{\link[GenomicRanges]{GRanges}} or \code{\link[SummarizedExperiment]{RangedSummarizedExperiment}} objects, they must first indicate
  to the session manager which \code{measurements} are provided by this object using the \code{addMeasurements} method. This creates
  an object derived from class \code{\link{EpivizData-class}} (using \code{\link{register-methods}}) which wraps the data object 
  so that efficient overlap queries are performed (using \code{\link[GenomicRanges]{GIntervalTree}} objects) and adds the set of 
  measurements provided to the web app. This allows users to create charts displaying data from this object 
  either by using the \code{addChart} method or the web app UI itself. See section 'Measurement management methods' below for more information.
  
  Once \code{measurements} have been declared, users can use the \code{addChart} method to visualize them in one or more different
  plot types (see section 'Chart types' below). Objects from classes derived from \code{\link{EpivizData-class}} have \code{plot} methods
  that use reasonable default chart types. See section 'Chart management methods' below for more information.
  
  We also provide a 'device' interface to simplify the steps above. Users can call the \code{addDevice} method, passing a data object
  to be visualized and a chart is added with the default visualization for the object. The same mechanism above is used, so both
  a \code{\link{EpivizData-class}} and \code{\link{EpivizChart-class}} objects are added to the session manager. See section 'Device
  management methods' below for more information.
}

\section{Fields}{
  \describe{ 
    \item{\code{url}:}{The url used to connect to the epiviz web app}
    \item{\code{msList}:}{List of added measurements (objects of classes that inherit from \code{\link{EpivizData-class}})}
    \item{\code{typeMap}:}{List of currently available measurements types}
    \item{\code{msIdCounter}:}{Used internally to manage \code{msList}}
    \item{\code{chartList}:}{List of currently added charts (objects of class \code{\link{EpivizChart-class}})}
    \item{\code{chartIdCounter}:}{Used internally to manage \code{chartList}}
    \item{\code{activeId}:}{Id of currently active chart (not used)}
    \item{\code{chartIdMap}:}{Character vector of chart ids assigned by the web app. Vector names are chart ids set by the session manager}
    \item{\code{deviceList}:}{List of currently added devices (objects of class \code{\link{EpivizDevice-class}})}
    \item{\code{deviceIdCounts}:}{Used internally to manage \code{deviceList}}
    \item{\code{server}:}{Object of class \code{\link{EpivizServer-class}} encapsulating the websocket connection to app}
    \item{\code{verbose}:}{Log to console on requests received}
    \item{\code{nonInteractive}:}{Run in non-interactive mode, for internal development use only}
    \item{\code{callbackArray}:}{Object of class \code{\link{IndexedArray-class}} containing request callback functions}
  }
}

\section{Utility methods}{
  \describe{
    \item{}{
      \code{bindToServer()}: Call the \code{bindManager} function of the \code{server} object passing this object as argument.
    }
    
    \item{}{
      \code{isClosed()}: Returns \code{TRUE} if connection to app is closed.
    }
    
    \item{}{
      \code{openBrowser(url=NULL)}: Open browser for epiviz web app, if argument \code{url} is \code{NULL} (the default), the \code{url} field is used.
    }
    
    \item{}{
      \code{service()}: Service requests from app (this call blocks the R/interactive session until loop is escaped)
    }
    
    \item{}{
      \code{stopService()}: Stop the service loop
    }
    
    \item{}{
      \code{startServer()}: Start the websocket connection server
    }
    
    \item{}{
      \code{stopServer()}: Stops the websocket connection server. Also removes all currently added devices, charts and measurements from web app.
    }
  }
}

\section{Seqinfo methods}{
  \describe{
    \item{}{
      \code{addSeqinfo(x)}: Add sequence space (e.g., chromosome) informationto the web app.
      \code{x} should be a \code{\link[GenomeInfoDb]{Seqinfo-class}} object, or a named integer (or numeric) vector.
    }
    \item{}{
      \code{rmSeqinfo(seqnames)}: Remove sequence spaces (e.g., chromosome) information
      from the web app. \code{seqnames} should be a character vector containing chromosome 
      names.
    }
  }		 
}

\section{Navigation methods}{
  \describe{
    \item{}{
      \code{refresh()}: NOT SUPPORTED YET
    }
    
    \item{}{
      \code{navigate(chr, start, end)}: Navigate in web app to give genomic region
    }
    
    \item{}{
      \code{slideshow(granges, n=length(granges))}: Navigate in web app to the first \code{n} regions in \code{\link{GenomicRanges-class}} object \code{granges} in succession.
    }
  }
}

\section{Measurement management methods}{
  \describe{
    \item{}{
      \code{addMeasurements(obj, msName, sendRequest=TRUE, ...)}: Add measurements in \code{obj}, calls the \code{\link{register-methods}} on
      \code{obj} passing extra arguments in \code{...}. Measurements are given names derived from \code{msName}. Measurements are added to the web
      app itself if \code{sendRequest==TRUE} (the default). This method adds a measuremnt object of class \code{\link{EpivizData-class}}, returned
      by the \code{\link{register-methods}} to the \code{msList} field. Measurement ids are generated automatically using \code{msIdCounter}
      and are returned by this method.
    }
    
    \item{}{
      \code{.clearDatasourceGroupCache(msObj, sendRequest=!nonInteractive)}: Clears data caches in web app using any of the measurements defined by \code{msObj}
      of class (\code{\link{EpivizData-class}}). Only affects web app if \code{sendRequest==TRUE}. This method is for internal use only.
    }
    
    \item{}{
      \code{updateMeasurements(oldObject, newObject, sendRequest=TRUE)}: Update the data object wrapped in \code{\link{EpivizData-class}} object
      \code{oldObject} to \code{newObject}. The class of \code{newObject} depends on the type of data, but is checked for correctness, i.e., the
      the classes of the new and current data objects are the same. Argument \code{oldObject} can be a \code{\link{EpivizData-class}} object
      already added to the session, or a character containing the id of an object. Ids are returned by the \code{addMeasurements} method.
    }
    
    \item{}{
      \code{.getMsObject(msObjId)}: Get the \code{\link{EpivizData-class}} object corresponding the the id \code{msObjId}. This method is for 
      internal use only.
    }
    
    \item{}{
      \code{rmMeasurements(msObj)}: Remove measurements stored in \code{\link{EpivizData-class}} object \code{msObj}. The \code{msObj} argument
      can be an \code{\link{EpivizData-class}} object added to the session manager, or the id of such an object.
    }
    
    \item{}{
      \code{rmAllMeasurements()}: Remove all measurements added to the session manager.
    }
  
    \item{}{
      \code{listMeasurements()}: List measurements added to the session manager.
    }
  
    \item{}{
      \code{getMeasurements()}: Returns list of currently added measurements in format required by web app. This method is for internal use only.
    }
  
    \item{}{
      \code{getMeasurementType(x)}: Returns type of measurement object \code{x} of class \code{\link{EpivizData-class}}.
    }
  }
}

\section{Data fetch methods}{
  \describe{
    \item{}{
	\code{getRows(chr, start, end, metadata, datasource)}: Get genomic location and metadata
	for rows in given \code{datasource} that overlap the region defined by \code{chr},
	\code{start} and \code{end}.
    }

    \item{}{
      \code{getValues(chr, start, end, datasource, measurement)}: Get data values for column
      \code{measurement} for rows in \code{datasource} that overlap the region defined by 
      \code{chr}, \code{start} and \code{end}.
    }      
  }	      
}

\section{Chart management methods}{
  \describe{
    \item{}{
      \code{addChart(chartObject, sendRequest=TRUE, ...)}: Adds chart specified by \code{chartObject} of class \code{\link{EpivizChart-class}} to the
      session manager. The chart is added to the web app if \code{sendRequest==TRUE} (the default). The \code{chartObject} is inserted to the
      \code{chartList} field. Chart ids are generated automatically using \code{chartIdCounter} and are returned by this method.
    }
    
    \item{}{
      \code{.getChartObject(chartId)}: Returns the \code{\link{EpivizChart-class}} object corresponding to id \code{chartId}.
    }
    
    \item{}{
      \code{rmChart(chartObj)}: Remove chart corresponding to \code{chartObj}. Argument \code{chartObj} can be an object of class
      \code{\link{EpivizChart-class}} or the character id of such an object.
    }
    
    \item{}{
      \code{rmAllCharts()}: Remove all charts currently in the session manager.
    }
    
    \item{}{
      \code{listCharts()}: List all charts currently added to session manager.
    }
  }
}

\section{Chart types}{
  \describe{
    \item{}{
      \code{blockChart(ms, ...)}: Used to display data in \code{\link[GenomicRanges]{GenomicRanges}} objects as genomic regions using rectangles
      in a browser track. \code{ms} is a list of lists, each component describing a measurement provided by an object of class \code{\link{EpivizBlockData-class}}. 
      They can be obtained using the \code{getMeasurements} method in the \code{\link{EpivizBlockData-class}}. \code{plot} method for \code{\link{EpivizBlockData-class}} calls this method.
    }
    
    \item{}{
      \code{lineChart(ms, ...)}: Used to display continuous data at base-pair level as a line plot in a browser track. 
      \code{ms} is a list of lists, each component describing a measurement provided by objects of class \code{\link{EpivizBpData-class}}. 
      They can be obtained using the \code{getMeasurements} method in the \code{\link{EpivizBlockData-class}}. The \code{plot} method for 
      \code{\link{EpivizBpData-class}} calls this method.
    }
    
    \item{}{
      \code{scatterChart(x, y, ...)}: Used to display genomic feature data in \code{\link[SummarizedExperiment]{RangedSummarizedExperiment}} objects
      as a scatter plot. \code{x} and \code{y} are lists for measurements provided by objects of class \code{\link{EpivizFeatureData-class}}.
      They can be obtained using the \code{getMeasurements} method in the \code{\link{EpivizFeatureData-class}}.
      The \code{plot} method for \code{\link{EpivizFeatureData-class}} calls this method.
    }

    \item{}{
      \code{heatmapChart(ms, ...)}: Used to display genomic feature data in \code{\link[SummarizedExperiment]{RangedSummarizedExperiment}} objects
      as a heatmap. \code{ms} is a list of lists, each component describing a measurement provided by an object of class \code{\link{EpivizFeatureData-class}}.
      They can be obtained using the \code{getMeasurements} method in the \code{\link{EpivizFeatureData-class}}.
    }

   \item{}{
     \code{genesChart(ms, ...)}: Used to display a gene annotation track. \code{ms} is a list 
     containing a single entry: a list describing data provided by an object of class
     \code{\link{EpivizGeneInfoData-class}}. This list can be obtained using the 
     \code{getMeasurements} method in the \code{\link{EpivizGeneInfoData-class}}.
   } 
 }
}

\section{Device management methods}{
  \describe{
    \item{}{
      \code{addDevice(obj, devName, sendRequest=TRUE, ...)}: Adds device for object \code{obj}. This method calls the
      \code{addMeasurements} method on \code{obj} and calls the \code{plot} method of the resulting \code{\link{EpivizData-class}} object.
      Measurements and charts added by this called are given names derived from \code{devName}. Arguemnts to \code{addMeasurement} and the
      \code{plot-methods} call can be given in \code{...}. Device ids are generated automatically using \code{devIdCounter} and are returned
      by this function. An object of class \code{\link{EpivizDevice-class}} is created and inserted into \code{devList}.
    }
    
    \item{}{
      \code{rmDevice(devObj)}: Remove the device corresponding to \code{devObj}: an object of class \code{\link{EpivizDevice-class}} or
      the id for such an object. This removes the corresponding chart and measurement objects from the session manager.
    }
    
    \item{}{
      \code{rmAllDevices()}: Remove all devices added to the session manager.
    }
    
    \item{}{
      \code{updateDevice(oldObject, newObject, sendRequest=TRUE, ...)}: Update the data object wrapped in \code{\link{EpivizDevice-class}} object
      \code{oldObject} to \code{newObject}. The class of \code{newObject} depends on the type of data, but is checked for correctness, i.e., the
      the classes of the new and current data objects are the same. Argument \code{oldObject} can be a \code{\link{EpivizDevice-class}} object
      already added to the session, or a character containing the id of an object. Ids are returned by the \code{addDevice} method.
    }
    
    \item{}{
      \code{listDevices()}: List all devices added to the session manager.
    }
  }
}

\author{Hector Corrada Bravo}

\seealso{
  \link{startEpiviz}
  \link{EpivizData-class}
  \link{EpivizChart-class}
  \link{EpivizDevice-class}
  \link{GenomicRanges-class}
  \link{EpivizServer-class}
  \link{register-methods}
  \link{IndexedArray-class}
}

\examples{
\dontrun{
  require(epivizrData)
  data(tcga_colon_example)
  
  mgr <- startEpiviz(openBrowser=interactive())
  
  # using the device interface
  blockDev <- mgr$addDevice(colon_blocks, "blocks_test", type="block")
  
  # using the measurement/chart interface
  # add measurements
  blockMs <- mgr$addMeasurements(colon_blocks, "blocks_test2", type="block")
  
  # add chart
  blockChart <- mgr$blockChart(blockMs$getMeasurements()[1])
  
  # using plot methods
  blockChart2 <- blockMs$plot()
  
  # list devices
  mgr$listDevices()
  
  # list measurements
  mgr$listMeasurements()
  
  # list charts
  mgr$listCharts()
  
  # remove a chart
  mgr$rmChart(blockChart2)
  
  # navigate to genomic region
  mgr$navigate("chr2", 10000000, 30000000)
  mgr$stopServer()
}
}
