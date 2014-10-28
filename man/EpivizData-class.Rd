\name{EpivizData-class}
\docType{class}

\alias{class:EpivizData}
\alias{EpivizData-class}

\title{'EpivizData' objects}

\description{This class serves as the basis for the epiviz data container class hierarchy.}

\details{Objects of classes derived from this class define \code{measurements} in the measurement/chart
design behind epiviz. Subclasses extended from this class correspond to specific data types,
and are responsible for packaging data for display in the epiviz web app. In principle,
each subclass corresponds to a data type in the epiviz JS framework. Use the \code{listTypes} method
of \code{\link{EpivizDeviceMgr-class}} to see the R-JS class correspondence. This virtual class implements
common functionality for subclass objects. Objects in this class hierarchy are created using 
\code{link{register-methods}}.}

\section{Fields}{
  \describe{
    \item{\code{object}:}{The data containing object}
    \item{\code{mgr}:}{The epiviz session manager (object of class \code{\link{EpivizDeviceMgr-class}})}
    \item{\code{inDevice}:}{(logical) TRUE if object defined in a \code{\link{EpivizDevice-class}} object.}
    \item{\code{id}:}{(character) the measurement object id determined by the session manager. Use \code{getId}
    method to get this field.}
    \item{\code{name}:}{(character) the measurement object name. Measurements declared by the object are given
    names derived from this field. Use \code{getName} method to get this field.}
    \item{\code{columns}:}{(character vector) the names of the columns containing measurements encapsulated by this object.}
    \item{\code{ylim}:}{(numeric matrix) data range for measurements encapsulated by this object. These are passed to the web app
    to determine plot ranges.}
    \item{\code{curQuery}:}{(GRanges object) last query made to this object.}
    \item{\code{curHits}:}{(integer) indexes returned by last query to this object.}
  }
}

\section{Methods}{
  Below, methods tagged as 'VIRTUAL' must be implemented by subclasses.
  \describe{
    \item{}{
      \code{.checkColumns(columns)}: Check if argument \code{columns} is valid for data object. 'VIRUTAL'.
    }
    
    \item{}{
      \code{.getColumns()}: Infer a valid set of \code{columns} from data object. 'VIRTUAL'.
    }
    
    \item{}{
      \code{.checkLimits(ylim)}: Check if argument \code{ylim} is valid for data object. 'VIRTUAL'.
    }
    
    \item{}{
      \code{.getLimits()}: Infer a valid set of \code{ylim} from data object. 'VIRTUAL'. 
    }
    
    \item{}{
      \code{update(newObject, sendRequest=TRUE)}: updates data object to argument \code{newObject}. This method checks that the classes
      of \code{newObject} and the current \code{object} are the same, that the \code{columns} field is valid for \code{newObject} and 
      updates the \code{ylim} field using the \code{.getLimits} method. If \code{sendRequest==TRUE} the web app is refreshed to
      reflect the data in \code{newObject}.
    }
    
    \item{}{\code{getId()}: Get the object's \code{id}.}
    \item{}{\code{setId(id)}: Set the object's \code{id}. For internal use only.}
    \item{}{\code{getName()}: Get the object's \code{name}.}
    \item{}{\code{setName(name)}: Set the object's \code{name}. For internal use only.}
    \item{}{\code{setLimits(ylim)}: Set the object's \code{ylim} field. Argument is checked using the \code{.checkLimits} method.}
    \item{}{\code{getMeasurements()}: Get the object's encapsulated measurements in format appropriate for web app. VIRTUAL, for internal use only.}
    \item{}{\code{parseMeasurement(msId=NULL)}: Parse measurement id
    \code{msId} to extract object id and column. VIRTUAL, for internal use only.}
    \item{}{\code{setMgr()}: Set the associated session manager. For internal use only.}
    \item{}{\code{setInDevice(x)}: Set the \code{inDevice} field to \code{x}. For internal use only.}
    \item{}{\code{plot()}: Plot data in this object. VIRTUAL.}
    
    \item{}{
	\code{getRows(query, metadata)}: Get rows overlapping \code{query}, 
	a \code{GenomicRanges} object. \code{metadata}: list of metadata columns;
	metadata values returned for each row.
    }

    \item{}{
	\code{getValues(query, measurement)}: Get \code{measurement} values for
	rows overlapping \code{query}, a \code{GenomicRanges} object.
    }
  }
}


\author{Hector Corrada Bravo}
