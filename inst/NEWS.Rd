\name{epivizrnews}
\title{Epivizr News}

\encoding{UTF-8}

\section{Version 1.4.6}{
  \itemize{
    \item Switch default URL to still use epiviz version 2
  }
}

\section{Version 1.3.20}{
  \itemize{
    \item Standalone mode introduced, a version of epiviz with reduced capabilities
    	  is now included as part of epivizr. The epiviz web app is run locally using 'httpuv's
	  http server
    \item Add and remove seqinfo (e.g., chromosome info) to any epiviz session
  }		 
}

\section{Version 1.3.11}{
  \itemize{
    \item Add NEWS file
    \item Update documentation on 'slideshow' function
  }
}

\section{Version 1.3.10}{
  \itemize{
    \item Changed default on 'slideshow' to show all ranges
  }
}

  \section{Version 1.3.9}{
    \itemize{
      \item Added 'heatmapChart' convenience function
    }
  }

  \section{Version 1.3.8}{
    \itemize{
      \item Fixed bug in 'startEpiviz' not sending 'seqName' parameter correctly
    }
  }

  \section{Version 1.3.7}{
    \itemize{
      \item Fixed bug in 'EpivizBpData' that sent 'metadata' info in
    wrong format
    }
  }

  \section{Version 1.3.6}{
    \itemize{
      \item Changes slots using lists in 'EpivizDeviceMgr' to
    environments to avoid crashing RStudio due to inspection of manager
    objects
    }
  }

  \section{Version 1.3.5}{
    \itemize{
      \item Fails gracefully on daemonization request on Windows
      \item Deprecates the 'proxy' argument to 'startEpiviz'
    }
  }

  \section{Version 1.3.4}{
    \itemize{
      \item Upgrading to Epiviz v2 webapp
    }
  }
