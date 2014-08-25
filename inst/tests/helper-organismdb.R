makeGeneInfo <- function() {
  library(bumphunter)

  tt <- bumphunter::TT
  tt <- keepSeqlevels(tt, paste0("chr",c(1:22,"X","Y")))
  tt
}

