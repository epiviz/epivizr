make_test_gene_info <- function() {
  library(bumphunter)

  tt <- bumphunter::TT$transcripts
  tt <- keepSeqlevels(tt, paste0("chr",c(1:22,"X","Y")), pruning.mode="coarse")
  tt
}

