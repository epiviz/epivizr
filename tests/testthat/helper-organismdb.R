make_test_gene_info <- function() {
  tt <- bumphunter::TT$transcripts
  tt <- GenomeInfoDb::keepSeqlevels(tt, paste0("chr",c(1:22,"X","Y")), pruning.mode="coarse")
  tt
}

