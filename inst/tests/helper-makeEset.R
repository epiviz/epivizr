makeEset <- function() {
  require(hgu133plus2.db)
  nprobeids=1000
  nsamples=6
  expr=matrix(rnorm(nprobeids*nsamples), nr=nprobeids)
  pd=data.frame(a=1:nsamples,b=10*(1:nsamples))
  rownames(pd)=paste0("SAMP_",1:nsamples)
  rownames(expr)=head(keys(hgu133plus2.db, keytype="PROBEID"), nprobeids)
  colnames(expr)=rownames(pd)

  ExpressionSet(assayData=expr, phenoData=AnnotatedDataFrame(pd),annotation="hgu133plus2")
}

makeSExp <- function() {
	nranges=200
	nsamples=6
	counts1=matrix(runif(nranges*nsamples,1,1e4), nranges)
	counts2=matrix(runif(nranges*nsamples,1,1e2), nranges)

	rowData=sort(GRanges(rep(c("chr1","chr2"), c(50,150)),
		IRanges(floor(runif(200,1e5,1e6)),width=100),
		strand=sample(c("+","-"),200,TRUE),
                probeid=paste0("sid_",1:200)))
	colData=DataFrame(Treatment=rep(c("ChIP","Input"),3),row.names=LETTERS[1:6])
	SummarizedExperiment(assays=SimpleList(counts1=counts1,counts2=counts2),rowData=rowData,colData=colData)
}
