
#' @export
run_DESeq <- function(count,Condition){
  DESeq <- DESeq2::DESeqDataSetFromMatrix(countData = count, colData = Condition, design = ~ SampleType)
  DESeq_hist <- sort(rowMeans(DESeq2::counts(DESeq2::estimateSizeFactors(DESeq), normalized = T)), decreasing = T)
  DESeq <- DESeq2::DESeq(DESeq)
  Result <- as.data.frame(DESeq2::results(DESeq))
  Count <- as.data.frame(DESeq2::counts(DESeq, normalized=TRUE))
  return(list(DESeq=DESeq, Result=Result, Count=Count))
}
