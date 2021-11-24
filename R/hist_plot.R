
#' @export
hist_plot_2x3 <- function(file_location, file_name){
  setwd(file_location)
  hist_genes <- read.csv(file_name, header = F)$V1
  Count_subset_hist_genes <- Count[row.names(Count) %in% hist_genes, ]
  hist_gene_count_max <- (max(Count_subset_hist_genes)*1.1)
  hist_gene_count_min <- (min(Count_subset_hist_genes)*.8)
  par(mfrow=c(2,3))
  for (gene in hist_genes){
    plotCounts(DESeq, gene=gene, intgroup="SampleType", transform=F, ylim=c(hist_gene_count_min, hist_gene_count_max))
  }
}

#' @export
hist_plot_3x4 <- function(file_location, file_name){
  setwd(file_location)
  hist_genes <- read.csv(file_name, header = F)$V1
  Count_subset_hist_genes <- Count[row.names(Count) %in% hist_genes, ]
  hist_gene_count_max <- (max(Count_subset_hist_genes)*1.1)
  hist_gene_count_min <- (min(Count_subset_hist_genes)*.8)
  par(mfrow=c(3,4))
  for (gene in hist_genes){
    plotCounts(DESeq, gene=gene, intgroup="SampleType", transform=F, ylim=c(hist_gene_count_min, hist_gene_count_max))
  }
}
