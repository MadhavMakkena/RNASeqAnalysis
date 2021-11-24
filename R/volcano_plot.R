
#' @export
volcano_plot_subset <- function(result, pval, log2FC, subset_file_location, subset_file_name){
  setwd(subset_file_location)
  subset_genes <- read.csv(subset_file_name, header = F)$V1
  result_subset<-subset(result[subset_genes,])
  result_up<-subset(result, pvalue<=pval & log2FoldChange >= log2FC)
  result_down<-subset(result, pvalue<=pval & log2FoldChange <= -log2FC)
  result_nochange<-subset(result, (pvalue<=pval & (log2FoldChange >-log2FC &  log2FoldChange < log2FC)) | (pvalue>pval))
  plot(abs(result$stat)~result$log2FoldChange, pch=16, cex=0.6, col="gray80", ylim = c(-10,(ceiling(max(abs(result$stat))))), xlim = c(-(ceiling(max(abs(result$log2FoldChange)))),(ceiling(max(abs(result$log2FoldChange))))), xlab="Log2FoldChange", ylab="Absolute Wald Statistic", main="Differentially Expressed Genes [Fibroblast vs. SH_SY5Y]", cex.main=.85)
  points(abs(result_up$stat)~result_up$log2FoldChange, pch=16, cex=0.6, col="gray60")
  points(abs(result_down$stat)~result_down$log2FoldChange, pch=16, cex=0.6, col="gray60")
  points(abs(result_subset$stat)~result_subset$log2FoldChange, pch=16, cex=0.6, col="darkgreen")
  abline(h=min(abs(subset(result, pvalue<pval)$stat)), v=c(log2FC,-log2FC), lty=2, col="darkseagreen4")
  text(-22,140, (nrow(result_down)), col = "Blue", pos = 4, cex = .85)
  text(-22,130, "Down", col = "Blue", pos = 4, cex = .85)
  text(-22,120, "regulated", col = "Blue", pos = 4, cex = .85)
  text(22,140, (nrow(result_up)), col = "Red", pos = 2, cex = .85)
  text(22,130, "Up", col = "Red", pos = 2, cex = .85)
  text(22,120, "regulated", col = "Red", pos = 2, cex = .85)
  text(0,1, paste((nrow(result_nochange)), " No_change"), col = "Black", pos = 1, cex = .85)
  # text(-22,6, (nrow(result_nochange)), col = "Black", pos = 4, cex = .85)
  # text(-22,-4, "No_change", col = "Black", pos = 4, cex = .85)
  return(list(result_up=result_up, result_down=result_down, result_nochange=result_nochange))
}

#' @export
volcano_plot <- function(result, pval, log2FC){
  result_up<-subset(result, pvalue<=pval & log2FoldChange >= log2FC)
  result_down<-subset(result, pvalue<=pval & log2FoldChange <= -log2FC)
  result_nochange<-subset(result, (pvalue<=pval & (log2FoldChange >-log2FC &  log2FoldChange < log2FC)) | (pvalue>pval))
  plot(abs(result$stat)~result$log2FoldChange, pch=16, cex=0.6, col="gray80", ylim = c(-10,(ceiling(max(abs(result$stat))))), xlim = c(-(ceiling(max(abs(result$log2FoldChange)))),(ceiling(max(abs(result$log2FoldChange))))), xlab="Log2FoldChange", ylab="Absolute Wald Statistic", main="Differentially Expressed Genes [Fibroblast vs. SH_SY5Y]", cex.main=.85)
  points(abs(result_up$stat)~result_up$log2FoldChange, pch=16, cex=0.6, col="gray40")
  points(abs(result_down$stat)~result_down$log2FoldChange, pch=16, cex=0.6, col="gray40")
  abline(h=min(abs(subset(result, pvalue<pval)$stat)), v=c(log2FC,-log2FC), lty=2, col="gray20")
  text(-22,140, (nrow(result_down)), col = "Blue", pos = 4, cex = .85)
  text(-22,130, "Down", col = "Blue", pos = 4, cex = .85)
  text(-22,120, "regulated", col = "Blue", pos = 4, cex = .85)
  text(22,140, (nrow(result_up)), col = "Red", pos = 2, cex = .85)
  text(22,130, "Up", col = "Red", pos = 2, cex = .85)
  text(22,120, "regulated", col = "Red", pos = 2, cex = .85)
  text(0,1, paste((nrow(result_nochange)), " No_change"), col = "Black", pos = 1, cex = .85)
  # text(-22,6, (nrow(result_nochange)), col = "Black", pos = 4, cex = .85)
  # text(-22,-4, "No_change", col = "Black", pos = 4, cex = .85)
  return(list(result_up=result_up, result_down=result_down, result_nochange=result_nochange))
}
