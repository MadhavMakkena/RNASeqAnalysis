
#' @export
GO_geneset_DEG <- function(geneset_file_location, geneset_file_name, Complete_Data, Up_Data, Down_Data, Nochange_Data){
  setwd(geneset_file_location)
  subset_genes <- read.csv(geneset_file_name, header = F)
  subset_genes <- dplyr::distinct(subset_genes)
  subset_genes <- as.character(subset_genes[,])
  GO_geneset_Data <- Complete_Data[row.names(Complete_Data) %in% c(subset_genes), ]
  GO_geneset_Up_Data <- GO_geneset_Data[row.names(GO_geneset_Data) %in% row.names(Up_Data), ]
  GO_geneset_Down_Data <- GO_geneset_Data[row.names(GO_geneset_Data) %in% row.names(Down_Data), ]
  GO_geneset_Nochange_Data <- GO_geneset_Data[row.names(GO_geneset_Data) %in% row.names(Nochange_Data), ]
  return(list(GO_geneset_Data=GO_geneset_Data, GO_geneset_Up_Data=GO_geneset_Up_Data, GO_geneset_Down_Data=GO_geneset_Down_Data, GO_geneset_Nochange_Data=GO_geneset_Nochange_Data, subset_genes=subset_genes))
}
