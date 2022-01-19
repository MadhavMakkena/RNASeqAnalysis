
#' @export
DEG_data <- function(result, count, result_up, result_down, result_nochange){
  Complete_Data <- merge(count, result, by = 0)
  rownames(Complete_Data)=Complete_Data$Row.names
  Complete_Data <- subset(Complete_Data, select = -c(Row.names))
  Up_Data <- Complete_Data[row.names(Complete_Data) %in% row.names(result_up), ]
  Down_Data <- Complete_Data[row.names(Complete_Data) %in% row.names(result_down), ]
  Nochange_Data <- Complete_Data[row.names(Complete_Data) %in% row.names(result_nochange), ]
  return(list(Complete_Data=Complete_Data, Up_Data=Up_Data, Down_Data=Down_Data, Nochange_Data=Nochange_Data))
}
