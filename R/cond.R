
#' @export
cond <- function(Count){
  condition <- data.frame(SampleName=c(colnames(Count)), SampleType=c(unlist(strsplit(colnames(Count), "_.*"))))
  row.names(condition) <- condition$SampleName
  condition <-subset(condition, select = -c(SampleName))
  print(condition)
  user_input <- readline("Does this look correct? (Y/N)  ")
  if(user_input != 'Y') stop('Manually load the conditions to condition')
  return(condition)
}
