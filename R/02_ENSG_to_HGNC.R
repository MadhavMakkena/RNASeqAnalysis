
#' @export
ENSG_to_HGNC <- function(data){
  print("Converting ENSG to HGNC and removing genes with mean counts of 0")
  print("If you run into an error ensure that the following packages are installed correctly:")
  print("BinfTools")
  count_combined_HGNC <- BinfTools::getSym(data,
                        obType="counts",
                        species="hsapiens",
                        target="HGNC",
                        addCol=F)
  print("This function is a wrapper to getsym function created by Kevin Nixon")
  print("https://github.com/kevincjnixon")
  return(count_combined_HGNC)
}
