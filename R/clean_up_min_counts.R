
#' @export
clean_up_min_counts <- function(data, min_count_avg){
  count_combined_HGNC_clean <- count_combined_HGNC[rowMeans(count_combined_HGNC) > min_count_avg, ]
  return(count_combined_HGNC_clean)
}

