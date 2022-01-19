
#' @export
single_files <- function(folder_location){
  setwd(folder_location)
  print("If you run into an error ensure that the following packages are installed correctly:")
  print("dplyr, tidyverse, tibble, purrr, plyr")
  # print(list.files())
  # user_input <- readline("Confirm the files? (Y/N)  ")
  # if(user_input != 'Y') stop('Enter the correct file location')
  count_lists <- lapply(list.files(), read.csv, sep=",", header=TRUE, row.names="ENSG")
  count_combined <- map(count_lists, ~ .x %>%
                          tibble::rownames_to_column("ENSG")) %>%
    purrr::reduce(full_join, by = "ENSG") %>%
    dplyr::mutate(across(everything(), replace_na, 0))
  count_combined <- tibble::column_to_rownames(count_combined, var = "ENSG")
  count_combined <- count_combined %>% filter(str_detect(row.names(count_combined) , "ENSG*"))
  print(head(count_combined))
  user_input <- readline("Does this look correct? (Y/N)  ")
  if(user_input != 'Y') stop('Enter the correct folder location')
  # print("Success count_combined returned")
  count_combined_ENSG <- count_combined
}

#' @export
combined_file <- function(file_location, file_name){
  setwd(file_location)
  count_combined <- read.csv(file_name, header = TRUE, row.names = 1)
  print(head(count_combined))
  user_input <- readline("Does this look correct? (Y/N)  ")
  if(user_input != 'Y') stop('Enter the correct file location and name')
  # print("Success count_combined returned")
  count_combined_ENSG <- count_combined
  return(count_combined_ENSG)
}
