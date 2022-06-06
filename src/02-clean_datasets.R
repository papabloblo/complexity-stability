
library(tidymodels)
source("src/00-aux/cleanDatasets.R")

path_in <- "data/01-keel_datasets/"
files <- list.files(path_in)

i <- 0
for (f in files){
  i <- i + 1
  cat("Cleaning", f, "(", i, "of", length(files), ")\n")
  
  clean_dataset(
    dataset_name = str_remove(f, ".csv"),
    file_name = paste0(path_in, f),
    col_names = TRUE,
    target_name = TRUE,
    output_path = "data/02-clean_datasets/"
  )  
}



