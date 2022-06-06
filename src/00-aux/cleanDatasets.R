library(tidyverse)
library(tidymodels)

clean_dataset <- function(dataset_name,
                          file_name,
                          col_names = TRUE,
                          target_name = TRUE,
                          output_path
                          ){
  
  if (isTRUE(col_names)) {
    df <- read_csv(file_name, show_col_types = FALSE)  
    target_name <- names(df)[ncol(df)]
  } else {
    df <- read_csv(file_name,
                   col_names = col_names,
                   show_col_types = FALSE
                   )  
  }
  
  df <- df[, sapply(df, function(x) length(unique(x))) > 1]
  
  df <- recipe(as.formula(paste0(target_name, "~.")), data = df) %>% 
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
    prep(training = df) %>% 
    bake(new_data = NULL) %>% 
    select(target_name, everything())
  
  
  for (class in unique(df[[target_name]])){
    df_aux <- df
    df_aux[[target_name]] <- as.numeric(df[[target_name]] == class)
    
    file_name <- paste0(output_path, 
                        dataset_name,
                        "-class",
                        class,
                        ".csv")
    
    write_csv(df_aux, file_name)
  }
}