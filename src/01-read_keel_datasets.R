library(tidyverse)

path_in <- "data/raw/keel-datasets/"
files <- list.files(path_in)

path_out <- "data/01-keel_datasets/"

i <- 0
for (f in files){
  i <- i + 1
  cat("Reading", f, "(", i, "of", length(files), ")\n")
  
  path_file <- paste0(path_in, f, "/", f, ".dat")
  
  if (file.exists(path_file)){
    keel_lines <- read_lines(path_file)

    var_names <- keel_lines[str_starts(keel_lines, "@[Aa]ttribute")] %>% 
      str_split("[ {]") %>% 
      map_chr(function(x) x[2]) %>% 
      str_replace_all("-", "_") %>% 
      str_replace_all("/", "_") %>% 
      str_replace_all("\\(", "") %>% 
      str_replace_all("\\)", "") %>% 
      str_to_lower() %>% 
      paste(collapse = ",")
    
    keel_lines <- keel_lines[!str_starts(keel_lines, "@")]
    
    keel_lines <- c(var_names, keel_lines)
    
    write_lines(keel_lines, file = paste0(path_out, f, ".csv"))    
  }
}