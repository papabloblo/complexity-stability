library(ECoL)
library(tidyverse)

path_in <- "data/02-clean_datasets/"
files <- list.files(path_in)

files_to_remove <- list.files("data/04-complexity_measures/") %>% 
  str_remove(".rds") %>% 
  paste0(".csv")


files_to_remove <- c(files_to_remove,
                     "nursery-classnot_recom.csv", 
                     "nursery-classpriority.csv",
                     "nursery-classspec_prior.csv",
                     "adult-class<=50K.csv",
                     "adult-class>50K.csv",
                     "coil2000-class0.csv",
                     "connect-4-classloss.csv",
                     "connect-4-classwin.csv",
                     "fars-classFatal_Injury.csv",
                     "fars-classIncapaciting_Injury.csv", 
                     "fars-classNo_Injury.csv",
                     "fars-classNonincapaciting_Evident_Injury.csv",
                     "kr-vs-k-classeleven.csv",
                     "kr-vs-k-classfourteen.csv",
                     "kr-vs-k-classthirteen.csv",
                     "kr-vs-k-classtwelve.csv",
                     "magic-classg.csv",
                     "magic-classh.csv",
                     "marketing-class6.csv",
                     "marketing-class7.csv",
                     "marketing-class9.csv",
                     "mushroom-classe.csv",
                     "penbased-class0.csv",
                     "penbased-class1.csv",
                     "penbased-class2.csv",
                     "penbased-class4.csv",
                     "penbased-class7.csv",
                     "ring-class0.csv",
                     "ring-class1.csv",
                     "shuttle-class1.csv",
                     "shuttle-class4.csv",
		     "satimage-class2.csv",
		     "thyroid-class3.csv",
		     "titanic-class-1.csv",
		     "titanic-class1.csv",
		     "twonorm-class1.csv"
                     )

files <- files[!(files %in% files_to_remove)]
i <- 0
for (file in files){
  i <- i + 1
  cat("Calculating complexity of", file, "(", i, "of", length(files), ")\n")
  
  df <- read_csv(paste0(path_in, file),
                 show_col_types = FALSE)
  
  target_name <- names(df)[1]
  
  
  if (mean(df[[target_name]]) > .1){
    
    df <- df[, sapply(df, function(x) length(unique(x))) > 1]
    
    predictors_names <- names(df)[names(df) != target_name]
    
    df_complex <- complexity(x = df[, predictors_names], 
                             y = factor(df[[target_name]], levels = c(0, 1))
                             )
    
    saveRDS(df_complex,
            file = paste0("data/04-complexity_measures/", 
                          str_remove(file, ".csv"),
                          ".rds")
            )
  }
}



