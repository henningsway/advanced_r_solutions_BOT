parse_source <- function(advr_source_df){
  
  require(tidyverse)
  
  rmd <- readLines(advr_source_df$chapter_sources)
  part_name <- advr_source_df$parts
  chapter_name <- advr_source_df$chapter_names  #remove plural later

  get_rows_subchapter_exerc <- function(rmd){
    rows_subchapter_all <- str_which(rmd, "^## ")
    rows_exercises_start <- str_which(rmd, "^### Exercises")
    
    # Find Subchapters with Exercises
    l <- list()
    for (i in seq_along(rows_exercises_start)){
      l[[i]] <- max(rows_subchapter_all[rows_subchapter_all < rows_exercises_start[i]])
    }
    
    # return character vector of subchapter names
    unlist(l)  
  }
  
  get_names_subchapter <- function(rmd, rows_subchapter_exerc){
    subchapter_names <- rmd[rows_subchapter_exerc] %>% 
      str_sub(start = 3) %>% 
      str_remove("\\{.+\\}$") %>% 
      str_trim()
    
    paste(seq_along(subchapter_names), subchapter_names)
  }
  
  get_rows_exercises <- function(rmd){
    # "initialize"
    rows_subchapter_all <- str_which(rmd, "^## ")
    rows_exercises_start <- str_which(rmd, "^### Exercises")
    rows_subchapter_exerc <- get_rows_subchapter_exerc(rmd)
    
    
    following_subchapter <- which(rows_subchapter_all %in% rows_subchapter_exerc)
    rows_exercises_end <- rows_subchapter_all[following_subchapter + 1] %>% 
      tidyr::replace_na(length(rmd)) # if last subchapter has exercise
    
    # return (named) list of exercise lines for one subchapter
    rows_exercises_start %>% 
      map2(rows_exercises_end, `:`) %>% 
      set_names(get_names_subchapter(rmd, rows_subchapter_exerc))
    ## unattended CORNERCASE
    ## (Last Exercise block, last exercise may include prose at the end)
  }
  
  get_rows_exercises(rmd)
  
  ## EXERCISE PARSING
  get_exercise_by_row <- function(lines, res){
    paste(res[lines], collapse = "\n")
  }
  
  parse_exercises <- function(rows, rmd){
    exerc_raw <- rmd[rows]
    
    exerc_start <- str_which(exerc_raw, "^1. ")
    exerc_end <- c(exerc_start[-1], length(exerc_raw)) - 1
    
    exerc_lines <- map2(exerc_start, exerc_end, `:`) %>% 
      set_names(paste("Exercise", seq_along(exerc_end)))
    
    exerc_lines %>% 
      map(get_exercise_by_row, exerc_raw)
  }
  
  
  exerc_raw_list <- get_rows_exercises(rmd) %>% 
    map(parse_exercises, rmd)
  
  exerc_list <- exerc_raw_list %>% 
    purrr::flatten()
  
  # Improvements
  # think about structuring into dataframe
  # think about exercise-BLOCK-naming
  
  
  
  # build data frame
  # part, chapter, subchapter, exercise_id, exercise
  exercise <- exerc_list %>% flatten_chr() %>% str_sub(start = 4) %>% str_replace("    ```", "```")
  exercise_id <- names(exerc_list)
  subchapter <- map2(names(exerc_raw_list), map_dbl(exerc_raw_list, length), rep) %>% unlist()
  chapter <- rep(chapter_name, length(exercise_id))
  part <- rep(part_name, length(exercise_id))
  
  parsed_exerc_df <- tibble(part, chapter, subchapter, exercise_id, exercise)
  parsed_exerc_df
}
