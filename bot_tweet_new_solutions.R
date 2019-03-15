# run every 10 minutes minute
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(rtweet))
suppressPackageStartupMessages(library(glue))

# setup authentication
oauth <- readRDS("data/credentials.Rds")
create_token("advrsoltuions_bot",
             consumer_key = oauth$c_key,
             consumer_secret = oauth$c_secret,
             access_token = oauth$a_token,
             access_secret = oauth$a_secret)

# read new data
url_gh <- "https://raw.githubusercontent.com/Tazinho/Advanced-R-Solutions/master/progress_data.csv"
progress_new_df <- read.table(url_gh, stringsAsFactors = FALSE,
                              sep = "\t", header = TRUE) %>%
  purrr::set_names(tolower) %>%
  as_tibble()

# get progress updates
progress_old_df <- readRDS("data/progress_old_df.Rds")
progress_increment_df <- progress_new_df %>% 
  anti_join(progress_old_df)


if(nrow(progress_increment_df)){
  # join data to prepare data for image
  advr_parsed_exercises_df <- readRDS("data/advr_parsed_exercises_df.rds") %>% 
    modify_at(c("chapter", "subchapter"), tolower)
  source("fun_render_exercise.R")
  
  updated_exercises_df <- progress_increment_df %>% 
    # hacky...
    mutate(exercise_id = paste("Exercise", exercise)) %>% 
    select(-exercise) %>% 
    modify_at(c("chapter", "subchapter"), tolower) %>% 
    left_join(advr_parsed_exercises_df, by = c("subchapter", "exercise_id")) %>% 
    select(part, chapter = chapter.x, subchapter = subchapter, exercise_id = exercise_id, exercise = exercise) %>% 
    filter(subchapter != "tbd")
  
  # render image(s)
  render_exercise_safely <- possibly(render_exercise, otherwise = NULL)
  images <- NULL
  images <- updated_exercises_df %>% 
    rowid_to_column() %>% 
    group_by(rowid) %>% 
    tidyr::nest() %>% 
    pluck("data") %>% 
    map_chr(render_exercise_safely)
  
  # construct message text
  msg <- glue('~~~ New Exercise {progress_increment_df$status %>% unique() %>% paste(collapse = "and")}. ~~~
Please take a look at: https://advanced-r-solutions.rbind.io/
Feedback and QA appreciated.
#rstats #advrsolutions')
  
  # post tweets
  post_tweet_safely <- safely(post_tweet)
  map2(msg, images, post_tweet_safely)
  
    # save status
  saveRDS(progress_new_df, "data/progress_old_df.Rds")
}
