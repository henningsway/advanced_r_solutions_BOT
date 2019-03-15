source("fun_parse_advr.R")

# Prepare some chapter data
chapter_filenames <- c(
  `Names and values` = "Names-values.Rmd",
  Vectors = "Vectors.Rmd",
  Subsetting = "Subsetting.Rmd",
  `Control flow` = "Control-flow.Rmd",
  Functions = "Functions.Rmd",
  Environments = "Environments.Rmd",
  Conditions = "Conditions.Rmd",

  Functionals = "Functionals.Rmd",
  `Function factories` = "Function-factories.Rmd",
  `Function operators` = "Function-operators.Rmd",
  
  `Base types` = NA,
  S3 = "S3.Rmd",
  R6 = "R6.Rmd",
  S4 = "S4.Rmd",
  `Trade-offs` = NA,
  
  `Big picture` = NA,
  Expressions = "Expressions.Rmd",
  Quasiquotation = "Quotation.Rmd",
  Evaluation = "Evaluation.Rmd",
  `Translating R code` = "Translation.Rmd",
  
  Debugging = NA,
  `Measuring performance` = "Perf-measure.Rmd",
  `Improving performance` = "Perf-improve.Rmd",
  `Rewriting R code in C++` ="Rcpp.Rmd"
)

# build df for parsing
five_part_names <- c("Foundations", "Functional Programming",
  "Object oriented programming", "Metaprogramming",
  "Techniques")
parts <- map2(five_part_names %>% paste(seq_along(.), .),
              c(7, 3, 5, 5, 4), rep) %>% unlist()
chapter_names <- names(chapter_filenames) %>% paste(seq_along(.), .)
chapter_sources <- paste0("https://raw.githubusercontent.com/hadley/adv-r/master/", chapter_filenames)

advr_source_df <- tibble(parts, chapter_names, chapter_sources) %>% 
  filter(!is.na(chapter_filenames))


advr_parsed_exercises_df <- advr_source_df %>%
  # hacky
  rowid_to_column() %>% 
  group_by(rowid) %>% 
  tidyr::nest() %>% 
  pluck("data") %>% 
  map(parse_source) %>% 
  bind_rows()

saveRDS(advr_parsed_exercises_df, "advr_parsed_exercises_df.rds")
