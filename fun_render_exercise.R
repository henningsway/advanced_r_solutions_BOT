render_exercise <- function(exercise_df, tmp = TRUE){
  # compute parameters
  tmp_rmd <- tempfile(fileext = ".Rmd")
  # file.copy("preamble.tex", "/tmp/RtmpjU0wYL/preamble.tex")

  # build source document
  writeLines(glue::glue('---
output:
  pdf_document:
    highlight: default
geometry: "left=1cm, right=1cm, top=1cm, bottom=1cm"
---
\\eject \\pdfpageheight={{4 + str_count(exercise, "\\n") * 0.9}}cm
\\fontsize{12}{22}
\\selectfont

```{r setup, include=FALSE}
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)
library(rlang)
library(lobstr)
```

# {{part}} | {{chapter}} | {{str_sub(subchapter, start = 3)}}
## {{exercise_id}}
{{exercise}}
', .envir = exercise_df, .open = "{{", .close = "}}"), tmp_rmd)
  
  # render to pdf
  tmp_pdf <- tempfile(fileext = "pdf")
  rmarkdown::render(tmp_rmd, output_file = tmp_pdf)
  
  # transform to image
  if(tmp){
    file_name <- tempfile(fileext = ".png")
  } else {
    file_name <- paste0(snakecase::to_any_case(with(exercise_df, paste(part, chapter, subchapter, exercise_id))), ".png")
  }
  pdftools::pdf_convert(tmp_pdf, dpi = 150, filenames = file_name) %>% 
    normalizePath()
}

# render_exercise(advr_parsed_exercises_df[13, ])


# "safely" render all exercises
if(FALSE){
  render_exercise_p <- possibly(render_exercise, otherwise = NA)
  advr_sourced_df %>% 
    rowid_to_column() %>% 
    group_by(rowid) %>% 
    tidyr::nest() %>% 
    pluck("data") %>% 
    walk(render_exercise_p)
}
