# Helper functions for weather gathering app
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(stringr)
library(readr)
library(here)
library(glue)


# Add a nicely styled and centered label above a given input
labeled_input <- function(id, label, input){
  div(id = id,
      span(label, style = "font-size: small;"),
      input)
}

# Text grob with color matching current theme
themed_text <- function(text){
  grid::textGrob(
    text,
    gp = grid::gpar(col = thematic::thematic_get_option("fg"))
  )
}
