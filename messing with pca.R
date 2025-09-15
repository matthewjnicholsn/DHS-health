library(tidyverse)
library(conflicted)
library(caret)
conflicts_prefer(dplyr::filter())
conflicts_prefer(dplyr::lag())
source("/Users/matthewnicholson/DHS/get_file_function.R")
countries <- c("Kenya_DHS")
years <- c(2003)
surveys <- c("IR")
data <- readRDS(get_file(countries,years,surveys))

library(dplyr)
library(caret)

clean_data <- function(df) {
  df %>%
    mutate(across(where(is.character), ~ as.numeric(as_factor(.)))) %>%
    select(
      -which(nearZeroVar(., saveMetrics = TRUE)$zeroVar),
      -where(~ all(is.na(.)))
    ) %>%
    select(-which(colSums(.) == 0))
}
data <- clean_data(data)

pca_results <- prcomp(data, scale = T)



