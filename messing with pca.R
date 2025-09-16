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

# 1. Convert character columns to numeric (using as.numeric(as_factor()))
data <- data %>%
  mutate(
    across(where(is.character), ~ as.numeric(as_factor(.))),
    across(where(haven::is.labelled), ~ as.numeric(.)),
    across(where(is.factor), ~ as.numeric(.))
  )

# 2. Remove columns with near zero variance
nzv <- nearZeroVar(data)
if(length(nzv) > 0) {
  data <- data[, -nzv, drop = FALSE]
}

# 3. Handle NA values:
#    - Impute columns with <10% NAs using median
#    - Drop columns with >=10% NAs

na_prop <- sapply(data, function(x) mean(is.na(x)))
cols_to_impute <- names(na_prop[na_prop > 0 & na_prop < 0.10])
cols_to_drop   <- names(na_prop[na_prop >= 0.10])

# Impute with median
for(col in cols_to_impute) {
  med <- median(data[[col]], na.rm = TRUE)
  data[[col]][is.na(data[[col]])] <- med
}

# Drop high-NA columns
if(length(cols_to_drop) > 0) {
  data <- data %>% select(-all_of(cols_to_drop))
}

# Now 'data' is ready for PCA
prcomp(data, scale. = TRUE)
screeplot(pca_results)

# Proportion of variance explained by each PC
explained_var <- pca_results$sdev^2 / sum(pca_results$sdev^2)
cumulative_var <- cumsum(explained_var)
print(explained_var)
plot(explained_var)
print(cumulative_var)
plot(cumulative_var)

# Loadings: contributions of each variable to each PC
loadings <- pca_results$rotation
# View loadings for PC1
print(loadings[, 1]) #This shows that highest educational attainment, 
#roof material, floor material, and birth control explained significant 
# #amounts of variance in the data

# Variables with highest (absolute) contribution to PC1
head(sort(abs(loadings[, 1]), decreasing = TRUE), 5)
 #v762bz    v762br    v762bf    v762bx    v762bo 
 #0.1344685 0.1341633 0.1341556 0.1341556 0.1341551 
 # This shows that highest contribution to PC1 are all female contraception related

# Scores: coordinates of your samples in PC space
scores <- pca_results$x
# Plot the first two PCs
plot(scores[, 1], scores[, 2], 
     xlab = "PC1", ylab = "PC2", 
     main = "PCA: Individuals in PC1 vs PC2 space")

biplot(pca_results, scale = 0)


# Number of top variables to show
top_n <- 10

# Get loadings for PC1 and PC2
loadings <- pca_results$rotation[, 1:2]
loading_magnitude <- apply(abs(loadings), 1, max) # Use max or sum(abs(...))
top_vars <- names(sort(loading_magnitude, decreasing = TRUE)[1:top_n])

# Use the 'factoextra' package for a clean biplot (install if needed)
if(!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra")
library(factoextra)

fviz_pca_biplot(
  pca_results,
  select.var = list(name = top_vars), # only top variables
  repel = TRUE, # better label spacing
  col.var = "red", 
  col.ind = "black"
)

# Plot only a random subset of 200 individuals
set.seed(42)
ind_sample <- sample(1:nrow(pca_results$x), 200)
fviz_pca_biplot(
  pca_results,
  select.ind = list(name = ind_sample),
  select.var = list(name = top_vars),
  repel = TRUE
)

#Plot only variables, no individuals
fviz_pca_var(pca_results, col.var = "red", repel = TRUE)

#problem: many age related variables are collinear. Need to control for this
