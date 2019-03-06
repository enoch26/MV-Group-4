# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(dplyr)

# IMPORT DATA -------------------------------------------------------------
imdbData <- read.csv("Data/IMDB Data.csv", header = TRUE)

# HAVE A LOOK -------------------------------------------------------------
glimpse(imdbData)

#Change title_year and aspect_ratio to factor
imdbData[c(24,27)] <- lapply(imdbData[c(24,27)], factor) 

#Missing values per column
missingData <- which(is.na(imdbData) | imdbData == "", arr.ind = TRUE) #2698 Missing values
nrow(missingData)

missing_in_each_col <- matrix(NA, nrow = 28, ncol = 2)
for(i in 1:ncol(imdbData)){
  missing_in_each_col[i, 1] <- sum(is.na(imdbData[, i]))
  missing_in_each_col[i, 2] <- sum(imdbData[, i] == "")
}
colnames(missing_in_each_col) <- c("NAs", "Missing Text")
rownames(missing_in_each_col) <- names(imdbData)
missing_in_each_col

#Filter out the non-numerical data
imdbData_numeric <- imdbData %>% 
  select_if(is.numeric)
missingData_numeric <- which(is.na(imdbData_numeric) | imdbData_numeric == "", 
                             arr.ind = TRUE)

#Filter out the numerical data
imdbData_factor <- imdbData %>% 
  select_if(is.factor)
missingData_factor <- which(is.na(imdbData_factor) | imdbData_factor == "", 
                            arr.ind = TRUE)

# REMOVE VARIABLES -------------------------------------------------------------
#movie_title, plot_keywords, movie_imdb_link and aspect_ratio
remove_indices <- c(which(names(imdbData) == "movie_title"),
                    which(names(imdbData) == "plot_keywords"),
                    which(names(imdbData) == "movie_imdb_link"),
                    which(names(imdbData) == "aspect_ratio"))
imdbData <- imdbData[, -remove_indices]

# CHANGE CONTENT RATINGS -------------------------------------------------------

content_ratings_levels <- levels(imdbData$content_rating) #19 levels
