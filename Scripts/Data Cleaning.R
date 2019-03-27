# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(dplyr)

# IMPORT DATA -------------------------------------------------------------
imdbData <- read.csv("Data/IMDB Data.csv", header = TRUE)

# HAVE A LOOK -------------------------------------------------------------
glimpse(imdbData)

#Change title_year and aspect_ratio to factor
imdbData[c(24, 27)] <- lapply(imdbData[c(24,27)], factor) 

#Missing values per column
vis_miss(imdbData, cluster = TRUE)
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
remove_variables <- c(which(names(imdbData) == "movie_title"),
                    which(names(imdbData) == "plot_keywords"),
                    which(names(imdbData) == "movie_imdb_link"),
                    which(names(imdbData) == "aspect_ratio"))
imdbData <- imdbData[, -remove_variables]
vis_miss(imdbData, cluster = TRUE)

<<<<<<< HEAD:Data Cleaning.R
content_ratings_levels <- levels(imdbData$content_rating) #19 levels

# FIX MISSING 
||||||| merged common ancestors:Data Cleaning.R
content_ratings_levels <- levels(imdbData$content_rating) #19 levels

# FIX MISSING ------------------------------------------------------------------
=======
# FIX MISSING VALUES------------------------------------------------------------
>>>>>>> 6e6321e5817f068313782b276c79dffee8a65041:Scripts/Data Cleaning.R
## Color: All appear to be color movies
## Content: Will group with "Not Rated"
## Country: Missing values appear to be American Movies
imdbData$color[imdbData$color == ""] <- "Color"
imdbData$content_rating[imdbData$content_rating == ""] <- "Not Rated"
imdbData$country[imdbData$country == ""] <- "USA"

#Remove observations for where actor name and actor likes are missing
rm_observations <- which(imdbData$actor_1_name == "" |
                           is.na(imdbData$actor_1_facebook_likes) |
                           imdbData$actor_2_name == "" |
                           is.na(imdbData$actor_2_facebook_likes) |
                           imdbData$actor_3_name == "" |
                           is.na(imdbData$actor_3_facebook_likes))
imdbData <- imdbData[-rm_observations, ]

<<<<<<< HEAD:Data Cleaning.R
||||||| merged common ancestors:Data Cleaning.R
#Find where there is more than 1 missing value per observation
missing_in_each_row <- rep(NA, nrow(imdbData))
for(i in 1:nrow(imdbData)){
  missing_in_each_row[i] <- sum(is.na(imdbData[i, ]) | (imdbData[i, ] == ""))
}

#If more than 1 missing value
length(which(missing_in_each_row > 1))/nrow(imdbData)

#If more than 2 missing values
length(which(missing_in_each_row > 2))/nrow(imdbData)

#Try remove 2 missing value first
rm_observations <- which(missing_in_each_row > 2)
imdbData <- imdbData[-rm_observations, ]
=======
#Find where there is more than 1 missing value per observation
missing_in_each_row <- rep(NA, nrow(imdbData))
for(i in 1:nrow(imdbData)){
  missing_in_each_row[i] <- sum(is.na(imdbData[i, ]) | (imdbData[i, ] == ""))
}

#If more than 1 missing value we lose 5.44% of the current dataset
length(which(missing_in_each_row > 1))/nrow(imdbData)

#If more than 2 missing values  we lose 2.57% of the current dataset
length(which(missing_in_each_row > 2))/nrow(imdbData)

#Try remove 2 missing value first
rm_observations <- which(missing_in_each_row > 2)
imdbData <- imdbData[-rm_observations, ]
>>>>>>> 6e6321e5817f068313782b276c79dffee8a65041:Scripts/Data Cleaning.R

vis_miss(imdbData, cluster = TRUE)

#Write "Cleanish Data Set" to new file
write.csv(imdbData, file = "Data/Clean IMDb Data.csv", row.names = FALSE)


