# Read data
movie <- read.csv(file="/Users/zhangliang/Desktop/IMDB-Data.csv", header=TRUE)

imdbData <- read.csv(file="/Users/zhangliang/Desktop/IMDB-Data.csv", header=TRUE)

# Get libraries needed
if (!require(naniar)){ 
  install.packages("naniar") 
} 

hist(num$gross)
require(naniar)
require(raster)
require(simputation)
require(dplyr)

# Search for hidden missing values (not coded as NA)
# Remove leading and trailing spaces
trim(imdbData)
### But how to remove spaces inside characters?(eg."South Korea")
### The reason I want to remove spaces is that when I search hidden missing values,
### the spaces in characters will be recognized as missings

hidden_miss1 <- imdbData %>%
  miss_scan_count(search = list("N/A", "N/a")) # No result
hidden_miss2 <- imdbData %>%
  miss_scan_count(search = list("missing"))# Check "plot_keywords", they are not missings
hidden_miss3 <- imdbData %>%
  miss_scan_count(search = " ") # Many missing values

# Replace them with NA 
replace_with_na_all(imdbData, ~.x == " ")

# Look the number and proportion of missing values in total
n_miss(imdbData) 
prop_miss(imdbData) 

# More details on each variable and observation
miss_each_variable <- miss_var_summary(imdbData)
miss_each_observation <- miss_case_summary(imdbData)

miss_var_table(imdbData)
miss_case_table(imdbData)


# Visualise missing values
# Get a bird's eye view of missing values to see if any pattern
vis_miss(imdbData, cluster = TRUE)

# Look at missing values in variables and observations
gg_miss_var(imdbData)
gg_miss_case(imdbData)
missing.values <- aggr(imdbData, sortVars = T, prop = T, sortCombs = T, cex.lab = 1.5, cex.axis = .6, cex.numbers = 5, combined = F, gap = -.2)
# See common combinations of missings
gg_miss_upset(imdbData)

# is available for us to just ignore the missing value or take another 
# measurement to deal with ?
movie.1 <- movie[!is.na(movie$gross), ]
movie.1 <- movie.1[!is.na(movie.1$budget), ]
dim(movie.1)

# we can calculate the proportion of gone value if we determine to just 
# ignore the rows which contains NA values belonging to columns of "gross"
# and "budget"
1-(dim(movie.1)[1]/dim(movie)[1])


### Might do some stuff about missings of one variable across another variable
### to see if there is significant difference? But the workload is too 
### heavy if do this to each pair. I am not sure which variables to choose

# the remaining observations total 3801
sum(complete.cases(movie.1))

# Continue to look though the columns which contain missing values.
colSums(sapply(movie.1,is.na))

# aspect_ratio has the highest number of missing values
table(movie.1$aspect_ratio)

mean (movie.1$aspect_ratio )
# new data sets are created to compare the results of 
# different assignments to missing values.
movie.2<-movie.1
movie.3<-mobvie.1

movie.2$aspect_ratio[is.na(movie.2$aspect_ratio)] <- 2.35
mean(movie.2$imdb_score[movie.2$aspect_ratio == 2.35])
# 6.498325
median(movie.21$imdb_score[movie.2$aspect_ratio == 2.35])
# 6.6
movie.3$aspect_ratio[is.na(movie.3$aspect_ratio)] <- 1.85
mean(movie.3$imdb_score[movie.3$aspect_ratio == 1.85]) 
# 6.371564
median(movie.3$imdb_score[movie.3$aspect_ratio == 1.85])
# 6.5

# By comparing the results, when we use the 2.35 ,
# the difference between the median and the mean is small, 
# indicating more stable.

###############################################
# This part is emulated from online sourses,
# would you mind us rewritting as "loop" method or "vectorization"
#############################################
# replace NA with column average for facenumber_in_poster
movie.1$facenumber_in_poster[is.na(movie.1$facenumber_in_poster)] <- round(mean(movie.1$facenumber_in_poster, na.rm = TRUE))
# convert 0s into NAs for other predictors
movie.1[,c(5,6,8,13,24,26)][movie.1[,c(5,6,8,13,24,26)] == 0] <- NA
# impute missing value with column mean
movie.1$num_critic_for_reviews[is.na(movie.1$num_critic_for_reviews)] <- round(mean(movie.1$num_critic_for_reviews, na.rm = TRUE))
movie.1$duration[is.na(movie.1$duration)] <- round(mean(movie.1$duration, na.rm = TRUE))
movie.1$director_facebook_likes[is.na(movie.1$director_facebook_likes)] <- round(mean(movie.1$director_facebook_likes, na.rm = TRUE))
movie.1$actor_3_facebook_likes[is.na(movie.1$actor_3_facebook_likes)] <- round(mean(movie.1$actor_3_facebook_likes, na.rm = TRUE))
movie.1$actor_1_facebook_likes[is.na(movie.1$actor_1_facebook_likes)] <- round(mean(movie.1$actor_1_facebook_likes, na.rm = TRUE))
movie.1$cast_total_facebook_likes[is.na(movie.1$cast_total_facebook_likes)] <- round(mean(movie.1$cast_total_facebook_likes, na.rm = TRUE))
movie.1$actor_2_facebook_likes[is.na(movie.1$actor_2_facebook_likes)] <- round(mean(movie.1$actor_2_facebook_likes, na.rm = TRUE))
movie.1$movie_facebook_likes[is.na(movie.1$movie_facebook_likes)] <- round(mean(movie.1$movie_facebook_likes, na.rm = TRUE))

# From the desciption of the data set ,we have following relationship with content_rating column:
# M = GP = PG, X = NC-17
movie.1$content_rating[movie.1$content_rating == 'M'& movie.1$content_rating == 'GP']   <- 'PG' 
movie.1$content_rating[movie.1$content_rating == 'X']   <- 'NC-17'


# Blanks should be taken as missing value. Since these missing values cannot be replaced with reasonable data, 
# we delete these rows.
movie.1 <- movie.1[!(movie.1$content_rating %in% ""),]
table(movie.1$facenumber_in_poster)


# Visualise using boxplot
imdbData %>%
  bind_shadow() %>%
  ggplot(aes(x = budget_NA, y = movie_facebook_likes)) +
  geom_boxplot() +
  scale_y_log10()

# Or might visualise missings acorss two variables.
# For example
imdbData %>%
  bind_shadow() %>%
  ggplot(aes(x = imdb_score, y = movie_facebook_likes, color = budget_NA)) +
  geom_point()
  

### Maybe we can do imputation using linear model with impute_lm function
### but we need to decide the linear model first
### For example: budget_imp_lm <- imdbData %>%
###                                 bind_shadow() %>%
###                                 add_label_shadow %>%
###                                 impute_lm(budget ~       )%>%
###                                 impute_lm(gross ~        )

### Track missings using ggplot

### Evaluate imputations and models










