imdbData_Clean <- read.csv("Clean IMDb Data.csv")
gross_cat <- rep(NA, nrow(imdbData_Clean))
imputed_data <- cbind(imputed_data,gross_cat)
low <- 0
med <- quantile(imputed_data$gross, 0.33)
high <- quantile(imputed_data$gross, 0.66)
for(i in 1:nrow(imdbData_Clean)){
  if(is.na(imputed_data$gross[i])){
    imputed_data$gross_cat[i] = 'black'
  }
  else if(imputed_data$gross[i] >= low & imputed_data$gross[i] < med){
    imputed_data$gross_cat[i] = 'green'
  }
  else if(imputed_data$gross[i] >= med & imputed_data$gross[i] < high){
    imputed_data$gross_cat[i] = 'blue'
  }
  else if(imputed_data$gross[i] >= high){
    imputed_data$gross_cat[i] = 'red'
  }
}

plot(pmovie$scores[, 1], pmovie$scores[, 2],
     ylim = range(-10,8),
     xlab = "PC1", ylab = "PC2", cex=0.6, lwd = 2, type = 'p',
     col = imputed_data$gross_cat, pch = 16
)



#same again only for critics reviews 
critics_review_cat <- rep(NA, nrow(imdbData_Clean))
imputed_data <- cbind(imputed_data,critics_review_cat)
low <- 0
med <- quantile(imputed_data$num_critic_for_reviews, 0.33)
high <- quantile(imputed_data$num_critic_for_reviews, 0.66)
for(i in 1:nrow(imdbData_Clean)){
  if(is.na(imputed_data$num_critic_for_reviews[i])){
    imputed_data$critics_review_cat[i] = 'black'
  }
  else if(imputed_data$num_critic_for_reviews[i] >= low & imputed_data$num_critic_for_reviews[i] < med){
    imputed_data$critics_review_cat[i] = 'green'
  }
  else if(imputed_data$num_critic_for_reviews[i] >= med & imputed_data$num_critic_for_reviews[i] < high){
    imputed_data$critics_review_cat[i] = 'blue'
  }
  else if(imputed_data$num_critic_for_reviews[i] >= high){
    imputed_data$critics_review_cat[i] = 'red'
  }
}

plot(pmovie$scores[, 1], pmovie$scores[, 2],
     ylim = range(-10,5),
     xlim = range(-5,6),
     xlab = "PC1", ylab = "PC2", cex=0.8, lwd = 2, type = 'p',
     col = imputed_data$critics_review_cat, pch = 16
)


movie_fb_cat <- rep(NA, nrow(imdbData_Clean))
imputed_data <- cbind(imputed_data, movie_fb_cat)
for(i in 1:nrow(imdbData_Clean)){
  if(imputed_data$movie_facebook_likes[i] == 0){
    imputed_data$movie_fb_cat[i] = 'black'
  }
  else if(imputed_data$movie_facebook_likes[i] != 0){
    imputed_data$movie_fb_cat[i] = 'red'
  }
}

plot(pmovie$scores[, 1], pmovie$scores[, 2],
     ylim = range(-10,5),
     xlim = range(-5,6),
     xlab = "PC1", ylab = "PC2", cex=0.8, lwd = 2, type = 'p',
     col = imputed_data$movie_fb_cat, pch = 16
)



num_users_cat <- rep(NA, nrow(imdbData_Clean))
imputed_data <- cbind(imputed_data,num_users_cat)
low <- 0
med <- quantile(imputed_data$num_user_for_reviews, 0.33)
high <- quantile(imputed_data$num_user_for_reviews, 0.66)
for(i in 1:nrow(imdbData_Clean)){
  if(is.na(imputed_data$num_user_for_reviews[i])){
    imputed_data$num_users_cat[i] = 'black'
  }
  else if(imputed_data$num_user_for_reviews[i] >= low & imputed_data$num_user_for_reviews[i] < med){
    imputed_data$num_users_cat[i] = 'green'
  }
  else if(imputed_data$num_user_for_reviews[i] >= med & imputed_data$num_user_for_reviews[i] < high){
    imputed_data$num_users_cat[i] = 'blue'
  }
  else if(imputed_data$num_user_for_reviews[i] >= high){
    imputed_data$num_users_cat[i] = 'red'
  }
}

plot(pmovie$scores[, 1], pmovie$scores[, 2],
     ylim = range(-10,5),
     xlim = range(-5,6),
     xlab = "PC1", ylab = "PC2", cex=0.6, lwd = 2, type = 'p',
     col = imputed_data$num_users_cat, pch = 16
)


num_voters_cat <- rep(NA, nrow(imdbData_Clean))
imputed_data <- cbind(imputed_data,num_voters_cat)
low <- 0
med <- quantile(imputed_data$num_voted_users, 0.33)
high <- quantile(imputed_data$num_voted_users, 0.66)
for(i in 1:nrow(imdbData_Clean)){
  if(is.na(imputed_data$num_voted_users[i])){
    imputed_data$num_voters_cat[i] = 'black'
  }
  else if(imputed_data$num_voted_users[i] >= low & imputed_data$num_voted_users[i] < med){
    imputed_data$num_voters_cat[i] = 'green'
  }
  else if(imputed_data$num_voted_users[i] >= med & imputed_data$num_voted_users[i] < high){
    imputed_data$num_voters_cat[i] = 'blue'
  }
  else if(imputed_data$num_voted_users[i] >= high){
    imputed_data$num_voters_cat[i] = 'red'
  }
}

