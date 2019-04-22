#################################################################
#ANAYSIS SCRIPT: USE THIS SCRIPT TO CARRYOUT MULTIVARIATE METHODS
#################################################################

# IMPORT DATASET ---------------------------------------------------------------
imdbData_Clean <- read.csv("Data/Clean IMDb Data.csv")
glimpse(imdbData_Clean)
# LOAD LIBARARIES --------------------------------------------------------------
library(dplyr)
library(corrplot)
library(qgraph)

# PRINCIPLE COMPONENT ANALYSIS -------------------------------------------------
# Extract numeric attributes
numData <- imdbData_Clean[sapply(imdbData_Clean, function(x) is.numeric(x))]
str(numData)

# PCA
pmovie <- princomp(na.omit(numData), scores = T, cor = T)
summary(pmovie)
# PC7 78.8% PC11 94.6%

# Kaiser's criterion
eigen(cov(na.omit(numData)))$values
mean(eigen(cov(na.omit(numData)))$values)
# suggest retain the first two components, seem too little

# Screeplot
screeplot(pmovie, type = "l", main = "Scree Plot of IMDb PCA Analysis")

# Scores and loadings
pscores <- pmovie$scores
pload <- pmovie$loadings

pload[, 1:8]
# cast_total_facebook_likes dominates in PC2
plot(pmovie$scores[,2],na.omit(numData)$cast_total_facebook_likes)

# Exploring Important Vairbales
## Equilibrium Contribution
eCriterion <- 1/sqrt(ncol(numData))
length(which(abs(pload[, 1]) > eCriterion)) #Suggests 7 variables

## Mardia's Criterion 
mCriterion <- 0.7 * max(pload[, 1])
length(which(abs(pload[, 1]) > mCriterion)) #Suggests 6 variables

# Visual Exploration of principal components 
par(pty = "s")
# plot 1:19 var after 19 error
for (i in (1:19)) {
  plot(pmovie$scores[, 1], pmovie$scores[, 2],
  ylim = range(pmovie$scores[, 1]),
  xlab = "PC1", ylab = "PC2", col = imdbData_Clean[, i], cex=0.5, lwd = 2, 
  main = paste(names(imdbData_Clean)[i]))
}

text(pmovie$scores[, 1], pmovie$scores[, 2],
     labels = imdbData_Clean[, 1],  # to select the factor var
     cex = 0.1, lwd = 2)

text(pmovie$scores[, 1], pmovie$scores[, 2],
     labels = abbreviate(imdbData_Clean[, ]),  # to select the factor var
     cex = 0.1, lwd = 2)

# Biplot
biplot(pmovie, xlabs=na.omit(imdbData_Clean)$content_rating, 
       choices = c(1,2))
# show two main directions except title year 

# Network graph
corrplot(cor(na.omit(numData)), method="ellipse")
qgraph(cor(na.omit(numData)))
# all positive correlated
