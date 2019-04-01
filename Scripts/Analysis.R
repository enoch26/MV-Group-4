#################################################################
#ANAYSIS SCRIPT: USE THIS SCRIPT TO CARRYOUT MULTIVARIATE METHODS
#################################################################

# IMPORT DATASET ---------------------------------------------------------------
imdbData_Clean <- read.csv("Data/Clean IMDb Data.csv")

# LOAD LIBARARIES --------------------------------------------------------------

# PRINCIPLE COMPONENT ANALYSIS -------------------------------------------------
# Extract numeric attributes
numData <- imdbData_Clean[sapply(imdbData_Clean, function(x) is.numeric(x))]
str(numData)

# PCA
pmovie <- princomp(na.omit(numData), scores = T, cor = T)
summary(pmovie)

# Screeplot
screeplot(pmovie, type = "l", main = "Scree Plot of IMDb PCA Analysis")

# Scores and loadings
pscores <- pmovie$scores
pload <- pmovie$loadings
pload[, 1:8]
# cast_total_facebook_likes in PC2 noteworthy

# Exploring Important Vairbales
## Equilibrium Contribution
eCriterion <- 1/sqrt(ncol(numData))
length(which(abs(pload[, 1]) > eCriterion)) #Suggests 7 variables

## Mardia's Criterion 
mCriterion <- 0.7 * max(pload[, 1])
length(which(abs(pload[, 1]) > mCriterion)) #Suggests 6 variables

# Visual Exploration of principal components 
par(pty = "s")
plot(pmovie$scores[, 1], pmovie$scores[, 2],
     ylim = range(pmovie$scores[, 1]),
     xlab = "PC1", ylab = "PC2", type = "n", lwd = 2)
text(pmovie$scores[, 1], pmovie$scores[, 2],
     labels = abbreviate(imdbData_Clean[, 20]),  # to select the factor var
     cex = 0.1, lwd = 2)

