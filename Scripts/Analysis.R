#################################################################
#ANAYSIS SCRIPT: USE THIS SCRIPT TO CARRYOUT MULTIVARIATE METHODS
#################################################################

# IMPORT DATASET
imdbData_Clean <- read.csv("Data/Clean IMDb Data.csv")

# LOAD LIBARARIES

<<<<<<< HEAD
# PRINCIPLE COMPONENT ANALYSIS -------------------------------------------------
# extract numeric attributes
num <- imdbData_Clean[sapply(imdbData_Clean, function(x) is.numeric(x))]
str(num)

# PCA
pmovie <- princomp(na.omit(num), scores=T, cor=T)
summary(pmovie)

# screeplot
screeplot(pmovie,type="l")

# scores and loadings
pscores <- pmovie$scores
pload <- pmovie$loadings
pload[,1:8]
# cast_total_facebook_likes in PC2 noteworthy

par(pty="s")
plot(pmovie$scores[,1],pmovie$scores[,2],
     ylim=range(pmovie$scores[,1]),
     xlab="PC1",ylab="PC2",type="n",lwd=2)
text(pmovie$scores[,1],pmovie$scores[,2],
     labels=abbreviate(imdbData_Clean[,20]),  # to select the factor var
     cex=0.1,lwd=2)
names(imdbData_Clean[,3])
||||||| merged common ancestors
# PRINCIPLE COMPONENT ANALYSIS -------------------------------------------------
=======
# PRINCIPLE COMPONENT ANALYSIS -------------------------------------------------
# extract numeric attributes
num <- imdbData_Clean[sapply(imdbData_Clean, function(x) is.numeric(x))]
str(num)
# PCA
pmovie <- princomp(na.omit(num), scores=T, cor=T)
summary(pmovie)
screeplot(pmovie,type="l")
>>>>>>> 8776333e3a5e0c7f5c62a565ec698bf1f4518afa
