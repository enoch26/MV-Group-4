#################################################################
#ANAYSIS SCRIPT: USE THIS SCRIPT TO CARRYOUT MULTIVARIATE METHODS
#################################################################

# IMPORT DATASET
imdbData_Clean <- read.csv("Data/Clean IMDb Data.csv")

# LOAD LIBARARIES

# PRINCIPLE COMPONENT ANALYSIS -------------------------------------------------
# extract numeric attributes
num <- imdbData_Clean[sapply(imdbData_Clean, function(x) is.numeric(x))]
str(num)
# PCA
pmovie <- princomp(na.omit(num), scores=T, cor=T)
summary(pmovie)
screeplot(pmovie,type="l")