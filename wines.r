library("Hmisc")
normalize <- function(x) {
    return((x-min(x))/(max(x)-min(x)) )
}



# data <- read.csv(file= 'data/wines.csv')

# X <- data.frame(data[,1:13])
# df <- sapply(X,normalize)
# y <- data[,14]
# correlationMatrix <- rcorr(as.matrix(df))
# print(correlationMatrix)

# print(X_normalized$)

# hist(X_normalized$Alcohol)
# print(X_normalized)
# print(summary(X_normalized))