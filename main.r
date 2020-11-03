

calculateDistance<-function(p1,p2) {
    # Get point 1 length
    p1Len <-length(p1)
    # Get point 2  length 
    p2Len <-length(p2)
    
    # Check if the points have the same number of elements
    if(p1Len != p2Len) stop("Points does not have the same length")
    # Initialize distance to 0 
    distance<-0
    # Loop through 1 to p1Len
    for(i in 1:p1Len){
        # 
        distance <- distance+(p1[i]-p2[i])^2
    }
    # Return the square root of the squared differences of the coordinates
    return(sqrt(distance))

}
calculateDistanceToCenters<-function(data,centers){
    # Number of rows
    n <- length(centers[,1])
    # Create a vector of size n
    distanceToCenters <- vector(mode="numeric",length=n)
    # Iterate from 1 to n
    for(i in 1:n){
        # Calculate Euclidean distance between 1 record(data) and one center at position i. 
        distanceToCenters[i] = calculateDistance(data,centers[i,])
    }
    # Return a vector with the distances 
    return(unlist(distanceToCenters))
}


intializeCenters <- function(k,columnsLen ) {
    # Create a matrix with K rows and with columnsLen columns
    centersMatrix <- matrix(nrow=k,ncol=columnsLen)
    # Iterate from 1 to K
    for(i in 1:k){
        # Random values from 0 to 1 and assign to centersMatrix at the index i.
        centersMatrix[i,] = runif(columnsLen,min=0,max=1)
    }
    # Return the matrix with k centers coordinates
    return(centersMatrix)
}


calculateCentroid<-function(data){
    # Get the columns length
    columnsLen <- length(data[1,])
    # Initiliaze vector of columnsLen size
    center <- vector(mode="numeric",length=columnsLen)
    # Loop through 1 to ColumnsLen
    for(i in 1:columnsLen){
        # Get the mean of the column at index i
        center[i]<-mean(data[,i])
    }
    # Return center
    return(center)

}
updateCenters <-function(X,labels){
    columnsLen <- length(X[1,])
    centers <- matrix(nrow=k,ncol=columnsLen)
    for(i in 1:k){
        # Get data point with label 
        data<- X[which(labels==i),]
        centers[i,]<-calculateCentroid(data)
    }
    return(centers)

}


saveFrame <- function(frame,data,centers) {
        pointsColors <- c("red","blue","green")
        jpeg(file=paste(imagesPath,frame,".jpeg"))
        plot(data)
        points(centers,type="p",pch=20,col=pointsColors,cex=4)
        dev.off()
}
kMeans <- function(X,centers,iterations) {
    # Vector to store distances 
    distances <- vector(mode="numeric",length = k)
    # Vector to store labels(1,2,3)
    labels <- vector(mode="numeric",length=numbersOfRows)
    # Temporal centers
    temporalCenters <-centers
    # Iterate from 1 to iterations
    for( z in 1:iterations){
        #  Iterate from 1 to number of rows
        for(i in 1:numbersOfRows){
            # Calculate distances to centers
            distancesToCenters = calculateDistanceToCenters(X[i,],temporalCenters)
            # Getting the minimum distance , using which to get the index and assign to labels at i position
            labels[i]=which( distancesToCenters == min(distancesToCenters) )
        }
        # Save the graph as image file
        saveFrame(z,X,temporalCenters)
        # Update centers
        tempCenters <- updateCenters(X,labels)
    }
}



###################################### 

data <- read.csv(file= 'data/01.csv',header = FALSE)
Xdata <- data[,1:2]
k<-3
maxIterations <- 10
numbersOfRows<-length(Xdata[,1])
numbersOfColumns<-length(Xdata[1,])
initialCenters <- intializeCenters(k,numbersOfColumns)
imagesPath="images/"
kMeans(Xdata,initialCenters,maxIterations)
# ##################
