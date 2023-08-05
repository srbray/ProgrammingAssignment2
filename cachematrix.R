## These functions take a matrix object and returns the inverse of that matrix. 
## More importantly, we cache that inverse matrix so that we do not need to recalculate
## the inverse matrix each time we need it. If no new matrix has been closed for 
##makeCacheMatrix, cacheSolve will return the cached inverse matrix.

## The function makeCacheMatrix creates a list of functions that 1) set the value
## of the matrix, 2) get the value of the matrix, 3) set the value of the inverse matrix
## 4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                 #create empty i which will hold inverse
        set <- function(y) {
                x <<- y           #create in parent environment makeCacheMatrix
                i <<- NULL        #same as above
        }
        get <- function() x       #create get function that will get the matrix
        setinverse <- function(inverse) i <<- inverse #setting i to the inverse in parent environment
        getinverse<- function() i   #retrieving inverse
        list(set = set, get = get,    #creating output list that can be called by cacheSolve 
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve calculates the inverse of the matrix (list) created in
## makeCacheMatrix. It checks to see if that inverse has already been calculated;
## if so, it provides that inverse with the statement "getting cached data." If no
## inverse is stored, it calculate the inverse with solve and caches it via the 
## setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()    #calling inverse from previous to see if already cached
        if(!is.null(i)) {      #if cached, return inverse with message
                message("getting cached data")
                return(i)
        }
        data<-x$get()           #get the matrix to
        i<-solve(data, ...)     #solve for the inverse
        x$setinverse(i)         #set it with setinverse
        i                       #return the inverse
}
