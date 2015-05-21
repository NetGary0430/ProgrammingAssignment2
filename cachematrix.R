## Take a matrix as an argument and find its inverse.


## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x  ##returns the vector, x
    setmean <- function(mean) m <<- mean  ##sets the mean, m, to mean
    getmean <- function() m  ##returns the mean, m
    ## returns the a vector containing all of the functions defined above
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
    ## }
}
