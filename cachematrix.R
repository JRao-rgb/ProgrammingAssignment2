# This function takes a matrix as an input and returns a list, of which the 
# second element is the matrix itself. The first element of the list is 
# a way to set the value of the matrix and the third and fourth elements
# deal with setting and retrieving, respectively, the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

# This is a function that works in conjunction with the function makeCacheMatrix
# It computes the inverse of x, a matrix stored in a list of the type returned 
# in the makeCacheMatrix function. It does this by first checking to make sure that
# an inverse has not already been computed.
cacheSolve <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmean(m)
    m
}
