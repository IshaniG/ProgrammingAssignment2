## makeCacheMatrix takes a square invertible matrix as its arguments and returns a list containing 4 functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL      # i is a scalar, which has a NULL value at this point
                       # i will hold the inverse of the square matrix x
        set <- function(y) {
                x <<- y  # '<<-' operator assigns a value to an object in an environment that is different from the current environment
                i <<- NULL  # set function sets the matrix that is to be inverted
        }
        get <- function() x  # gets the matrix
        setinv <- function(solve) i <<- solve # setinv function sets the inverse of the matrix
        getinv <- function() i  #getinv function gets the inverse of the matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv() # Before computing the matrix inversion, it checks for its value in the cache
        if(!is.null(i)) {
                message("getting cached data")# if the inverse is already available in the cache, returns a message "getting cached data"
                return(i) # returns the inverse of the matrix
        } 
        data <- x$get() # If the inverse is not in the cache, this step gets the matrix
        i <- solve(data, ...) # Computes the inverse using the Solve function in R
        x$setinv(i)
        i # returns the inverse of the matrix as its final value

}
