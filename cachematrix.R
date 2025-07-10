## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## starts here by Wish
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It returns a list of functions to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    set <- function(y) {
        x <<- y         # Assign new matrix
        inv <<- NULL    # Clear previously cached inverse
    }
    get <- function() x  # Return the current matrix
    setinverse <- function(inverse) inv <<- inverse  # Cache the inverse
    getinverse <- function() inv                     # Return the cached inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and cached, it retrieves it from the cache
## instead of recomputing it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)  # Return cached inverse
    }
    mat <- x$get()          # Retrieve the original matrix
    inv <- solve(mat, ...)  # Compute the inverse
    x$setinverse(inv)       # Cache the inverse
    inv
}
