## Put comments here that give an overall description of what your
## functions does

## makeCacheMatrix creates a special matrix object. Then cacheSolve 
## calculates the inverse of the matrix. If the inverse matrix already existcs,
## cacheSolve will not calculate it one more time, but will find and return 
## already existing object. 
 
## Write a short comment describing this function
## makeCacheMatrix: creates a matrix object which is cached.

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inv_x <<-inverse
    getinverse <- function() inv_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve returns the inverse of a matrix A created with
## makeCacheMatrix function. If the cached inverse is available, 
## cacheSolve retrieves it. If not - it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getinverse()
    if (!is.null(inv_x)) {
        message("getting cached inverse matrix")
        return(inv_x)
    } else {
        inv_x <- solve(x$get())
        x$setinverse(inv_x)
        return(inv_x)
    }
}



