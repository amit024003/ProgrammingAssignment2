## Caching the Inverse of a Matrix:
## Inverse of a matrix is a costly operation. It would be better to cache the 
## matrix inverse for later purpose instead of recomputing it. Whenever first 
## time matrix inversion called for a given matrix, it computes the inversion 
## and stores it in cache for later purpose. If again matrix inversion called 
## on already computed inversion matrix (matrix is not changed), it gives the 
## result from cache instead of recomputing it.

## Example:
## originalMatrix <- matrix(c(4, 2, 7, 6), 2, 2)
## cacheableMatrx <- makeCacheMatrix(originalMatrix)
## ## First time inverse computation
## cacheSolve(cacheableMatrx)
##       [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## Second time inverse computation
## cacheSolve(cacheableMatrx)
## getting the cache inverse.
##       [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4


## Create special matrix that has functionality to store its inversion and 
## contains the following list of function:
## setMatrix - Set the matrix
## getmatrix - Get the stored matrix
## setInverse - Set the inverse of stored matrix
## getInverse - Get the cached inverse of stored matrix

makeCacheMatrix <- function(x = matrix()) {
    # Initiate the cacheInverse with NULL as its inversion is not calculated.
    cacheInverse <- NULL
    
    # setMatrix function to re-assign the matrix and initilize the cacheInverse
    # to NULL as its stored matrix value has changed.
    setMatrix <- function(newMatrix) {
        x <<- newMatrix
        cacheInverse <- NULL
    }
    
    # getMatrix function returns the stored matrix
    getMatrix <- function() {
        x
    }
    
    # setInverse function stores the computed inverse for this matrix 'x'
    setInverse <- function(inverse) {
        cacheInverse <<- inverse
    }
    
    # getInvese returns the cached inverse of this matrix 'x'
    getInverse <- function() {
        cacheInverse
    }
    
    # returns the list of supported functions
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse, getInverse = getInverse)
}


## This function solve for the matrix inversion for matrix which generated
## by the makeCacheMatrix. It compute the matrix inversion if and only if
## cache doesn't have it otherwise return the cached inversion.


cacheSolve <- function(x, ...) {
    
    # try to retrive the cache inverse
    cacheInverse <- x$getInverse()
    
    # Check whether cache inverse is not NULL and return the cached inverse.
    if(!is.null(cacheInverse)) {
        message("getting the cache inverse.")
        return(cacheInverse)
    }
    
    # As cache inverse is NULL, compute the inverse ans store in cache.
    matrix <- x$getMatrix()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    
    ## Return a matrix that is the inverse of 'x'
    inverse
}
