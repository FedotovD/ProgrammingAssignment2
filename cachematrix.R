## Functions below are used to cache the inverse of a matrix to decrece 
## computational costs

## makeCacheMatrix creates a list that contain a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

    inv_mat <- NULL
    set <- function(y) {
        x <<- y
        inv_mat <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv_mat <<- inverse
    getinverse <- function() inv_mat
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    
}


## This function returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
    inv_mat <- x$getinverse()
    if(!is.null(inv_mat)) {
        message("getting cached data.")
        return(inv_mat)
    }
    data <- x$get()
    inv_mat <- solve(data)
    x$setinverse(inv_mat)
    inv_mat
}
