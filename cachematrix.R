
## Maxtrix inversion is a time consuming computation, especially in a loop.
## These functions are made to compute and cache the inverse process outside the loop, 
## so that R can look them up later inside the loop instead of computing everytime in the loop.
## These functions would save computing time and prevent breakdowns.


## makeCacheMatrix function can pre-compute and cache the inverse matrix for later use.
## makeCacheMatrix is somehow like a multiplication table we can look up later.



makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL
        
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        
}



## cacheSolve looks up the inverse result of the “matrix” returned by makeCacheMatrix(). 
## If the inverse has already been calculated, it’ll retrieves the inverse from the cache directly.
## Otherwise, cacheSolve would tell makeCacheSolve to inverse the matrix, and cache its new answer for later use.



cacheSolve <- function(x, ...){
        ## x here is the output of makeCacheMatrix.
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                ## if makeCacheMatrix has already computed the inverse matrix.
                message("getting cache data")
                return(inverse)
                
        }
        ## Else, tells makeCacheMatrix to inverse the matrix right now.        
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        ## Sets the value of inverse.
        return(inverse)
        ## Return a matrix that is the inverse of 'x'
        
}
