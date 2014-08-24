## The following are a pair of functions that cache the inverse of a matrix, so that it is not required to calculate it again and again
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
                b <- NULL                  ## set the initial value of inverse matrix to NULL
                set <- function(y){        ## set function allows you to set the value of the matrix
                    
                     x <<- y
                     b <<- NULL
                }
                get <- function() x       ## get function allows you to retrive the current value of the matrix
                setinverse <- function(inv) b <<- inv    ## this function caches the inverse matrix after it is calculated in the other function
                getinverse <- function() b               ## this function allows you to retrive the current value of inverse matrix
                list( set = set, get = get,              ## the special "matrix" object is created and returned(it is a list containing functions)
                setinverse = setinverse,
                getinverse = getinverse) 

}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
             b <- x$getinverse()          ## retrieve the current value of the inverse matrix
             data <- x$get()              ## retrieve the current value of the matrix
             if(!is.null(b)){             ## if inverse has not been calculated, calculate using solve and cache it
                 if(ncol(data)==nrow(b)) {      ## a way to check if the matrix has changed or not
                     d <- det(data %*% b)           ## even if previous condition is satisfied, matrix might have changed
                     if( d==1 ){                ## if codition is satisfied, then is it actually the inverse of matrix
                                    message("matrix has not changed, getting cached data") 
                                    return(b)   ## return cached inverse value
                     }
                 }
             }
             inv <- solve(data)   ## if above conditions are not satisfied, compute inverse
             x$setinverse(inv)    ## cache the inverse value
             inv                  ## display the inverse value


## Return a matrix that is the inverse of 'x'
}