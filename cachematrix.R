## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
##(there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
                b <- NULL
                set <- function(y){
                    
                     x <<- y
                     b <<- NULL
                }
                get <- function() x
                setinverse <- function(inv) b <<- inv
                getinverse <- function() b
                list( set = set, get = get, 
                setinverse = setinverse,
                getinverse = getinverse) 

}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
             b <- x$getinverse()
             data <- x$get()
             if(!is.null(b)){
                 if(ncol(data)==nrow(b)) {
                     d <- det(data*b)
                     if( d==1 ){
                                    message("matrix has not changed, getting cached data")
                                    return(b)
                     }
                 }
             }
             inv <- solve(data)   
             x$setinverse(inv)                  
             inv


## Return a matrix that is the inverse of 'x'
}