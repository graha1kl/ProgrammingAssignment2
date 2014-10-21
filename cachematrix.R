## Pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Create variable to hold cached inverse
        inv <- NULL
        
        ## Set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Get the value of the matrix
        get <- function() x
        
        ## Set the value of the the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        
        ## Get the value of the inverse of the matrix
        getinverse <- function() inv
        
        ## Return a list of the functions just defined
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Check if the inverse has been cached
        inv <- x$getinverse()
        
        ## If inverse is cached, return cached value
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## If not, retrieve the matrix and calculate the inverse
        data <- x$get()
        inv <- solve(data)
        
        ## Store the inverse in the cache variable
        x$setinverse(inv)
        
        ## Return a matrix that is the inverse of 'x'
        inv
}
