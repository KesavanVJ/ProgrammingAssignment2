## Matrix inversion can be a very costly operation and these functions listed
## below enable to create a matrix whose inverse can be cached and retrieved 
## quickly. 

## Make CacheMatrix - Set, Retreive Matrix and it's (cached) Inverse
makeCacheMatrix <- function(cMatrix = matrix()) {
    
    # Initialize Inverse of Matrix (to be cached)
    InvcMatrix <- NULL
    
    # set Matrix function
    setMatrix <- function(newMatrix) {  
            # Set or Update cMatrix 
            cMatrix <<- newMatrix
            # Reset Inverse Matrix to Null
            InvcMatrix <<- NULL   
    }
    
    # get Matrix function
    getMatrix <- function() cMatrix
    
    # set Matrix Inverse
    setInverse <- function(newinvMatrix) InvcMatrix <<- newinvMatrix
    
    # get Matrix Inverse
    getInverse <- function() InvcMatrix
    
    # Returns the list of setMatrix, getMatrix,
    # setInverse & getInverse functions
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse) 
}

## Return the inverse of matrix created using makeCacheMatrix from Cache
## if not available, the inverse is calculated using solve() 
cacheSolve <- function(cacheMatrix, ...) {
    
    # Get Cached Inverse Matrix
    invMatrix <- cacheMatrix$getInverse()
    
    # if Inverse of Matrix cached - invMatrix is not null
    if(!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    } else { # Cache empty - calculate Inverse of Matrix
        message("calculating Inverse of Matrix")
        cMatrix <- cacheMatrix$getMatrix()
        invMatrix <- solve(cMatrix)
        cacheMatrix$setInverse(invMatrix)
        return(invMatrix)
    }
}
