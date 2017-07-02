makeCacheMatrix <- function(x = matrix()) {
        
        # This function creates a spacial "matrix" object that can cache its inverse
        
        cacheMatrix <- NULL
        
        # Method
        
        setMatrix <- function(y) {
                x <<- y
                cacheMatrix <<- NULL
        }
        
        # Defining method
        
        getMatrix <- function() x
        
        # Defining 'setCache'
        
        setCache <- function(inverse) cacheMatrix <<- inverse
        
        # Returning inverse
        
        getCache <- function() cacheMatrix
        
        # Listing the names
        
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setCache = setCache,
             getCache = getCache)
        
        #---------------------------------------------------------
        
}


cacheSolve <- function(x, ...) {
        
        
        # This function computes the inverse of the special "matrix" returned by 
        # makeCacheMatrix above. If the inverse has already been calculated (and 
        # the matrix has not changed), then the cachesolve should retrieve the 
        # inverse from the cache
        
        # Retriving contents
        
        cacheMatrix <- x$getCache()
        
        # Returning result based on condition
        
        if (!is.null(cacheMatrix)) {
                message("loading cache matrix...")
                return(cacheMatrix)
        }
        
        # CRUD method for the cache
        else {
                dMatrix <- x$getMatrix()
                cacheMatrix <- solve(dMatrix, ...)
                x$setCache(cacheMatrix)
                return(cacheMatrix)
        }
        #---------------------------------------------------------
        
}
