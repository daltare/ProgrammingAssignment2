## These functions are passed a matrix (x) as input, then calculate the inverse 
## of the matrix and cache the matrix inverse so it can be retrieved if needed 
## for re-use. The function makeCacheMatrix stores the matrix and can cache 
## its inverse. The cacheSolve function checks whether the matrix 
## inverse has been calcualted and is cached in the function makeCacheMatrix; 
## if the matrix inverse is cached then it is retrieved from the makeCacheMatrix 
## function, otherwise it is computed and passed back to the makeCacheMatrix
## function to be cached.


## This function is passed a matrix (x) as input, and returns a list containing 
## multiple functions. The functions store the matrix and can cache its inverse
## (i) if the inverse has been calculated. 
makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL  # The variable i stores the matrix inverse if computed
        
        # Stores the matrix passed to the set function, and makes it available 
        # to all functions within the makeCacheMatrix function
        set <- function(y) {    
                x <<- y         # Stores the matrix 
                i <<- NULL      # Re-sets the matrix inverse when matrix changes
        }
        
        # Returns the matrix
        get <- function() x     
        
        # Caches the inverse in the variable i, and makes it available to all 
        # functions within the makeCacheMatrix function
        setinverse <- function(inverse) i <<- inverse
        
        # Returns the value of the matrix inverse
        getinverse <- function() i
        
        # Create a list of the functions defined above. The list is returned to
        # the object assigned to makeCacheMatrix
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## This function computes the inverse of the matrix stored by makeCacheMatrix, 
## if the inverse has not already been computed. If the inverse has already been 
## computed (and the matrix has not changed), then the cacheSolve function 
## retrieves the inverse cached in makeCacheMatrix.

cacheSolve <- function(x, ...) {  ## Returns a matrix that is the inverse of 'x'
        
        i <- x$getinverse()     # Get the value of the inverse from makeCacheMatrix
        
        if(!is.null(i)) {       # Check if the inverse is cached. If so...
                message("getting cached data")  
                return(i)       # return the cached value
        }
        
        # If the inverse is not cached (i.e. has not been calculated), then...  
        data <- x$get()         # retrieve the matrix (x) from makeCacheMatrix
        i <- solve(data, ...)   # compute the inverse of the matrix x
        x$setinverse(i)         # pass the inverse back to MakeCacheMatrix
        i                       # return i
}
