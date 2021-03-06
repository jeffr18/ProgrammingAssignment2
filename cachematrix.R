## These functions calculate the inverse of a matrix.  It uses a
## caching mechanism so that the matrix inverses do not need to be 
## recalculated if it has already been done.

## This function creates a list of functions to which allow for the 
## calculation of the inverse of a matrix.  The calculated inverse
## matrix is cached so that it can be obtained later without having
## to perform the inverse calculation again.
makeCacheMatrix <- function(x = matrix()) {
    invX <- NULL
    
    #create the matrix set function
    set <- function(y) {
        x <<- y         #save the original matrix in variable 'x'
        invX <<- NULL    #no inverse matrix calculated yet
    }
    
    #create the matrix get function
    get <- function() {
        x    #return the original matrix
    }
    
    #create the matrix setInverse function
    setInverse <- function(inverse) {
        invX <<- inverse    #save the inverse matrix in variable 'invX'
    }
    
    #create the matrix getInverse function
    getInverse <- function()  {
        invX     #return the inverse martrix
    }
    
    #return the list containing the four functions created above
    list(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)
}


## This function calculates the inverse of a matrix that is stored
## in an object created by the makeCacheMatrix() function.  If the
## inverse has already been calculated, it returns that value, otherwise,
## the inverse is calculated and stored in the cache so that it can be 
## accessed later.
cacheSolve <- function(x, ...) {
    #Retrieve the cached inverse matrix if it has been calculated
    invX <- x$getInverse()
    
    if (!is.null(invX)) {
        #The inverse has already been calculated so return the cached value
        #The inverse does not need to be re-calculated
        message("getting cached data")
        return(invX)
    }
  
    #If this point is reached, then no cached inverse value has been found
    #and so the inverse needs to be calculated and saved
    data <- x$get()  #get the original matrix value
    invX <- solve(data, ...)  #calculate the inverse matrix
    x$setInverse(invX)      #save the calculated inverse in the cache for 'x'
    
    invX    #Return a matrix that is the inverse of 'x'
}
