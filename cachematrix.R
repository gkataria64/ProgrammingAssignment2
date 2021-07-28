## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Create the function that have as argument a matrix called x:
    makeCacheMatrix <- function(x = matrix()) {                 # Making a function makeCashMatrix
        inv <- NULL       # Assuming matrix is invertible
        set <- function(y) { #set the value of matrix  
            x <<- y          # double arrow preserves the state inside an R object
            inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse} # set the value of inverse
        getInverse <- function() {inv}                      # get the value of inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    }
    
    
    ## This function computes the inverse of the special "matrix" created by 
    ## makeCacheMatrix above. If the inverse has already been calculated (and the 
    ## matrix has not changed), then it should retrieve the inverse from the cache.
    
    cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()       # returns inverse of matrix in inv
        if (!is.null(inv)) {        # need to check inverse has already been calculated and check weather it is there in cache to skip the computation
            message("getting cached data")
            return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
    }