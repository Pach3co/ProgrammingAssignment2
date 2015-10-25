## makeCacheMatrix do:
## set the matrix
## get the matrix
## set the inverse matrix
## get the inverse matrix
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # defines a function to set the matrix x to a new matrix y
        # and resets the inverse to NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x # returns the matrix x
        setinv <- function(inverse) inv <<- inverse # sets the inv to inverse 
        getinv <- function() inv # returns the inverse inv
        # returns the matrix containing all of the functions just defined
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve calculates the inverse of a matrix.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        # if the inverse has already been calculated
        if(!is.null(inv)) {
                # get it from the cache and skips the computation
                message("getting cached data")
                return(inv)
        }
        # otherwise calculates the inverse
        m.data <- x$get()
        inv <- solve(m.data, ...)
        # sets the value of the inverse in the cache via the setinv function
        x$setinv(inv)
        inv
}

