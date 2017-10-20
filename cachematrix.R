## Calculates the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
         ## Set function
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
          ## Get function
        get <- function() x
         ## SetInv function
        setinv <- function(inv) i <<- inv
         ## GetInv function
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve<- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## Assigning the inverse of a matrix using solve function and assigning it to inverse variable
        data <- x$get()
        i <-solve(data,...)
        x$setinv(i)
        i
}
