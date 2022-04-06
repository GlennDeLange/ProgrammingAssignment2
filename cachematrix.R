## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to
#set the value of the Matrix
#get the value of the Matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function
#The following function calculates the mean of the special "Matrix" created with
#the above function. However, it first checks to see if the inverse has already 
#been calculated. If so, it gets the inverse from the cache and skips the 
#computation. Otherwise, it calculates the inverse of the data and sets the value 
#of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting inversed matrix")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
