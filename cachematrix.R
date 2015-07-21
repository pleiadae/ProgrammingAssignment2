## Function 'makeCacheMatrix' creates an object that caches its inverse.
## Function 'cacheSolve' computes the inverse of the matrix returned 
## by makeCacheMatrix. 

## Function 'makeCacheMatrix' has a matrix as an argument. It consists of 
## a list of four functions. It first sets its inverse matrix as NULL. 
## First function called 'set' sets the value of the matrix.
## Second function called 'get' gets the value of the matrix.
## Third function called 'setinverse' sets the value of the inverse matrix.
## Fourth function called 'getinverse' gets the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function 'cacheSolve' calculates inverse matrix of the matrix created by
## the function 'makeCacheMatrix'. It also takes a matrix as an argument,
## and ... passes further arguments. It first checks if the inverse matrix is
## already calculated. If yes, it gets its value from the cache and skips
## calculation (the whole point of all this). If not, it calculates the
## inverse matrix of the set matrix. Then it sets the value of the inverse
## matrix in the cache using the previously defined 'setinverse' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## Applying and checking how it works on an example:

m <- matrix(c(3,-5,-4.5,0,2,2,5,7,8),nrow = 3,ncol = 3) ## Creates matrix m
print(m) ## Prints matrix m
mymatrix <- makeCacheMatrix(m) ## Caches matrix m
mymatrix$get() ## Gets matrix m

## The following should give NULL as the inverse wasn't 
## calculated yet, this is the task of the 
## 'cacheSolve' function which we haven't called yet
mymatrix$getinverse() 

## The following computes the inverse of m, it returns cached inverse
cacheSolve(mymatrix) 

class(mymatrix$get()) ## Class of returned object - matrix
class(mymatrix$getinverse()) ## Class of returned inverse object - matrix
