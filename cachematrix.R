## The following functions calculate the inverse of a matrix and saves it
## to the cache so that when next time the user tries to calculate the
## inverse, the previously saved value is returned instead of repeating
## the computation.

## The first function here creates a special "matrix" object, which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The second function here calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the matrix of the
## data and sets the value of the inverse in the cache via the setinverse
## function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inverse(data,...)
        x$setinverse(m)
        m
}