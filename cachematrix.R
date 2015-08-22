## The following functions create an object to store a matrix and caches its 
## inverse. 

## The function, makeCacheMatrix, creates a special "matrix" object that can 
## cache its inverse. It sets the values of the matrix, gets the value of the 
## matrix, sets the value of the inverse, and gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set<- function(y) {
            x<<-y
            i<<- NULL
      }
      get<- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function, cacheSolve, calculates the inverse of the matrix created 
## above. If the the inverse has already been calculated, it is simply returned.
## If not, it finds the inverse and and returns it.

cacheSolve <- function(x, ...) {
        i <-x$getinverse()
        if(!is.null(i)) {
              message("getting cached data")
              return(i)
        }
        data<- x$get()
        i<-solve(data,...)
        x$setinverse(i)
        i
}
