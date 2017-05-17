##Matrix inversion is usually a costly computation and there may be some
##benefit to caching the inverse of a matrix rather than computing it
##repeatedly.  The pair of functions below cache the inverse of a matrix.

## The first function, `makeCacheMatrix` creates a special "vector", which is
##really a list containing a function to

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse of the matrix
##4.  get the value of the invesre of the matrix

makeCacheMatrix <- function(x = matrix()) {
      msolve <- NULL
      set <- function (y) {
            x <<- y
            msolve <<- NULL
      }
      get <- function () x
      setsolve <- function(solve) msolve<<- solve
      getsolve <- function() msolve
      list (set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

##Next I define an object of type makeCacheMatrix to feed into the cacheSolve function:
myMatrix<-makeCacheMatrix(matrix(c(-1,-2,1,1),2,2))

## The following function calculates the inverse of myMatrix.  However, it 
##first checks to see if the inverse has already been calculated. 
##If so, it `get`s the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value of the 
##inverse in the cache via the `setsolve` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      msolve <- x$getsolve()
      if(!is.null(msolve)) {
            message("getting cached data")
            return(msolve)
      }
      data <- x$get()
      msolve <- solve(data,...)
      x$setsolve(msolve)
      msolve
}

##Now I call the cacheSolve function on 'myMatrix'
cacheSolve(myMatrix)

##I run it again to confirm that the program skipped the computation this time
##and returned the cached value of the matrix
cacheSolve(myMatrix)
