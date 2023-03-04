## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}


#This is just a test comment to test github commit

## I'm preserving the original above; let's get started below.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function() x


}

# We are first going to define a null Matrix that
# We can input data into, as well as four subfunctions we can
# access using the $ subsetting operator.

makeMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y # Critically, these are superoperators, assigning
            m <<- NULL # these variables in the GLOBAL environment
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve # Also a superoperator.
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      m <- x$getsolve() # We're asking cacheSolve to see if the matrix
      # has already been solved for.

      if (!is.null(m)) {
            message("getting cached matrix inversion...")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      return(m)
      ## Returns a matrix that is the inverse of 'x'
}

testmatrix <- matrix(1:4,2,2)

testmatrix

h <- makeMatrix(testmatrix)

cacheSolve(h)


cachemean <- function(x, ...) {
      m <- x$getmean()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- mean(data, ...)
      x$setmean(m)
      m
}
