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

#### ACTUAL SOLUTION STARTS HERE ####
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

# Two ways to use cache solve - #1, a little less obvious
w <- makeMatrix()

w$set(testmatrix)

cacheSolve(w)

# #2, probably the better way.

h <- makeMatrix(testmatrix)

cacheSolve(h)

# try it again - we should get our cached retrieval message.

cacheSolve(h)

#### ACTUAL SOLUTION ENDS HERE ####

# Here's a test case based on the test matrices provided by Alan Berger in the
# Coursera forum at the link below:

# https://www.coursera.org/learn/r-programming/discussions/forums/_ZerTCj2EeaZ8Apto8QB_w/threads/ePlO1eMdEeahzg7_4P4Vvg

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

m1

testing <- h <- makeMatrix(m1)
# First run:
cacheSolve(testing)
# Second run to see if we get our cached retrieval message:
cacheSolve(testing)

# Let's just multiply the matrices to see if we really do get the identity matrix:
n1 <- cacheSolve(testing)

print(m1 %*% n1)

# Yep!

# One more test to see if our setters are working properly.

n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)

testing$set(n2)

# Now if we run cacheSolve on testing again, we should get the inversion of matrix
# n2, not matrix n1. All without having to run the function makeMatrix again!
cacheSolve(testing)
m2 <- cacheSolve(testing)
# Is this really the inversion of matrix n2? Let's find out.

n2 %*% m2
#Yep! Though R does give us 1 and 0 written in weird scientific notation due
# to some rounding errors somewhere! (At least it did on my machine!)


# This stuff below is just for reference, you don't need this.
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
