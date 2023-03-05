# We are first going to define a null matrix that
# we can input data into, as well as four subfunctions we can
# access using the $ subsetting operator on the object created by makeMatrix.

makeMatrix <- function(x = matrix()) { #This line formally defines x, so that we
                                    # don't need to actually run $setsolve necessarily.
      m <- NULL
      set <- function(y) {
            x <<- y # Critically, these are superoperators, assigning
            m <<- NULL # these variables in the GLOBAL environment.
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve # Also a superoperator. Note the variable
      getsolve <- function() m                # solve is only created by cacheSolve below.
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve) # Name our subfunctions so we can access them with $ rather than [[]].
}

## Now that we have the object created by makeMatrix and the functions created by
# makeMatrix, accessible with $, we need to actually solve for a matrix using cacheSolve.
# Note cacheSolve will ONLY accept a matrix object created by makeMatrix.

cacheSolve <- function(x, ...) {
      m <- x$getsolve() # We're asking cacheSolve to see if the matrix
      # has already been solved for.

      if (!is.null(m)) {
            message("getting cached matrix inversion...") # If the matrix was already solved for,
            return(m)                                     # we don't have to solve it again.
      }
      data <- x$get() # If the matrix was NOT solved for, use $get() to get the data from makeMatrix object.
      m <- solve(data, ...)
      x$setsolve(m) # This sets the solution to our matrix in the global environment.
      return(m)
      ## Returns a matrix that is the inverse of 'x'
}

# Now that we created our functions, let's test them!

# Here's a test case based on the test matrices provided by Alan Berger in the
# Coursera forum at the link below:

# https://www.coursera.org/learn/r-programming/discussions/forums/_ZerTCj2EeaZ8Apto8QB_w/threads/ePlO1eMdEeahzg7_4P4Vvg

# First create our matrix.
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

m1

# Now create a makeMatrix object using matrix m1, and give it a descriptive name.
test_makeMatrix_object_1 <- makeMatrix(m1)

# Now use cacheSolve() on the named makeMatrix object for m1 to see if it solves for m1.
cacheSolve(test_makeMatrix_object_1)
# Now run the command again to see if we get our cached retrieval message:
cacheSolve(test_makeMatrix_object_1)

# It worked!

# Let's just multiply the matrices to see if we really do get the identity matrix:
n1 <- cacheSolve(test_makeMatrix_object_1)

print(m1 %*% n1)

# Yep!

# One more test to see if our setters are working properly - we never did actually
# run $set explicitly yet.

n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)

test_makeMatrix_object_1$set(n2)

# Now if we run cacheSolve on test_makeMatrix_object_1 again, we should get the inversion of matrix
# n2, not matrix n1. All without having to run the function makeMatrix again!
cacheSolve(test_makeMatrix_object_1)
# Is this really the inversion of matrix n2? Let's find out.
m2 <- cacheSolve(test_makeMatrix_object_1)

n2 %*% m2
#Yep! Though R does give us 1 and 0 written in weird scientific notation due
# to some rounding errors somewhere! (At least it did on my machine!)


# Thanks for reading! Good luck on the rest of the course!

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
