makeVector <- function(x = numeric()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmean <- function(mean) m <<- mean
      getmean <- function() m
      list(set = set, get = get,
           setmean = setmean,
           getmean = getmean)
}

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

#This doesn't work... Attempt 1
samplevect <- as.list(c(1:50))
is.atomic(samplevect)

makeVector(samplevect)


cachemean(samplevect)


# While this does... why? Attempt 2

q <- makeVector()

q$set(1:50)

cachemean(q)

cachemean(q)

# Will this work? Attempt 3

x <- makeVector()

x$set(samplevect)

cachemean(x)

# Nope - let's try this, Attempt 4

samplevect2 <- 1:50

x <- makeVector()

x$set(samplevect2)

cachemean(x)

# Okay, that works.

is.numeric(samplevect2)
is.numeric(samplevect)

# I dunno why attempt 3 did not work and gave me the error:
# > cachemean(x)
# [1] NA
# Warning message:
#       In mean.default(data, ...) :
#       argument is not numeric or logical: returning NA

# if is.numeric tells me samplevect is numeric?

# Oh wait it's because I forgot I had NOT set samplevect to be as.list().

# If it's a list it is NOT considered numeric.


rm(list=ls())
