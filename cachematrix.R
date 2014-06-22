###
# Assignment: Caching the Inverse of a Matrix
# 
# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly 
# (there are also alternatives to matrix inversion that we will not discuss here). 
# Your assignment is to write a pair of functions that cache the inverse of a matrix.
# 
# Write the following functions:
#       
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned
# by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the inverse
# from the cache.
# Computing the inverse of a square matrix can be done with the solve function in R.
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# For this assignment, assume that the matrix supplied is always invertible.
###

### makeCacheMatrix
# makeCacheMatrix returns a list that contains four functions:
# set: saves the input y (the matrix in this application) in the parent environment
#      and also sets the inverse to NULL which overwrites any previous inverses.
#      This leads to a recalculation of the inverse when a new matrix is set
#      because cacheSolve checks if inv is NULL.
# get: returns the the matrix
# setinverse: saves the result of the calculation (which should typically be the input)
#             in the parent environment as the object inv.
# getinverse: returns inv (can be NULL)
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      # set the value of the matrix
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      # get the value of the matrix
      get <- function() x
      # set the value of the inverse
      setinverse <- function(inverse) inv <<- inverse
      # get the value of the inverse
      getinverse <- function() inv
      # return:
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}



# cacheSolve first checks if the inverse can be found in the cache.
# If it can be found the function just returns that value and the message "getting
# cached data". Otherwise the inverse is calculated, saved using setinverse and
# returned.
cacheSolve <- function(x, ...) {
      inv <- x$getinverse() # function() inv
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv) # return ends the function
      }
      data <- x$get() # function() x
      inv <- solve(data, ...) # calculates the inverse and stores it as inv
      x$setinverse(inv) # function(inverse) inv <<- inverse
      inv
}


### A sample application of the two functions including output:
# Use macheCacheMatrix() to store the four included functions in the list "mat".
mat <- makeCacheMatrix()

# Use the set function in that list (stored as set) to set the matrix to a
# 3x3 matrix of random numbers. This matrix is not stored in the global
# environment but in the parent environment of the function.
mat$set(matrix(rnorm(9), nrow = 3))

# Get the created matrix
mat$get()
# [,1]       [,2]        [,3]
# [1,] -1.2932742 -1.2479798 -0.05475115
# [2,] -0.7045076 -0.2535353  0.39429789
# [3,]  1.0195027  0.5456111 -0.53298341

# Using the list (not the matrix) as an input to cacheSolve returns the inverse
cacheSolve(mat)
# [,1]      [,2]      [,3]
# [1,] -1.0350765 -8.992196 -6.546043
# [2,]  0.3428151  9.640222  7.096562
# [3,] -1.6289806 -7.331853 -7.132953

# Second time. The value from the cache sould be used
cacheSolve(mat)
# getting cached data
# [,1]      [,2]      [,3]
# [1,] -1.0350765 -8.992196 -6.546043
# [2,]  0.3428151  9.640222  7.096562
# [3,] -1.6289806 -7.331853 -7.132953

# Now overwriting the original matrix with a 4x4 matrix of random numbers.
# The inverse should be recalculated since the set function not only sets
# the matrix but also sets the inverse to NULL.
mat$set(matrix(rnorm(16), nrow = 4))

# Calculating the inverse.
cacheSolve(mat)
# [,1]       [,2]       [,3]      [,4]
# [1,]  0.05901862  0.3667477  0.4990513 0.2687909
# [2,] -0.52937778 -0.4574198 -0.1544053 1.5542657
# [3,] -0.95180302 -0.4122526  0.2781711 0.6133362
# [4,]  0.05051576 -0.4257568  0.3065151 0.3811089

# Second time. Again the value from the cache should be used:
cacheSolve(mat)
# getting cached data
# [,1]       [,2]       [,3]      [,4]
# [1,]  0.05901862  0.3667477  0.4990513 0.2687909
# [2,] -0.52937778 -0.4574198 -0.1544053 1.5542657
# [3,] -0.95180302 -0.4122526  0.2781711 0.6133362
# [4,]  0.05051576 -0.4257568  0.3065151 0.3811089