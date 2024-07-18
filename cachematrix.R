## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Function to get the matrix
  get <- function() {
    x
  }
  
  # Function to set the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Function to get the inverse of the matrix
  getInverse <- function() {
    inv
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # Check if the inverse is already cached
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Get the matrix from the special object
  data <- x$get()
  
  # Compute the inverse of the matrix
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setInverse(inv)
  
  inv
}

# Example
# Create a matrix
mat <- matrix(c(1, 2, 3, 4), 2, 2)

# Create the special matrix object
specialMatrix <- makeCacheMatrix(mat)

# Calculate and cache the inverse
inverse1 <- cacheSolve(specialMatrix)
print(inverse1)

# Retrieve the cached inverse
inverse2 <- cacheSolve(specialMatrix)
print(inverse2)

