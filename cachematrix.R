# Month 2 week 3 programming assignment
# for John Hopkins Data Science Specializtion
# cachematrix.R
# by gdolwell
# 19 December 2017


# makeCacheMatrix takes a matrix and stores the matrix and caches
# its inverse

# Make the matrix object

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  # use setX to change X and reset the inverse to Null
  setX <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  # use getX to retrieve the stored matrix
  getX <- function(){
    x
  }
  
  # use setInverse to store the inverse of the matrix
  setInverse <- function(invertedM){
    inverse <<- invertedM
  }
  
  #use getInverse to return the stored inverted matrix.
  getInverse <- function() {
    inverse
  }
  return(list(setX = setX, getX = getX, setInverse = setInverse, 
              getInverse = getInverse))
}


# Solve the matrix and cache the inverse in the matrix object

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  
  # check to see if a stored value for the inverted matrix exists, 
  # return the inverted matrix if it exists
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("Returning cached matrix.")
    return(inverse)
  }
  
  # if inverse is null, solve the inverse of x and cache the result
  myMatrix <- x$getX()
  inverse <- solve(myMatrix, ...)
  x$setInverse(inverse)
  return(inverse)
}

# Check for expected results

my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$getX()

my_matrix$getInverse()
cacheSolve(my_matrix)

my_matrix$getInverse()
cacheSolve(my_matrix)
