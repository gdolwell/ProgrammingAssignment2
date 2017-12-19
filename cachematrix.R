# Month 2 week 3 programming assignment
# for John Hopkins Data Science Specializtion
# makeCacheMatrix
# by gdolwell
# 19 December 2017


# makeCacheMatrix takes a matrix and stores the matrix and caches
# its inverse

# my the matrix object

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setX <- function(y){
    x <<- y
    inverse <<- NULL
  }
  getX <- function(){
    x
  }
  setInverse <- function(invertedM){
    inverse <<- invertedM
  }
  getInverse <- function() {
    inverse
  }
  return(list(setX = setX, getX = getX, setInverse = setInverse, 
              getInverse = getInverse))
}


# Solve the matrix and cache the inverse in the matrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("Returning cached matrix.")
    return(inverse)
  }
  myMatrix <- x$getX()
  inverse <- solve(myMatrix, ...)
  x$setInverse(inverse)
  return(inverse)
}

my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$getX()

my_matrix$getInverse()
cacheSolve(my_matrix)

my_matrix$getInverse()
cacheSolve(my_matrix)
