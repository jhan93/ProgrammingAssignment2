## The code below is a combination of two functions so we can efficiently store previous runs of matrices.
## In order to save on computational time, the makeCacheMatrix outputs a list that store matricies, and their
## respective inverses if it has been calculated. The second function's role is to compute and store the inverse
## to the list outputted from the first function.

## makeCacheMatrix's purpose is to create a list where we can store the matrices fed in, and their respective
## inverses once computed. We are utilizing the <<- operator so that variables can be used and referenced
## in the next function. 

makeCacheMatrix <- function(x = matrix()) {
  m = NULL
  if(!is.matrix(x)){
    print('Please enter a matrix!')
  } else {
    
    set <- function(y) {
      x<<- y
      m<<- NULL
    }
    get <- function() x
    set_inverse <- function(solve) m <<- solve
    get_inverse <- function() m
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
  }
}

## This function is checking to see if there already exists an inverse of the matrix from the output of the
## previous function. If there already exists an inverse, it will note that we are retrieving the cached
## matrix and will return it once retrieved. If there does not exist an inverse matrix we have on file,
## it will call for the matrix, calculate the inverse, store it in the list from the previous function's 
## output and will output the inverse matrix.

cacheSolve <- function(x, ...) {

  m <- x$get_inverse()
  if(!is.null(m)){
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}
