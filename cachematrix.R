## Dan N / Reiuiji
## MIT License

##makeCacheMatrix:
## Will output a list of the following functions:
##   (*): set : set the values of the matrix
##   (*): get : get the values of the matrix
##   (*): set_inverse : set the values of the inverse
##   (*): get_inverse : get the values of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Init default value for inverse as a NULL
  inverse <- NULL
  
  ## set : will set the values of the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## get : will get the values of the matrix
  get <- function() x
  
  ## set : will set the values of the invserse
  set_inverse <- function(input) inverse <<- input
  
  ## get : will get the values of the inverse
  get_inverse <- function() inverse
  
  ## list of all the functions
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


##cacheSolve:
## Calculate the inverse of the matrix. Then it will
## check if the inverse has been calclated. If it
## was, then it grabs the inverse from the cache and
## skip recalculation of the inverse. Else if the
## inverse is not defined then it will calculate the
## inverse and store it in the cache

cacheSolve <- function(x, ...) {

  ## Check if inverse was previously calculated
  inverse <- x$get_inverse()
  ## if inverse does exist then it will return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## else it will recalculate it.
  
  ## grab the matrix
  data <- x$get()
  
  ## calculate the inverse
  inverse <- solve(data, ...)
  
  ## Set the inverse for the matrix
  x$set_inverse(inverse)
  
  ##return the newly calculated inverse
  inverse
}
