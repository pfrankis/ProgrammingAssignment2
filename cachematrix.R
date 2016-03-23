## The following functions allow you to create a special "matrix" object
## that can cache its inverse and to calculate the said inverse if it 
## it hasn't been calculated before

## This function creates the special "matrix" object whic is really 
## a list of functions to either set or get the value of the matrix
## or set or get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse global variable
  inv <- NULL  
  
  ## define the set funciton which basically assigns a new value to the matrix
  ## and re-initializes the inverse variable to Null again.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## definte the get funciton which just returns the value of the matrix object
  get <- function() x
  
  ## define the setInverse function which assigns the value of the inverse variable
  setInverse <- function(inverse) inv <<- inverse
  
  ## define the getInverse function which just returns the value of the inverse variable
  getInverse <- function() inv
  
  ## return a list of all the functions we defined above
  list(set = set, get = get ,
       setInverse = setInverse , 
       getInverse = getInverse)
}


## This function calculates the inverse of the special "matrix" created using the
## "makeCacheMatrix" function above.  Before it does that though, it first
## checks to see if the inverse has already been calculated and the matrix hasn't
## been changed.  In that case it skips the calculation and uses the cached inverse
## instead.  Otherwise it calculates the inverse of the matrix and sets the value
## of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  ## use the getInverse function defined above to get the current value of the inverse
  ## variable and store it locally
  my_inv <- x$getInverse()
  
  ## If it already exists, we're done, just return the cached value
  if(!is.null(my_inv)) {
    message("getting chached inverse")
    return(my_inv)
  }
  
  ## other wise, get the actual matrix from the special "matrix" object
  data <- x$get()
  
  ## calculate the inverse and set it to the local variable 
  my_inv <- solve(data, ...)
  
  ## use the setInverse function defined above to actually set the inverse 
  ## variable associated with the special "matrix" object
  x$setInverse(my_inv)
  
  ## return the value of the local inverse variable 
  my_inv 
}
