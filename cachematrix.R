## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This method creates matrix, and sets the inverse of that matrix in global scope 
# helper methods include get & set matrix from cache of golbal scoping (setsolve, getsolve)
# allows update of global set inverse of in case matrix content changes ( func set())
# additional helper method to fetch data (list)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  #define set for updating inverse matrix  
  setsolve <- function(solve) m <<- solve
  #define get for fetching the inverse of matrix  
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
# this function gets inverse of matrix which in  global scoping
# if found, prints data in cache
# if not, inverse the new matrix data and set it to global scope inverse matrix variable
cacheSolve <- function(x, ...) {
 # get the matrix x
 m <- x$getsolve()
 # check if global inverse of matrix is already set, if yes get matrix from cache
 if(!is.null(m)) {
        message("getting cached data")
        return(m)
  }
  data <- x$get()
  #using R solve(), inverse the matrix
  m <- solve(data, ...)
  x$setsolve(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
