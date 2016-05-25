## Creare fuction to cache matrix data and then solve it.


## Create a makeCache<Matrix function for cacheable matrix for inputting it
## in cacheSolve() function which sets and gets 
## the cached values

makeCacheMatrix <- function(x = matrix()) {
  
  # verify if input is correct to makeCacheMatrix
  
  if (!is.matrix(x)) {
    stop("Please input correct matrix")
  }
  
  invmatrix <- NULL
  
  set <- function(y) {
         x <<- y
         invmatrix <<- NULL
  }
  
  # Functions for getting and setting cached invmatrix value
  
  get <- function() x
  
  # Inverse the matrix using build in solve() function in R
  setinverse <- function(solve) invmatrix <<- solve
  getinverse <- function() invmatrix
  
  list(
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
  
}



## Computes the inverse of the cacheable matrix returned by makeCacheMatrix()
## If the inverse has already been calculated and there's no change in the matrix
## then the cacheSolve() returns the cached inverse

cacheSolve <- function(cachedMatrix, ...) {
  invmatrix <- cachedMatrix$getinverse()
  
  if(!is.null(invmatrix)) {
    message("Getting cached inversed matrix")
    return(invmatrix)
  }
  
  # Create a matrix if there is cached inverted matirx
  
  data <- cachedMatrix$get()
  invmatrix <- solve(data)
  cachedMatrix$setinverse(invmatrix)
  invmartrix
  
}

 ## Inverted Matirx is returned
