## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will create a special matrix
## example usage is: foo <-makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))

makeCacheMatrix <- function(x = matrix()) {
  ## placeholder for our matrix
  mat <- NULL
  ## define the set function where we can assign a value to the matrix
  ## example foo$(matrix(1:4,nrow=2,ncol=2))
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  ## return the matrix
  ## example call: foo$get()
  get <- function() x
  
  ##function to set our inverse matrix
  ##This normally will be called by cacheSolve but in case you want to caclulate
  ##the inverse matrix and assign is you can assign it here
  setimatrix <- function(imat) mat <<- imat
  ##function to return our inverse matrix
  getimatrix <- function() mat
  list(set = set, get = get,
       setimatrix = setimatrix,
       getimatrix = getimatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat <- x$getimatrix()
  if(!is.null(mat)) {
    message("getting cached inverse matrix")
    return(mat)
  }
  ## No cached data exists so we will need to invert and cache the inverse matrix
  
  ## Get the data from the 'Special' matrix by calling get
  data <- x$get()
  ## Calculate the inverse of the matris
  imat <- solve(data, ...)
  ## Store the result of the inverted matrix back in the object
  x$setimatrix(imat)
  ## Return the inverse martix
  imat
}
