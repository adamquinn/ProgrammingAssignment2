#makeCacheMatrix() creates a cacheable "matrix" object.  Function contains 4 main functions:
#set, get, setmatrix, getmatrix

makeCacheMatrix <- function(x = numeric()) {
  
  m <- NULL
  
  # set function will create the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #get function returns the created matrix
  get <- function()x
  setmatrix <- function(matrix) m<<- matrix
  getmatrix <- function()m
  
  #list of functions that can be utilized.  x$set and x$get create and return matrix respectively
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
  
}



#cacheSolve returns the inverse of the matrix returned by the "get" function in makeCacheMatrix()
#If the inverse has already been calculated the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  m <- x$getmatrix()
  
  #logic checks to see if the matrix is already cached.  If so, a message is printed and the 
  #solve function is performed on the object.  The object is then returned to the calling function
  if(!is.null(m)) {
    message("data is cached")
    solve(m)
    return(m)
  }
  
  #if matrix is not already cached, the solve function is performed and the object is cached using
  #the setmatrix() operation from the makeCacheMatrix() function.
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}