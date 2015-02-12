## makeCacheMatrix encapsulates the environment for e cached matix
## cacheSolve returns the inversed matrix
## USAGE:
## x <- makeCacheMatrix(my_matrix)
## cacheSolve(x)
## RESET matrix to a new one
## x$set(my_newMatrix)
##
## other functions internal use only, not not call them from outside
makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse){
    m_inv <<- inverse
  } 
  
  getinverse <- function() {
    m_inv
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## lookups if the inverse of the matrix x exist in cache
## if  it allready cached it will be returned, otherwise it calulated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m_inv <- x$getinverse()
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  
  # posible bug in additional arguments, if they are changed 
  # the inverse it not recalclated. I nice implementaion  should 
  # handle this issue
  m_inv<-solve(x$get(),...)
  x$setinverse(m_inv)
  m_inv
}

