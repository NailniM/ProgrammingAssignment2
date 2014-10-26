## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function gets the inverse of the given matrix
##to execute - source the code
##variable<-matrix where 1:4 with 2 rows and 2 columns (x<-matrix(1:4,2,2))
##y<-makeCachematric(x)
##cacheSolve(y)

makeCachematrix <- function(cm = matrix()) {
  n <- NULL
  set <- function(p) {
    cm <<- p
    n <<- NULL
  }
  get <- function() cm
  setinverse <- function() n <<- solve(cm)
  getinverse <- function() n
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##function to return matrix that is inverse of the original matrix
##first inverse is calculated, later it pulls cached data
cacheSolve <- function(cm, ...) {
  ## Return a matrix that is the inverse of 'x'
  n <- cm$getinverse()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  cm$setinverse()
  cm$getinverse()
}



