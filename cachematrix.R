## Two functions that cache the inverse of a matrix

## Creates a matrix that can cache its inverse
makeCacheMatrix <- function(m=matrix()){
  ## Define inverse variable
  i <- NULL
  ## Set the matrix and the inverse
  set <- function(mat){
    m <<- mat
    i <<- NULL
  }
  ## Get the value of the matrix
  get <- function(){
    m
  }
  ## Set the inverse value of the matrix
  setinv <- function(inv){
    i <<- inv
  }
  ## Get the inverse of the matrix
  getinv <- function(){
    i
  }
  ## Returns the list of functions
  list(set=set, get=get,setinv=setinv,getinv=getinv)
}

## Compute the inverse of above matrix - should read from cache if available 
cacheSolve <- function(x,...){
  ## Get the inverse of x
  i <- x$getinv()
  
  ## If i exists in the cache, return the value
  if (!is.null(i)){
    message("Getting Cached Data")
    return(i)
  }
  
  ## If the value is not in the cache, calculate and return inverse
  m <- x$get()
  i <- solve(m)
  x$setinv(i)
  i
  
}
