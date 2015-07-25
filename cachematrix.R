## Create the special vector

makeMatrix <- function(q = matrix()) {

  inv = NULL
  set = function(w) {
    q <<- w
    inv <<- NULL
  }
  get = function() q
  
  setinv = function(inverse) inv <<- inverse 
  
  getinv = function() inv
  
  list(set=set, 
       get=get, 
       setinv=setinv, 
       getinv=getinv)
}


## Calculates inverse of a matrix

cacheInverse <- function(q, ...) {
  
  inv = q$getinv()
  
  if (!is.null(inv)){
    message("Retrieving cached data...")
    return(inv)
  }
  
  mat.data = q$get()
  inv = solve(mat.data, ...)
  
  q$setinv(inv)
  
  return(inv)
}