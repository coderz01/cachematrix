makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {            
    x <<- y                   
    m <<- NULL                  
  }
  get <- function() x
  setInvmatrix <- function(InvMatrix) m <<- InvMatrix
  getInvmatrix <- function() m
  list(set = set, get = get,
       setInvmatrix = setInvmatrix,
       getInvmatrix = getInvmatrix)
  
}




cacheSolve <- function(x, ...) {
  
  m <- x$getInvmatrix()              
  if(!is.null(m)) {           
    message("cached data")  
    return(m)             
  }
  data <- x$get()             
  m <- solve(data, ...)     
  x$setInvmatrix(m)           
  m                       
}

