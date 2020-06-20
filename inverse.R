makeVector <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {  # for example if you have "a" which is of makevector type
                        # you can change the a value using a$set   
     x <<- y
    i <<- NULL   #clears the cache
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse 
  # setinverse.. it retrieves its value from cachemean function     
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
  #returning a named list is useful for e.g a$setinverse gives u the inverse. 
}
cacheinverse <- function(x, ...)
  #cacheinverse makes benefit from the lexical scoping thus you 
  #should have an argument of makeVector type. 
  {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <-solve(data)
  x$setinverse(i)
  i
}