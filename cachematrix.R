makeCacheMatrix <- function(x = matrix()) {
x<-NULL
set<-function(y){
  x<<-y
  inv<<-NULL
}
get<-function() x
setinv<-function(inverse) inv<<-inverse
getinv<-function() inv
list(set=set,get=get,setinv=setinv,getinv=getinv)
}



## The following function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated then cacheSolve 
## retrieves the inverse stored in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
