## makeCacheMatrix creates a special "matrix" object that can cache its 
## inverse.cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
  

##  makeCacheMatrix creates a special "matrix" object that can cache its 
## inverse.The special "matrix" is actually a list containing 4 functions to
##   set the value of the vector
##   get the value of the vector
##   set the value of the mean
##   get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("get inverse from cache.")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setinverse(inv)
  inv
}
