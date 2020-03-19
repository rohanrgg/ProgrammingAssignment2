## This assignment asks for two functions: makeCAcheMatrix() & cacheSolve()
## The two functions are meant to calculate the inverse of a sqaure matrix,
## assuming that, all matrices input to these functions can be inverted.
## These functions also utilise the lexical scoping method used in R to 
## store results in cache, which ensures that caclculations that are repeatedly 
## being undertaken and they require substantial time to compute results can be 
## fast-tracked by saving results in cache, so that they can be rertrieved rather
## than recalculated.

## makeCacheMatrix is a function which creates a list of setter and getter functions.
## The idea is to create an function environment within which cached computed values can be stored.
## makeCacheMatrix initializes the x with the matrix to be used for computation of the inverse and
## creates a list of function as follows-
## $get() when called will retrieve the input matrix
## $set() when called will update the stored matrix with a new matrix
## $getinverse() when called will retrieve the inverse matrix if stored in cache
## $setinverse() when called will update the cache whenever a new inverse is caculated
## The makeCacheMatrix on its own is incomplete, in the sense that it cannot caculate the inverse 
## without cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y){
    x<<-y
    I<<-NULL
  }
  get <- function()x
  setinverse <- function(inverse) I <<- inverse
  getinverse <- function() I
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)

}


## cacheSolve is a function that computes the inverse of a special matrix object created
## using the makeCacheMatrix function and either calculates the inverse of the matrix using solve() 
## or retrieves a cached value if initial input matrix x hasn't changed.
## The Function checks whether there is a cached computation whenever it is called, if no  
## value is found, it computes the inverse and stores it in cache.
## If data is infact rertrieved a message is displayed highlighting that data has in fact
## been retrieved from cache.


cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
