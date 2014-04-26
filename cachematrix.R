##WELCOME, in this file we do three things:
#1) define a function to generate a random invertible test matrix,
#2) define a function to cache the test matrix, and
#3) define a function to retrieve the inverse of the test matrix

##this function will generate a random invertible test matrix, "tx" and store it in your local environment.
#call tm() to have a fresh randomly generated test matrix to test my functions.
#the default size is 10x10 but this can be changed by giving tm a differnet numeric argument.
tm <- function(size=10){
    tx <<- replicate(size,rnorm(size))
} 

##this funcion caches a test matrix 'x' and has attributes that allow you to
#1) set a new test matrix 2) get (print) the test matrix
#3) set the inverse of the test matrix and 4) get(print) the inverse of the test matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
    set <- function(y){
                       x <<- y
                       m <<- NULL
                   }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#The first time you executef this funciton it will calculate and return the inverse of the matrix set in the function "makeCacheMatrix".
#Any subsequent time you run the function it will retrieve the cached value of the inverse of the matrix stored as "x$getinverse(),
#and print "retrieving cached inverse. This will save you time! seriously... try it iwth a 1000x1000 matrix.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){
      message("retrieving cached inverse")
      return(m)
  }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    ## Return a matrix that is the inverse of 'x'
}
