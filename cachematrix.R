## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## Calling this function creates a special "matrix" object that can cache its inverse.
## When you call this function to create matrix, then you can 
## create that SPECIAL matrix "a": a <- makeCacheMatrix()
## set the value of that special a to be "your matirx e.g. k" :a$set(k) and then you can call it to look at i: a$get()

makeCacheMatrix <- function(x = matrix()) {
        ## Set inverse matrix to null
        i<- NULL
        
        set <-function (y){ 
                x<<-y
                i <<-NULL }
        get<-function()x
        setinv<-function(solve) i <<-solve
        getinv<-function()i
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## x is the special matrix that we have defined with several functions: 
        ##  'get', 'set' 'getinv' and 'setinv'
        ## First read inverse of this matrix if it is available/cached
        i<-x$getinv()
        if(!is.null(i)){
                return(i)
        }
        ## this is else, i.e. we do not have it cached yet
        data<-x$get()
        i <-solve(data, ...)
        x$setinv(i)
        i 
}
