## The functions below implement a mechanism of caching results of a function to speed-up computation.
## In this example values returned by function solve() are cached within another function object called makeCacheMatrix which also provides procedures to maintain the cached value in a consistent way.

## makeCacheMatrix stores a matrix and its reverse matrix. The function also provides 4 procedures to set and retrieve both values in a consistent way.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set<-function(val) {x<<-val; inv<<-NULL}
    	get<-function() x
    	setinverse<-function(n_inv) inv<<-n_inv
    	getinverse<-function() inv
    
    	list(get=get,set=set,getinverse=getinverse,setinverse=setinverse)
}

## cacheSolve reads the cached inverse matrix. If it has not been calculated before, the function calculates it and stores within x's environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv<-x$getinverse()
	if (is.null(inv)) {
        	inv<-solve(x$get());
	        x$setinverse(inv)
    	 }
   	 inv
}
