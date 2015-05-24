## These functions will calculate the inverse of a matrix, and cache 
## the result.  If the matrix has not changed, and if the matrix has 
## already been calculated, the functions will pull the answer from
## the cache, instead of calculating the inverse again. 

## makeCacheMatrix creates a list that stores the functions to set the 
## matrix (set), get the matrix (get), set the inverse of the matrix 
##(setinv), and get the inverse of the matrix (getinv).can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	  ## 
	  i<-NULL
	  set<- function(y) {
		x<<-y
		i<<-NULL
	  }
	  get<- function() x
	  setinv <- function(inv) i<<-inv
	  getinv <- function() i
	  list(set = set, get = get,
	  	 setinv=setinv,
		 getinv=getinv)	

}


## cacheSolve either calculates the inverse, or pulls the answer
## from the cache.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
	  ## Verify that the value of i exists and isn't NULL
	  ## If it exists, it will return a message and the
	  ## cached inverse of the matrix.

	  i<- x$getinv()
	  if(!is.null(i)){
		message("Getting cached data")
		return(i)
	  }

	  ##  If the inverse was not cached, this next function
	  ##  will calculate the inverse of the matrix, and print it.

	  data<-x$get()
	  i<-solve(data,...)
	  x$setinv(i)
	  i
}
