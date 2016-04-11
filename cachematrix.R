## This function create a special "matrix" that can cache its
## own inverse.

## This function will set and get the value of the matrix.
##  Then the function will set and get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
		m<-NULL
		set,-function(y) {
				x<<-y
				m<<-NULL
		}
		get<-function() x
		setinverse<-function(inverse) m<<-inverse
		getinverse<-function() m
		list(set = set, get = get,
				setinverse=setinverse,
				getinverse=getinverse)
						

}



## This function computes the inverse of the special "matrix" returned by the function makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)) {
        	message("getting cached data")
        	return(m)
        }
        data<-x$get()
        m<-solve(data,...)	
        x$setinverse(m)
        m
}
