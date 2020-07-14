## makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse for the input (which is an invertible square matrix)
makeCacheMatrix <- function(x = matrix()) {
	m<-NULL  
	y<-NULL 

	setm<-function(y){   
		x<<-y   
		m<<-NULL  
	}
  
	getm<-function() x 
	setinv<-function(solve) m<<- solve 
	getinv<-function() m  

	list (setm=setm, getm = getm,  
	setinv = setinv,
	getinv = getinv)
}

## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache
cacheSolve <- function(xM= m(), ...) {
	m <- xM$getinverse()  
	if(!is.null(m)){   
    		message("Getting Data ......................")
    		return(m) 
    	}
    	y <- xM$getm()  
    	xM$setm(y) 
    	m <- solve(y, ...) 
    	xM$setinv(m)
	m 
}
