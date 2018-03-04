
## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	invMat <- NULL
	setMatrix <- function(n) {
	  x <<- n
	  invMat <<- NULL
	}
	getMatrix <- function() x
	setInverse<-function(i) invMat <<- i
	getInverse<-function() invMat
	list(set = setMatrix, 
	     get= getMatrix,
	     setInverse = setInverse,
	     getInverse = getInverse)
}



# This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat<-x$get()
  cachedInverted <- x$getInverse()
  
  if (identical(mat,x) & !is.null(cachedInverted)) {
    print ('Returning cached inverted matrix...')
    return (m)
  }
  matInv<-solve(mat)
  x$setInverse(matInv)
  matInv
}

test<-function(){
  q1<-matrix(rnorm(9),ncol=3,nrow=3)
  A1 <- makeCacheMatrix( q1)
  A1$get() 
  A1$getInverse()
  q2<-matrix(rnorm(9),ncol=3,nrow=3)
  A1$set(q2)
  cacheSolve(A1) 
  A1$getInverse()   
}

#test()