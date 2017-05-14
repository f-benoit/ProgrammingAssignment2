
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  set <- function(y) {
    x <<- y
    invMat<<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invMat <<- inverse
  getInverse <- function() invMat
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



#The cachesolve function returns the inverse of the matrix
#The function will first check if the inverse of the matrix has already been computed
#If yes the function will get the cached inverse of the matrix otherwhise it will compute the inverse
#once the inverse of the matrix has been computed it will be cached so the second execution will be much quicker
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMat <- x$getInverse()
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  data <- x$get()
  invMat <- solve(data, ...)
  x$setInverse(invMat)
  return(invMat)
  
}


#test 
#first execution will be slower
#then the second execution will use the cached inverse hence will be quicker
r <- rnorm(1000000)
mat <- matrix(r, nrow=1000, ncol=1000)
X <- makeCacheMatrix(mat)

cacheSolve(X)

cacheSolve(X)

