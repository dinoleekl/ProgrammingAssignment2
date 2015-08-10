## Computing the inverse of a matrix is costly
## We therefore define two functions, makeCacheMatrix and cacheSolve,
## to cache the inverse of a matrix, 
## assuming that this matrix is invertible



## makeCacheMatrix creates a special "matrix",
## which can
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the inverse of the matrix
##   4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # inverse not yet known when special matrix
                # is first created
    set <- function(y){
      x <<- y # reset the special matrix to y
      inv <<- NULL # inverse of y not yet computed
    }
    get <- function() x # give the special matrix
    setinv <- function(new_inv) inv <<- new_inv # set the inverse
    getinv <- function() inv # give the inverse
    list(set = set, get=get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrix" 'x'
## returned by makeCacheMatrix, or return the cached inverse
## if it has previously been computed
cacheSolve <- function(x, ...) {
    inv <- x$getinv() # extract the inverse of x
    if(!is.null(inv)){
      # inverse has been computed previously
      message("getting cached inverse")
      return(inv) # return the cached inverse 
                  # and skip subsequent computation
    }
    # inverse is not yet computed
    data <- x$get() # extract the matrix
    inv <- solve(data,...) # compute its inverse
    x$setinv(inv) # store the inverse
    inv ## Return a matrix that is the inverse of 'x'
}

## Sample output
## m <- makeCacheMatrix(matrix(1:4,2,2))
## > m$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > m$getinv()
## NULL

## > cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## > cacheSolve(m)
## getting cached inverse
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## > m$getinv() %*% m$get()
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1