
## Put comments here that give an overall description of what your
## functions do
#Caching the Inverse of a Matrix

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
         im <- NULL
         set <- function(y) {
                  x <<- y
                  im <<- NULL
         }
         get <- function() x
         setim <- function(imatrix) im <<- imatrix
         getim <- function() im
         list(set = set, get = get,
              setim = setim,
              getim = getim)
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getim()
        if(!is.null(im)) {
                 message("getting cached data")
                 return (im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setim(im)
        im
}
