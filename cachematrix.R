## This script contains 2 functions: makeCacheMatrix and cacheSolve.
## First call makeCacheMatrix, then cacheSolve.

## makeCacheMatrix will initialize (or reset) the cached inversed matrix to NULL. It also gives 
## 4 functions to object x.

## cacheSolve will take object x and call function getInverse embedded to it to get the cached 
## value of the inversed matrix. If this value is not NULL, it will return the previously cached
## value. If it is NULL, then it will use function get() to retrieve the original matrix that was 
## provided by calling makeCacheMatrix. It will calculate the inverse and then use setInverse()
## to cache it.

## Example of use:
## output = makeCacheMatrix(x = my_matrix); where my_matrix is a variable containing a matrix.
## inversed_matrix = cacheSolve(output)


makeCacheMatrix <- function(x = matrix()) {
  # initialize/reset matrix_inverse to the value 'NULL'
  matrix_inverse <- NULL 
  
  set <- function(y) {
    x <<- y # cache the input matrix
    matrix_inverse <<- NULL # reset the cached inverse value to NULL
  }
  
  # Create a function that returns the value of the original input matrix
  get <- function() x
  
  # cache the inversed matrix
  setInverse <- function(inverse_value) matrix_inverse <<- inverse_value # inverse_value is calculated by cacheSolve
  
  # get the cached value of the matrix inverse (value is either a matrix or NULL)
  getInverse <- function() matrix_inverse
  
  # Return a list (whose values are named) containing the 4 subfunctions created above 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve looks up if the inverse of the matrix has been cached. If not, it calculates the inverse.

cacheSolve <- function(x, ...) {
    inverse_value <- x$getInverse() # call function getInverse() that's been assigned to x by makeCacheMatrix
    
    # Check if the inverse is already cached (not NULL). If so, send a message to the console and
    # return the cached value
    if(!is.null(inverse_value)) { # if the inverse was already cached (not NULL)
        message("getting cached data")  # send message to the console
        return(inverse_value) # return the cached value of the inversed matrix and exit the cacheSolve function
    }
    
    # Execute the next part if matrix_inverse is not cached (thus equals NULL) 
    data <- x$get()        # call function 'get' (created by makeCacheMatrix). This retrieves the original input matrix and assigns it to 'data'
    inverse_value <- solve(data)   # calculate the inverse of the original matrix by using solve()
    x$setInverse(inverse_value)  # call function setInverse (created by makeCacheMatrix). This function caches the inverse of the matrix.
    inverse_value           # return the inverse value
}
