## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # initializing inverse as NULL
    set <- function(y){ # set value of matrix
        x <<-y # note double arrow assignment operator, managing variables at different levels to parent
        inv <<- NULL
    }
    get <- function() {x} #function to get matrix x
    setInverse <- function(inverse) {inv <<- inverse} #set value of inverse
    getInverse <- function() {inv} #obtain value of inverse
    list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)){ # checking if inverse is NULL
        message("getting cached data")
        return(inv) # returns inverse value
    }
    mat <- x$get()
    inv <- solve(mat,...) #compute inverse of matrix
    x$setInverse(inv) #set value of inverse in cache
    inv  ## Return a matrix that is the inverse of 'x'
}

## Testing out cacheSolve function with variable called pmatrix
pmatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
pmatrix$getInverse()
cacheSolve(pmatrix)
pmatrix$getInverse()
