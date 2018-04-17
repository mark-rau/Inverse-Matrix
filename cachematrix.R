
## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## defining argument with matrix
        inv <- NULL ## inv as NULL, can hold value of matrix inverse
        set <- function(y) { 
                x <<- y ## define value of matrix in parent environment
                inv <<- NULL ## if a new matrix is created, value of inv is NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse ## assigns value to inv in parent environment
        getinverse <- function() inv ## gets the value of inv where called
        list(set = set, get = get,
             setinverse= setinverse,
             getinverse = getinverse) ## use list to refer to function with $ operator
}
        

## cacheSolve calculates the inverse of the matrix after being returned by makeCacheMatrix
## If the inverse has been calculated already, cacheSolve will recall the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}
