## This piece of code provides two functions. The first one, "makeCacheMatrix", creates a special
#  object (a list) from a matrix which can store and retrieve its value and its inverse's value.
#  The second one, calculates the inverse of a matrix stored as objects of this format. returning
## the cached value if available and storing it otherwise.



## makeCacheMatrix creates a special object (a list) which stores not only the matrix passed to it,
# but also functions for setting (set) and  getting (get) its value and setting/getting 
# (setinverse/getinverse) its inverse. Note that it does not calculate the inverse,
## and if it is not already set, the getinverse subfunction will return NULL.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                # Creating the placeholder for the inverse matrix
    set <- function(y) {   # for setting the matrix's value
        x <<- y
        inv <<- NULL           # If we set a new value to the matrix, the cached inverse is discarded
    }
    
    get <- function() x    # for getting the matrix's value
    setinverse <- function(inverse) inv <<- inverse   # for setting the inverse's value
    getinverse <- function() inv                      # for getting the inverse's value
    
    # This is the structure of the special object which represents the matrix with a cached placeholder
    # for its inverse. The actual variables were capture in the closure created by the subfunctions
    # above. For more about the closure programming concept, see
    # http://en.wikipedia.org/wiki/Closure_(computer_programming)
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve takes an object of the type created by makeCacheMatrix as an input and returns
#  the inverse of the matrix stored within this object. If the value was already cached, it will be
#  returned, otherwise the inverse wll be calculated and its value will be stored in the special
#  object before returning the result.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()    # Let's see what is stored in the cache
    if (!is.null(inv))       # if there's anything other than NULL, it must be the cached inverse
        message("getting cached inverse")  # Let's warn that we're using the cache
    else {                   # There was nothing on the cache
        data <- x$get()      # Retrieve the matrix from the special object
        inv <- solve(data, ...)    # Calculate the inverse
        x$setinverse(inv)    # Store a copy of it on the cache for future use
    }
    inv                      # Return the inverse
}
