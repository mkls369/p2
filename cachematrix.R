
# makes list object for matrix inverse, which later can be used by cacheSolve
makeCacheMatrix <- function( m = matrix() ) {

    i <- NULL

    # sets the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    # gets the matrix
    get <- function() {
    	# Returns the matrix
    	m
    }

    #sets the inverse of the matrix
    setinverse <- function(inverse) {
        i <<- inverse
    }

    # Method to get the inverse of the matrix
    getinverse <- function() {
        ## Return the inverse property
        i
    }

    # Return a list of the methods
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#  Makes an inverse matrix from an object which is produces by makeCacheMatrix. 
#If the inverse exits it just returns the inverse matrix, if doesn't exist it calculates the inverse and returns it
cacheSolve <- function(x, ...) {

    ## Return inverse matrix of x
    m <- x$getinverse()

    ## Returns the inverse if it exists
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    # Gets the matrix from our object
    data <- x$get()

    #Calculates the inverse using matrix multiplication
    m <- solve(data) %*% data

    # Sets the inverse to the object
    x$setinverse(m)

    # Returns the matrix
    m
}

