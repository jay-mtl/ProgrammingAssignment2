## This code consists of two functions that are : 
##      makeCacheMatrix: function that allow to set a matrix, cached this 
##      matrix, inverse it and cached the inverse.
##      cacheSolve: function that compute the inverse of a matrix if not in 
##      cache and return it.


## makeCacheMatrix is a function that return a list of 4 functions
## that allow to set a matrix (i.e., cached), get this cached matrix, set the 
## inverse of a matrix (i.e., cached, if needed), and get this cached inverse

makeCacheMatrix <- function( x = matrix() ) {
        ## Set the inverse to NULL for starters
        inv <- NULL
        ## Create a function set that caches inv et x
        set <- function( y ) {
                x <<- y
                inv <<- NULL
        }
        ## Create a function get that returns the cached matrix x
        get <- function() {
                x
        }
        ## Create a function setInverse that cashes the inverse of matrix x
        setInverse <- function( inverse ) {
                inv <<- inverse
        }
        ## Create a function getInverse that return the inverse
        getInverse <- function() {
                inv
        }
        ## return  the makeCachMatrix object as a list of 4 functions
        list( set = set, get = get, setInverse = setInverse, 
              getInverse = getInverse )
}


## cacheSolve is a function that check if the inverse of the matrix x
## exist, if it's exist the inverse is return, if not the inverse of 
## the matrix is compute then return.

cacheSolve <- function(x, ...) {
        ## We start by get the inverse of the matrix x
        inv <- x$getInverse()
        ## We check if the inverse of matrix x has already be calculate (i.e., 
        ## the inv matrix is not NULL) or not (i.e, inv is NULL) 
        if( !is.null(inv) ) {
                ## If inv is not NULL, so it's in the cashe, we print a message
                ## specifying that we get the cashed inverse of the matrix
                ## x
                message( "getting cached data" )
                ## We return the cashed inverse of the matrix x
                return( inv )
        }
        ## If inv is NULL we start to put the matrix x in data with the get 
        ## function
        data <- x$get()
        ## We now calculate the inverse of the matrix x (i.e., the matrix data)
        ## for this we use the solve command, and we store the result in the 
        ## variable inv
        message( "computation is necessary, no cached data" )
        inv <- solve( data, ... )
        ## We now cahsed the inverse of the matrix x to the variable inv with the 
        ## function setInverse
        x$setInverse( inv ) 
        ## We now return the inverse of the matrix x
        inv
                
}

