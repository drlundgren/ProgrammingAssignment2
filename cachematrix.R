
## This function creates a special "matrix" object that can cache its inverse.
## It supports 'set' and 'get' operations on the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {  
        
    ## initialize cache
    m <- NULL
    
    ## set a new matrix and "clear" the cache (no inverse matrix in cache)
    set <- function (y) {
        x <<- y
        m <<- NULL
    }
    
    ## get the original matrix
    get <- function () { 
        return(x) 
    }
    
    ## set (cache) the inverse of the matrix  
    setInverse <- function (x) {
        m <<- x
    }   
    
    ## get the inverse of the matrix
    getInverse <- function () { 
        return(m) 
    }
    
    ## creates a vector (list) of the functions in this function
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}



## This function returns the inverse of the matrix provided as input
## (returned by makeCacheMatrix function). 
## If the inverse already exists, then it is retrieved from the cache. 
## Else (inverse does not exist), the inverse is computed and stored in the cache. 
## The function returns the inverted matrix.   
## Note: The function assumes that the provided matrix is invertible.

cacheSolve <- function(x, ...) {

    ## if inverse of matrix exists (already computed), get it from the cache and return it
    m <- x$getInverse()
    if (!is.null(m)) {
        message("retrieve inverted matrix from cache")
        return(m)
    }
    
    ## else if inverse of matrix is not in cache: 
    ##     get matrix, compute inverse (using solve()), cache it, and return it
    mat <- x$get()
    m <- solve(mat)
    x$setInverse(m)
    m
}
