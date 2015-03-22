## Put comments here that give an overall description of what your
## functions do

## Function: Create a List with four elements of managing the inverse 
## of matrix.
##
## Input: x - a variable storing the origin matrix
## Output: a list with 4 elements 4 functions: set/get, set_inverse/get_inverse,
## which return the value of origin and inverse matrix repectively.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULLcm
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inv <- function(m_inv) inv <<- m_inv
    get_inv <- function() inv
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
    
}


## Function: Calculate the inverse of a specified matrix and return the inverse matrix.
## Input: a list created by function of makeCacheMatrix
## Output: an inverse of matrix strored in list x.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m_inv <- x$get_inv()
    if(!is.null(m_inv)) {
        message("getting cached matrix")
        return(m_inv)
    }
    data <- x$get()
    m_inv <- solve(data)
    x$set_inv(m)
    m_inv
}
