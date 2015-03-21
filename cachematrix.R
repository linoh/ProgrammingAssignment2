## Theses functions below create an object that captured and stores a matrix and cache's its inverse 
## The makeCachematrix create a specific matrix tha have capability that;-
## set the matrix value
## get the matrix value
## set the inverse value
## get the inverse value

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL                                 ## set NULL to inverse
        sMatrix <- function (y) {
                x <<- y                                 ## Asigned x to the matrix
                inverse <<- NULL
        }
        gMatrix <-function() x                          ## returning matrix x
        sInverse <-function(solve) inverse <<-solve     ## cache inverse value        
        gInverse <-function() inverse                   ## returning inverse
        list(sMatrix = sMatrix, gMatrix = gMatrix,
                sInverse = sInverse,
                gInverse = gInverse)


## This function calculate the inverse of the matrix

cacheSolve <- function(x, ...) {
        inverse <- x$gInverse                            ## getting inverse
        if(!is.null(inverse)) {                         ## check if inverse value is NULL
                message("getting cache data")           ## if true then display message
                return(inverse)
        }
        data <- x$gMatrix()                             ## getting matrix
        inverse <- solve(data, . . .)                   ## compute inverse        
        x$sInverse(inverse)                             ## cache inverse
        inverse                                         ## returning inverse value
}        

