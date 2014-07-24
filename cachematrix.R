## Function creating cache matrix (CM)
## set(y) - set matrix to y
## get - gives matrix 
## setInverse(y) - sets y as an inverse of x
## getInverse - gives inverse of x

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #by default matrix is empty (NULL)
        set <- function(y) {
                x <<- y
                m <<- NULL #by default inverse matrix is empty (NULL)
        }
        get <- function() x #function that returns x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m #function that returns inverse of x
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse() 
        #checks whether inverse matrix has been already calculated
        #if yes, returns the matrix
        if(!is.null(m)){ 
                message("getting cached data")
                return(m)
        }
        #if no, calculates the inverse matrix
        mat <- x$get()
        m <- solve(mat) #calculates the inverse matrix
        x$setInverse(m)
        return(m)
}