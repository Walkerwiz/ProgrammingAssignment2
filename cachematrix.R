## This file contains two functions "makeCacheMatrix" and "cacheSolve"
## Have fun with it! 

## This function makes the cache for the inverse matrix.
## The returned object is a list with "$get" provides the original matrix,
## "$set" re-sets the matrix, "$setinv" finds the inverse (notice that manual input required)
## and "$getinv" returns the current inverse matrix. 
## If the inverse matrix has not been calcuated by "$setinv", "$getinv" returns null.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        message("Data are initialized to ",x[1][1], " ...")
        set <- function(y){
                x <<- y
                message("Data are initialized to ",x[1][1], " ...")
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(get = get, set = set, setinv = setinv, getinv = getinv)
}


## This function finds the inverse matrix of x by using the cache.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(x$getinv())){
                message("getting cached data")
                return(x$getinv())   
        }
        x$setinv(solve(x$get()))
        x$getinv()
}
