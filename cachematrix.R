#Function that creates 4 functions that store/retrieve objects of interests 
#of the data

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y){
                x<<- y
                inv<<- NULL
        }
        get<- function() x
        setinv <- function(solve) inv<<- solve
        getinv <- function() inv
        list(set=set, get=get,
             setinvt= setinv,
             getinv = getinv)
                
}


## function that uses the data above to check if it inverse exists of not

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}


KAPPA <- matrix(
        c(3,5,1,9),
        nrow=2
)

KAPPA2<- makeCacheMatrix(KAPPA)

cacheSolve(KAPPA2)
