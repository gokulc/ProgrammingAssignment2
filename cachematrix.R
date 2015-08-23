## cacheSolve is R code to cache matrix inverse. Matrix inverse is 
## time consuming operation. Every time one calls this function large 
## amount of computer resource is booked by this function and it does 
## take time. So the idea is to store the result in cache (memory)
## and retrieve it when the inverse function is called on any of the 
## cached matrix-matrix inverse pairs. The function takes advantage of
## Lexical Scoping in R language.

## makeCacheMatrix checks if a copy of the matrix has been saved
## in cache, and if yes retrieves solve (inverse) from cache. Else it
## sets a new environment to save the matix-matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve tries to get matrix inverse off makeCacheMatrix. If the result
## is an inverse matrix then bingo! else it gets a NULL, upon which it 
## goes about the regular way of computing the inverse and saving the pair 
## in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

