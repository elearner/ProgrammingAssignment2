## Functions for caching matrix inversion that potentially is time-consuming computation.

## Creates spetial matrix container with cache.
makeCacheMatrix <- function(cachedMatrix = matrix()) {
    solution <- NULL
    setMatrix <- function(newCachedMatrix) {
        cachedMatrix <<- newCachedMatrix
        solution <<- NULL
    }
    getMatrix <- function() cachedMatrix
    setSolution <- function(newSolution) solution <<- newSolution
    getSolution <- function() solution
    list(
        setMatix = setMatrix,
        getMatrix = getMatrix,
        setSolution = setSolution,
        getSolution = getSolution)
}

## Calculate inverse of matrix if it is not calculated yet or return
## cached in opposite case.
cacheSolve <- function(x, ...) {
    solution <- x$getSolution()
    if(!is.null(solution)) {
        message("getting cached data")
        return(solution)
    }
    data <- x$getMatrix()
    solution <- solve(data, ...)
    x$setSolution(solution)
    solution
}
