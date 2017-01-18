
## Implement matrix capable of caching its own inversion.
##
## Given a matrix returns a list object whose elements are functions
## - set: store original matrix object
## - get: get the original matrix object
## - setInversion: store inversion of matrix into cache
## - getInversion: retrieve cached inverted matrix, if available or NULL
## 
## For example:
##   cm <- makeCacheMatrix(matrix(1,4,2,2))
##
## Call methods of cm object, for example
##   cachedInversion <- cm$getInversion()
makeCacheMatrix <- function(x = matrix()) {
    # our cached inverted matrix, initially NULL
    cachedInversion <- NULL
    # makes matrix y into a matrix capable of caching its inversion
    set <- function(y) {
        x <<- y
        cachedInversion <<- NULL
    }
    # get our inversion-caching matrix
    get <-function() {
        x
    }
    # store the inversion of a matrix in the cache
    setInversion <-function(an_inversion) {
        cachedInversion <<- an_inversion
    }

    # return cached inverted matrix
    getInversion <- function() {
        cachedInversion
    }

    # a list where elements are functions
    # useage:
    #   foo <- makeCacheMatrix(x)
    #   invertedMatrix = foo$getInversion()
    list(set = set, get = get, setInversion = setInversion, getInversion = getInversion)
}

## x is a cachingMatrix obtained by calling makeCacheMatrix()
cacheSolve <- function(x, ...) {
        ## check to see if x has inversion in it's cache
        invertedMatrix = x$getInversion()
        if (!is.null(invertedMatrix)) {
            # it's in the cache, celebrate!
            message("Cache hit")
            return(invertedMatrix)
        }

        # get the uncaching matrix so we can call solve() using it as an argument
        m <- x$get()
        inversion = solve(m)
        # store result in the cache
        x$setInversion(inversion)

        # return the inversion of the matrix
        inversion
}
