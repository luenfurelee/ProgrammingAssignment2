## Caching the Inverse of a matrix

## makeCacheMatrix produces a "List" object containing 4 functions: set, get, setInverse and getInverse
## Use set function to enter a matrix to be inverted

makeCacheMatrix <- function() {

        list(
             set=function(y=natrix()) {
                x<<-y
                inv<<-NULL
             }, 
            
             get=function() x,
            
             setInverse=function(inverse) inv<<-inverse,
            
             getInverse=getInverse<-function() inv)
}


## cacheSolve computes the inverse of the matrix that was entered via the set function defined in makeCacheMatrix

## If the inverse has already been calculated, then it can be looked up from in the cache.
## Otherwise, it calculates the inverse and sets it in the cache via the setInverse function

cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'
    
        inv<-x$getInverse()
        
        if (!is.null(inv)) {
                message("getting cache data")
                return(inv)
        }
        
        else {
                mat<-x$get()
                inv<-solve(mat, ...)
                x$setInverse(inv)
                inv
        }
}
