## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Assignment written by Haifeng Yu on 2017-03-09
## This makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## With the input of a matrix ("x"), this function returns a list of functions that are named
## as x$set(), x$get(), x$setinv() and x$getinv().
## Because of lexical scope, values of x and inv can be passed through between MakeCacheMatrix and 
## CacheSolve functions.


makeCacheMatrix <- function(x = matrix()) {
        ## initialize condition and clear cache
        inv <- NULL
        ## $set() pushes the matrix in the parameter to x and clear cache
        ## here x and inv are lexical scope variables
        set <- function (y = matrix()) {
                x <<- y
                inv <<- NULL
        }
        ## $get() has no parameter, it simply returns x - the original matrix in the 
        ## parameter when makeCacheMatrix is called
        get <- function () x
        ## $setinv() has one parameter and pushes it to the lexical scope variable inv
        setinv <- function (inver)  inv <<- inver
        ## $getinv() has no parameter, simply returns the inverse matrix in variable inv
        getinv <- function () inv
        ## makeCacheMatrix then ends by returning a list of the above mentioned four funcitons
        list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## 
## Assignment written by Haifeng Yu on 2017-03-09
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## Check if the inverse matrix is already cached in the matrix "inv" 
        ## by calling one of the four functions in the list - the $getinv() 
        inv <- x$getinv()
        ## If yes, print a message that data obtained from cache
        ## return the cached matrix and exit
        if (!is.null(inv)) {
                message ("Fetching data from the cache")
                return (inv)
        }
        ## Otherwise, call the $get() function in the list to get the origin matrix and store
        ## in the variable "data"
        data <- x$get()
        ## Use solve to calculate the inverse of this matrix
        inv <- solve(data, ...)
        ## call another function $setinv from the function list created by makeCacheMatrix
        ## to store the inverse matrix in the "cache" variable - "inv". Here lexical scope
        ## helped to maintain the data in the cache even when the $setinv() function ends.
        x$setinv(inv)
        ## then return the value of this inverse matrix
        inv
}
