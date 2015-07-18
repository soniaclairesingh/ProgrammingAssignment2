setwd("~/Desktop/Coursera/Data Science Specialization")
# Name: Sonia Singh
# Assnmt:R-Prog, #2 

# This function creates a special "matrix" object that can cache its inverse.
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        invr <- NULL
        set <- function(y) {
                x <<- y
                invr <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invr <<- inverse
        getinverse <- function() invr
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve should retrieve the inverse 
# from the cache.This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        invr <- x$getinverse()
        if(!is.null(invr)) {
                message("getting cached data.")
                return(invr)
        }
        My_data <- x$get()
        invr <- solve(My_data)
        x$setinverse(invr)
        invr
}

## Example:
x = rbind(c(3, -1/5), c(-2/1, 6))
m = makeCacheMatrix(x)
m$get()
cacheSolve(m)

#Output:
# > ## Example:
#         > x = rbind(c(3, -1/5), c(-2/1, 6))
# > m = makeCacheMatrix(x)
# > m$get()
# [,1] [,2]
# [1,]    3 -0.2
# [2,]   -2  6.0
# > cacheSolve(m)
# [,1]       [,2]
# [1,] 0.3409091 0.01136364
# [2,] 0.1136364 0.17045455
