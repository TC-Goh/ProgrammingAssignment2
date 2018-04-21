## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly 
## These pair of functions can cache the inverse of a matrix.


## This function ------ makeCacheMatrix 
## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y) {
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, 
             get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}



## This function ------ cacheSolve 
## computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}


## ---- Here is the test code on a 3x3 matrix and output
## > testmat<-matrix(c(2,-3,8,5,-4,1,5,4,2),3,3)
## > testmat
##      [,1] [,2] [,3]
## [1,]    2    5    5
## [2,]   -3   -4    4
## [3,]    8    1    2
## > testmat2<-makeCacheMatrix(testmat)
## > cacheSolve(testmat2)
##             [,1]        [,2]        [,3]
## [1,] -0.03858521 -0.01607717  0.12861736
## [2,]  0.12218650 -0.11575563 -0.07395498
## [3,]  0.09324759  0.12218650  0.02250804
## > 
