##This pair of functions: 1) creates a special "matrix" object that can cache its inverse out of a vector of numbers
##to be transformed into a square matrix; and then 2) computes the inverse of the special matrix. If the inverse has
##already been calculated and the matrix has not changed, then cachesolve will retrieve the inverse from the cache. 

## This function creates the special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = numeric(), d = numeric()) { #enter vector of numbers to be contained in square matrix and integer to use for row + column dimensions
        a <- matrix(x, nrow = d, ncol = d) #make square matrix object from above v and d specifications 
        i <- NULL #create null inverse matrix object
        set <- function(b) { #set the value of a and clear i if cached by prior execution of cachesolve()
                a <<- b 
                i <<- NULL 
        }
        get <- function() a #retrieve a from the parent environment of makeCacheMatrix()
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the matrix that is the inverse of "a" specified in makeCacheMatrix

cacheSolve <- function(a, ...) { ## Return a matrix that is the inverse of 'a'
        i <- a$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- a$get()
        i <- solve(data, ...)
        a$setinverse(i)
        i
}



