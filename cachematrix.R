## Put comments here that give an overall description of what your
## functions do

## Here I will create two functions ir order to cache the value of a inverse matrix. First, I will create a function
##(makeCacheMatrix) that creates a matrix object that will store a matrix x and its inverse. The second function 
##(cacheSolve) will retrieve the inverse from the cached value that is stored in the makeCacheMatrix() object's.

## Firs step: Creating the makeCacheMatrix function that creates a matrix object that will store a matrix x and its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    #create a null object that will be used by later code in the function:
    
    inv <- NULL
    
    #create a "set function" that will: Assign the input argument to the x object in the parent environment, and assign the value of NULL to the m object in the parent environment. This line of code clears any value of m that had #been cached by a prior execution of cachesolve().
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Create the get function that will get the matrix "x" from the parent environment of makeCacheMatrix.
    
    get <- function() x
    
    # Create the setinverse funtion that will set the inverse of x. 
    
    setinverse <- function(solve) inv <<- solve(x)
    
    # Create the getinverse funtion that will get the inverse from the parent environment of makeCacheMatrix.
    
    getinverse <- function() inv
    
    # Assigne each of these functions as an element within a list(), and returns it to the parent environment.
    
    list(set = set, get = get,
         
         setinverse = setinverse,
         getinverse = getinverse)
    

}


## Write a short comment describing this function

## Second step: Creating the cacheSolve function that will retrieve the inverse from the cached value that is stored in the makeCacheMatrix().

cacheSolve <- function(x, ...) {
    
    # get the matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    
    #checks to see whether the result is NULL.
    
    if(!is.null(inv)) {
        
        message("getting cached data")
        
        return(inv)
    }
    
    # If the inverse was not computed before, retrives the data and computes the inverse
    
    data <- x$get()
    
    inv <- solve(data,...)
    
    # Return a matrix that is the inverse of 'x'
    
    x$setinverse(inv)
    
    inv
    
    
}

###testing_1###

    I=c(2,5)

    II = c(1,3)

    M <- makeCacheMatrix(x=matrix(c(I,II),nrow=2,ncol=2))

    cacheSolve(M)


###testing_2###

    III = c(1,0,0)
    IV = c(2,1,0)
    V = c(3,4,1)

    M2 <- makeCacheMatrix(x=matrix(c(III,IV,V),nrow=3,ncol=3))

    cacheSolve(M2)
    

