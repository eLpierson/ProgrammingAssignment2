## Assignment: 
    ## Caching the Inverse of a Matrix. (2 functions)

## Objective: 
    ## Understand & utilize lexical scoping rules
    ## for caching the values of potentially 
    ## costly (time-consuming) computations.

## Explanation: 
    ## In situations where an expensive/costly computed value
    ## may be needed again in a later process and is derived/calculated 
    ## from a content population that is unlikely to change
    ## (i.e. the contents of a vector or matrix), preserving
    ## the state of this value inside an R object can be benefitial
    ## and is accommplished by taking advantage of the capability 
    ## to manipulate R's scoping rules.

## Matrix Inversion Knowns:
    ## Matrix inversion is usually costly, especially
    ## when repeated computation is necessary (i.e. inside a loop).
    ## There are known alternatives to matrix inversion, but
    ## they are outside of the scope of this assignment.

## Assumptions:
    ## For the purposes of this assignment, we are to assume
    ## that any matrix supplied for testing these functions
    ## is always a square invertible matrix. Computing the inverse 
    ## of a square matrix can be done with R's solve function, where
    ## solve(X) returns the inverse of square matrix 'X'.

##----------Functions----------##

## Function makeCacheMatrix()
    ## This creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## main function
    m <- NULL
    set <- function(y) { ## changes main func matrix (only use when need change)
        x <<- y ## << substitutes main func input (< would only sub in set func)
        m <<- NULL ## restores inverse value to null for cacheSolve to calc
    }
    get <- function() x ## returns matrix x stored in main function
    setInverse <- function(solve) m <<- solve ## stores input in var m (no calc)
    getInverse <- function() m## stores input value in main func var m (no calc)
    list(set = set, get = get,   ## stores list of 4 functions
         setInverse = setInverse,## use by subsetting main function
         getInverse = getInverse)## call ex. mainfunc+"$"+subfunc+(arguments)
}

## Function cacheSolve()
    ## This computes the inverse of the special "matrix" returned by 
    ## the makeCacheMatrix function. 
    ## 1. Checks if the inverse has been calculated.
    ## 2. If so, retrieves the inverse value from the cache 
    ##    and skips computation.
    ## 3. If not, calculates inverse of data.
    ## 4. After calculations (if necessary), sets value of inverse in cache
    ##    via setInverse() function by calling x$setInverse(m) to store
    ##    the inverse in the object generated assigned with makeCacheMatrix.

cacheSolve <- function(x, ...) { ## main function 
    m <- x$getInverse() ## input is object where makeCacheMatrix is stored
    if(!is.null(m)) { ## verifies getInverse value m exists & is not null
        message("getting cached data") ## returns message
        return(m) ## matrix inverse from makeCacheMatrix m value
    } 
    data <- x$get() ## data gets makeCacheMatrix stored matrix
    m <- solve(data, ...) ## calculates matrix inverse
    x$setInverse(m) ## stores matrix inverse value
    m ## returns matrix inverse
}

##----------Testing----------##

## 1. Create a variable and assign makeCacheMatrix
    ## z <- makeCacheMatrix()
    ## x is now a list of functions

## 2. Create a variable and assign it a matrix
    ## d <- matrix(1:4,2,2)

## 3. Use the variable z to set the matrix
    ## z$set(d)
    ## Now the variable, x, defined in the makeCacheMatrix function has the value      
    ##      [,1] [,2]
    ## [1,]    1    3
    ## [2,]    2    4
    ## Check with z$get()

## 4. Use z$setInverse to set variable m defined in makeCacheMatrix to solve()
    ## z$setInverse(solve(d))

## 5. Use z$getInverse() to get inverse m inside makeCacheMatrix z$getInverse()
    ## This should return:
    ##      [,1] [,2]
    ## [1,]   -2  1.5
    ## [2,]    1 -0.5

## 6. Use cacheSolve to get m defined in makeCacheMatrix by passing it z
    ## cacheSolve(z)
    ## This should return:
    ## message 'getting cached data'
    ##      [,1] [,2]
    ## [1,]   -2  1.5
    ## [2,]    1 -0.5
                                        
## 7. Set a new different matrix
    ## d <- matrix(5:8,2,2)
    ## z$set(d)
    ## z$getInverse() 
        ## This returns NULL because when z$set stored d it also set m = NULL.
                                        
## 8. Get the inverse using the cacheSolve function cacheSolve(z)
    ## THis calculated a new inverse, and should return: 
    ##      [,1] [,2]
    ## [1,]   -4  3.5
    ## [2,]    3 -2.5
                                        
## 9. Try z$getInverse() to ensure it returns the same matrix inverse as step 8.
    ## cacheSolve stored the value in m using z$setInverse(m) using m<<-solve(m)