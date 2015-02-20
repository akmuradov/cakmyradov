## makeCacheMatrix is a function with argument x as a matrix.
## This function is holding four sub functions.
## Before the first subfunction it will clear the inverse variable

## makeCacheMatrix will take the argument and return a list which is holding
## set, get, setsolve and getsolve items.
## The items in this list are functions.

## set(y) is a function which is to be used to reset the argument of makeCacheMatrix
## by the functionality of " <<- "
## for example: if we have run the code:
## >specialmatrix<-makeCacheMatrix(mymatrix)
## and let's say we calculated the inverse with
## >cacheSolve(specialmatrix)
## if we have another matrix to be inversed, then we can make use of just the followings:
## >specialmatrix$set(MyNewMatrix)
## then re-run the following cacheSolve to inverse MyNewMatrix.
## >cacheSolve(specialmatrix)
## this is a really usefull feature to automate and inverse series of matrices

## get() is a function which is going to return the matrix to be inversed

## setsolve() is a function to set the solution (the argument of setsolve()) 
##to the variable inverse

## getsolve() is a function which is returning the inverse variable.

## All in all, makeCacheMatrix will hold the given Matrix and its inverse as a list.
## At the end, the list function is used to keep all of the features of makeCacheMatrix.
## As a result if we just run >makeCacheMatrix(mymatrix) it will return list of funtions.
## if we run >makeCacheMatrix(mymatrix)$get() , we will get mymatrix printed.
## and >makeCacheMatrix(x)$getsolve() , we will get NULL since it is not yet computed
## by cacheSolve. If we run cacheSolve we will get our solution is saved into 
## inverse variable.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
       set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setsolve <- function(solution) inverse <<- solution
        getsolve <- function() inverse
        list(set = set, 
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve is a function with x and related arguments (...)
## where x is the value of makeCacheMatrix.
## it starts by saving the value of getsolve() from makeCacheMatrix into solution
## this is going to retrieve the inverse variable wich is a solution from previous run.
## if this is the first run, then solution will remain NULL
## If statement is testing whether the solution is not NULL or NULL.
## if it is the first run, then we will continue the execution of the function
## if it is not the first run, then the solution variable (retrieved from inverse) 
##will be printed and we get a message "getting cached data"
## Otherwise, we continue to get the original matrix value from makeCacheMatrix
## with x$get() and set it into data.
## with the solve(data,...) function we set the solution (inverse of our matrix) 
## into solution variable.
## x$setsolve(inverse) will get the makeCacheMatrix() use the setsolve() function
## to save the solution into inverse varialbe.
## cacheSolve() function then ends with the solution variable printed.
 

cacheSolve <- function(x, ...) {
        solution <- x$getsolve()
        if(!is.null(solution)) {
                message("getting cached data")
                return(solution)
        }
        data <- x$get()
        solution <- solve(data, ...)
        x$setsolve(solution)
        solution
}
