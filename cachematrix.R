## makeCachematrix function takes input of a square invertible matrix i.e.x
## and returns a list containing a function to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of matrix
## 4. get the inverse of matrix
##cacheSolve takes the output of makeCacheMatrix() and returns the inverse of the original matrix


## Write a short comment describing this function
#This function takes a matrix as input and returns a matrix of functions that sets and caches the inverse of input matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <-NULL  #set inverse value as NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function() x   #returns the matrix x
    setinverse<-function(inverse) inv<<-inverse #set the cache 'inv' equal to the inverse of the matrix x
    getinverse<-function() inv   #return the cached inverse of x
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## This function checks if there is a cached inverse of matrix X in makecachematrix function. If so, then it returns that value, otherwise it calculates a new value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()  # assign value of inverse from makecachematrix function
        if(!is.null(inv)){   #checks if the inv is null, if not then returns the cached inverse
            message("getting cached data")
            return(inv)
        }
        data<-x$get()    #assigns the matrix X from makecachematrix
        inv<-solve(data,...)  #calculates inverse of matrix X
        x$setinverse(inv)  #assigns the inverse value to setinverse function in makecachematrix
        inv  #returns the inverse of the matrix X
}
