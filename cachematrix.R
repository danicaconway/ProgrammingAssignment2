## R Programming Assignment #2:  Caching the Inverse of a Matrix
## Author:  Danica Conway
## Last Update: 6/5/2016


## makeCacheMatrix creates matrix object that can cache its inverse
## Defines get, setsolve, getsolve and creates a list containing
## the functions

makeCacheMatrix <- function(x = matrix()) 
{

    
    m<-NULL
    set<-function(y)  ## sets original matrix
    {
        x<<-y
        m<<-NULL
    }
    get<-function() x  ## gets original matrix
    setsolve<-function(solve) m<<-solve  ## sets cache value to inverse matrix
    getsolve<-function() m   ## gets the inverse matrix
    list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)    
}

## cacheSolve inverts matrix x, then return it if m is NULL
## If m is not NULL, returns m

cacheSolve <- function(x, ...) 
{

    
    m<-x$getsolve()  ## gets m
    if(!is.null(m))  ## if not null, returns m
    {
         message("getting cached data")
         return(m)
    }
    data<-x$get()  ## gets x & sets as 'data'
    m<-solve(data,...)  ##  Solves for x ('data')
    x$setsolve(m)  ## sets cache value to inverse matrix 
}
