##
## Function 1 : as per the assessssment request, create matrix and its cached inverse
## containing a list of 4 elements
##
## All the below step are as per the assessemnet question
##
makeCacheMatrix<- function(x=matrix()){        
        myinverse<-NULL                                      ##initialise inverse matrix        
        set<-function(y){                              ## set the value of the matrix                                
                x<<-y
                myinverse<<-NULL 
        }
        get <- function() x                            ## get the matrix
        setinv <- function(inverse) myinverse <<- inverse    ## setting the inverse 
        getinv <- function() myinverse                       ## getting the inverse
list(set=set, get=get, setinv=setinv, getinv=getinv)   ## return inverse matrix list        
}
##
## Function 2 : create the inverse of the matrix -- BUT, first check the cache - 
## If cache still contains the result , then do not compute but retrieve from cache
## If Cache does not contain result anymore, re-compure fro values
##
cacheSolve<- function(x, ...){
        myinverse<-x$getinv()         ##checking the cache
        if(!is.null(myinverse)){
                 message("Retrieving cached data")
                 return(myinverse)
        }
        data<-x$get()
        myinverse<-solve(data, ...)
        x$setinv(myinverse)    ## cache the inverse now and return value
        myinverse
}
##done

