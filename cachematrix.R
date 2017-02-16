## Put comments here that give an overall des
## Write a short commencription of what your
## functions do
MakeCacheMatrix<-function(AyanM=matrix())
{
Inv<-NULL
set<-function(y) {
AyanM<<-y
inv<<-NULL
}
get<-function()AyanM
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		makeCacheMatrix()
        
        inv = x$getinv()
        
                if (!is.null(inv)){
                message("Obtaining cached data")
                return(inv)
        }
        
        
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        x$setinv(inv)
        
        return(inv)

}



