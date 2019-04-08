## This function calculate the inverse of x.
makeCacheMatrix <- function(x = matrix()) {
if (nrow(x)==ncol(x) & det(x)!=0) {  #Check if matrix is invertible
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x  #Get the matrix x
        setinverse<-function() m <<-solve(x) #Calculate inverse of x
        getinverse<-function() m #Get inverse of x.
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}else{
        message("The matrix isn't invertible.")
}
}



## This function check if the inverse is in cache. Otherwise, the following is calculated

cacheSolve <- function(x, ...) {
        m<-x$getinverse
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data<-x$get
        m <- inv(data, ...)
        x$setinverse(m)
        m
}









