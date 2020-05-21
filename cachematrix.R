## La función makeCacheMatrix crea una lista con 4 funciones que permiten tener informacion acerca de determinada matrix x
## y su inversa,de no suministrar la matrix(unico parametro de la funcion)la funcion tiene un valor por defecto.

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            ## cambia el valor de x
            set <- function(y) {
                x <<- y
                m <<- NULL
           }
           ##obtiene el valor de x
           get <- function() x
           ## cambia el valor de m(inversa de la matrix ) por el que se pase de parametro
           setinverse <- function(inverse) m <<- inverse
           ##obtiene el valor de m(inversa)
           getinverse <- function() m
           list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## cacheSolve recibe la lista que retorna makeCacheMatrix y usa sus funciones para buscar si a los parametros suministrados
## ya se les habia calculado de no ser asi, calcula la inversa y la guarda
cacheSolve <- function(x, ...) {

            m <- x$getinverse()
            if(!is.null(m)) {
                message("getting cached data")
                return(m)
            }
            data <- x$get()
            m <- solve(data,...)
            x$setinverse(m)
            m
}
