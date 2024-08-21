## Programming Assignment 2

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y       # Asignar la nueva matriz
    m <<- NULL  # Reiniciar la inversa en caché porque la matriz cambió
  }
  # Función para obtener el valor de la matriz
  get <- function() x
  # Función para establecer la inversa de la matriz y almacenarla en la caché
  setInverse <- function(inverse) m <<- inverse
  # Función para obtener la inversa en caché y devolverla en caché
  getInverse <- function() m
  # Devolver una lista de las funciones anteriores para interactuar con la matriz y su inversa
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  # Si la inversa ya está en caché, devolverla con un mensaje
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Si la inversa no está en caché, calcularla y almacenarla
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
