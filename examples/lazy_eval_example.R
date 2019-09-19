#Lazy-evaluation exercises
j <- function (){
  if(!exists("a")) {
    a <- 1
  } else {
    a <- a + 1
  }
  a
  
}

j()

c <- 10
c(c=c)

f <- function(x){
  f <- function(x){
    f <- function(x){
      x ^ 2
    }
    f(x) + 1
  }
  f(x) * 2
}

f(10)
