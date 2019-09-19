# y <- 10 
# 
# f <- function(x) {
#   
#   y <- 2
#   
#   y^2 + g(x)
#   
# }
# 
# g <- function(x) {
#   
#   x * y
#   
# }

y <- 1
g <- function(x) {
  y <- 2
  UseMethod("g")
}
g.numeric <- function(x) y
g(10)

h <- function(x) {
  x <- 10
  UseMethod("h")
}
h.character <- function(x) paste("char", x)
h.numeric <- function(x) paste("num", x)

h("a")