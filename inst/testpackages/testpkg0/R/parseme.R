function() {
}
fa = function() TRUE

# a bracketless function with <-
f2 <- function() TRUE

# a bracketed function with =
f3 = function()
{
    TRUE

}

# a bracketed function with <-
f4 <- function()
{
    TRUE
}

# an lapply with an anonymous function
lapply(LETTERS, 
    function(x) {
    print(x)



})

# a function with arguments
f5 <- function(a, b, c) {}

f6 <- function(a=1, b, c) {}

f7 <- function(x, ...) {




}