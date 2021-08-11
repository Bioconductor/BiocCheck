a <<- 1
# this is a really long line with many characters in it. too many characters! So many that a NOTE is triggered.
#ooh, this has a tab here:  that is so terrible!

# the following line is terrible because it is indented by
     # something other than a multiple of 5 spaces!
is(a) == "numeric"
# good code (sort of)
is(a, "numeric")

is(a) != "character"
class(a) == "character"
class(a) != "character"
