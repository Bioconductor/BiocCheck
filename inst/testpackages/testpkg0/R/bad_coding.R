bad_fun <- function(){

    update.packages("base")

    sapply(letters, function(x) x)
}

pasteTest <- function() {

    message(paste0("A", "B"))
    message ( paste("A", "B"))

    warning(paste("A", "B"), "C")
    warning(paste0("A", "B"), "C")

    stop("A", paste0("B", "C"))
    stop("A", paste("B", "C"))
}

invalid_ref <- function() {

    bcheck <- BiocCheck:BiocCheck
    red <- 1
    xbluex <- 3
    bcheck <- xbluex:red

}

bad_dl <- function() {

    dataurl <- "https://raw.githubusercontent.com/file.csv"
    githurl <- "https://github.com/user/package/"
    githurl <- "https://dropbox.com/data?dl=1"
    laburl <- "https://gitlab.com/raw/master/data.Rda"
    bucketurl <- "https://bitbucket.org/test/raw/file.sav"
    download.file(dataurl, destfile = tempfile())

}

bad_install <- function(pkg) {
    BiocManager::install(pkg)
}

bad_cat <- function() {
    cat("There is a cat here")
    ## except in show methods
    setMethod("show", signature = "character", function(obj) {
        cat("Cat is allowed here")
    })
}

bad_print <- function() {
    for (i in 1:10) {
        print("test")
    }
    print("Using print instead of message")
    lapply(c("A", "B"), print)
}

bad_assignment <- function() {
    # using = assignment
    value = "there is a equals assignment operator"
}

## This should not trigger 
check_install <- function(pkg) {
    instPkgs <- installed.packages()
    pkg %in% rownames(instPkgs)
}

check_inst_pkg <- function(pkg = "getPass") {
    check_install(pkg = pkg)
}

## test functions > 50 lines are reported
really_long_function <- function() {
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  return(TRUE)
}