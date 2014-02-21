baddep <- function()
{
    isValidJSON('{"foo": "bar"}')
}

iambad <- function()
{
    df <- data.frame(colone=1,coltwo=2)
    with(df, colone)
}