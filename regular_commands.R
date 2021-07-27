rm(list = ls())

x <- matrix(rep(2:10), 3, 3)
> x
     [,1] [,2] [,3]
[1,]    2    5    8
[2,]    3    6    9
[3,]    4    7   10

## Converting a sparse > dense > dataframe
df <- as.data.frame(as.matrix(mat))


## Dataframe > excel

library(openxlsx)
write.xlsx(df, "name.xlsx")


## Filtering a dataframe

x_filt <- x[x[2] > 3,]

## Df >> List

list <- as.vector(x[["Var1"]])   ##The first col header is Var1

## Colors

## http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
