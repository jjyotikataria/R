# Replacing selected values by something else in one column of the dataframe

project_samples$Sample_type <- ifelse(project_samples$Sample_type=="1","FFPE1",
                     ifelse(project_samples$Sample_type=="2","FFPE2",
                     ifelse(project_samples$Sample_type=="5","FFPE5",as.character(project_samples$Sample_type))))

# Deleting the environment variables
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

## Table >> Df

x <- as.data.frame(table(pred.hesc$labels))

## Setting up ggplot theme

My_Theme = theme(
  axis.title.x = element_text(size = 10),
  axis.text.x = element_text(size = 8),
  axis.text.y = element_text(size = 8),
  axis.title.y = element_text(size = 10))

## Find out the intersection of two dataframes(common columns) and then remove them from another dataframe
DF <- data.frame(
  x=1:10,
  y=10:1,
  z=rep(5,10),
  a=11:20
)
drops <- c("x","z")
DF[ , !(names(DF) %in% drops)]

## Colors

## http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
