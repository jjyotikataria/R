## Example Data
## City	            State or Province	Revenue
## Los Angeles	    CA	               27.38
## Los Angeles	    CA	               14.9
## Bremerton	    WA	               5.52
## Portland	    OR	               4.44
## Beverly Hills    CA	               14

## Create side-by-side box plots for revenues broken down by state or province. Are these distributions essentially symmetric or skewed?

data2 <- read.csv("SuperMarketTransactions.csv")
data2$State.or.Province <- as.factor(data2$State.or.Province)
data2$Revenue <- as.numeric(gsub('\\$','', data2$Revenue))
boxplot(Revenue ~ State.or.Province, data = data2)

## create side-by-side box plots of revenue for only states within the United States.

data3 <- read.csv("SuperMarketTransactions.csv")
data3$Revenue <- as.numeric(gsub('\\$','', data3$Revenue))
data3 <- data3[data2$Country =="USA",]
data3$State.or.Province <- as.factor(data3$State.or.Province)
boxplot(Revenue ~ State.or.Province, data = data3)


![](https://github.com/jjyotikataria/R/blob/main/figures/boxplot.png)


## Links to refer

## https://r-coder.com/boxplot-r/


## https://towardsdatascience.com/guide-to-dimensionality-reduction-in-single-cell-rna-seq-analysis-1d77284eed1c
