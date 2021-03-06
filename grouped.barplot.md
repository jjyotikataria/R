### Input : csv
"","Cell_Type","Sample_1","Sample_2"<br>
"1","CD8 T cells",2375,4038<br>
"2","Unknown",2114,3109<br>
"3","NK cells",351,633<br>
"4","CD4 T cells",433,804<br>
"5","Macrophages",207,26<br>
"6","Neutrophils",13,<br>
"7","MDSC",7,15<br>
"8","B cells",11,200<br>
"9","Monocytes",3,7<br>
"10","Dendritic cells",5,8

### Output : grouped bar plot showing cell type proportions across samples

```{r, echo=FALSE}
library(dplyr)
library(reshape2)

x<-read.csv("cell_number.csv")
x<-x[,-1]
melted<-melt(x, id = "Cell_Type")
colnames(melted)<-c("Cell_Type","Sample","Count")

#colnames(x)[3]<-"Sample_2"
#colnames(x)[2]<-"Sample_1"

check<-as.data.frame(melted%>%group_by(Sample)%>%mutate(Percentage=paste0(round(Count/sum(Count)*100,3))))
check<-check[,-3]

name<-"cell_percentage.csv"
write.csv(check,name)

x<-read.csv("cell_percentage.csv")
x<-x[,-1]

p<-ggplot(x, aes(x = Sample, y = Percentage))+ geom_col(aes(fill =Cell_Type), width = 0.7) +
scale_fill_manual(values = cluster.colors[names(cluster.colors)%in%x$Cell_Type])

plot6<-p + theme_classic() + ggtitle("Proportion of Cell Types") +  theme(text = element_text(face="bold")) + theme(plot.title = element_text(hjust=0.2)) + theme(text = element_text(size = 5)) + theme(legend.text = element_text(size=3)) + theme(legend.key.size = unit(0.2, "cm")) + guides(fill=guide_legend(title="Cell Type"))
ggsave(plot6, file=paste0("barplot_percentage.png"), width = 12, height =10, units = "cm", dpi=300)

```

![alt text](https://github.com/jjyotikataria/R/blob/main/figures/grouped_barplot.png)


