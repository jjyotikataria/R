## Plot the grouped bar plot showing the overlapping genes percentage in each quarter in each bar.


## Input file
Barcode,S18-17254,S17-24259,S19-05529,S18-05326,S17-19223,S17-05151,S17-03773,S14-02537,S16-20126,S19-05246,S14-06283,S15-15868,S15-16890,S15-09910,S19-08705
Q1,83.8,75.9,80.9,72.2
Q2,69.6,54.2,66.5,50.7
Q3,75.7,59.5,73.2,58
Q4,90.9,82.5,89,81.4


colors <- c("#00AFBB","#E7B800","#EE0099","darkgreen","#FC4E07","#BB3099")

data <- read.csv("concordance_percentage.csv")
melted <- melt(data)
colnames(melted) <- c("Quarter","Barcode","Value")
p1 <- ggplot(melted, aes(fill=Quarter, y=Value, x=Barcode)) + 
  geom_col(width=0.5,position=position_dodge(0.5)) + scale_fill_brewer(palette = "Set2")+
  xlab("Barcodes") + ylab("Percentage Overlap") + ggtitle("Percentage of gene overlap")+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1)) +  theme(plot.title = element_text(hjust=0.5)) + ylim(0,100) +  scale_fill_manual(values=colors)

p1

