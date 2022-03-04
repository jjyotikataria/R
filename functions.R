## Scatter Correlation Plot
## Input: Dataframe

##                  EA5045843  EA5045844  EA5045845  EA5045846
## ENSG00000164627  2.820233   1.423621   2.493922   1.134235
## ENSG00000118898  5.030993   5.040754   5.576804   5.655347

corr_plot <- function(dataframe,x,y, comparison){
    p <- ggscatter(dataframe, x = x, y = y, add= "reg.line",add.params = list(color = "blue", fill = "lightgray")) + 
    stat_cor(aes(label = ..r.label..), label.x = 3) + ggtitle(comparison) + theme(plot.title = element_text(hjust = 0.5)) + 
    geom_abline( slope = 1, color="red")
    p
}

p1 <- corr_plot(data,"EA5045806","EA5045791","FFPE1 vs FFPE2")
p2 <- corr_plot(data,"EA5045830","EA5045791","FFPE5 vs FFPE2")
p3 <- corr_plot(data,"EA5045830","EA5045806","FFPE5 vs FFPE1")
ggarrange(p1, p2, p3, 
          ncol = 3, nrow = 1)
          
          
#------------------------------------------------------------------------------------------------------------------------------------------#










          
   
