## Inputs: design matrix and contrast matrix to perform differential expression

## Design matrix

   FFPE1 FFPE2 FFPE5
1     1     0     0
2     1     0     0
3     1     0     0
4     1     0     0


volcano_plot <- function(df,pval_filt,logFC_filt,comparison_name){
  df$Significance <- ifelse(df$P.Value < pval_filt & df$logFC >= logFC_filt,"Up",
                     ifelse(df$P.Value < pval_filt & df$logFC <= -logFC_filt,"Down",
                     "No"))
  
  #order_pval <- df[order(df$P.Value),]
  #order_fc  <- order_pval[order(order_pval$logFC, decreasing = T),]
  #order_fc$gene_symbol <- pc_gtf$gene_name[match(order_fc$gene_id, pc_gtf$gene_id)]
  sigp5 <- df[df$P.Value<0.05,]
  sig_fc1.5 <- sigp5[sigp5$logFC >=1.5 | sigp5$logFC <= -1.5,]
  sig_fc1.5$gene_symbol <- pc_gtf$gene_name[match(sig_fc1.5$gene_id, pc_gtf$gene_id)]
  ordered_sig_fc1.5 <- sig_fc1.5[order(sig_fc1.5$logFC, decreasing =T),]
  down_reg <- ordered_sig_fc1.5[ordered_sig_fc1.5$logFC < 0,]
  up_reg <- ordered_sig_fc1.5[ordered_sig_fc1.5$logFC > 0,]
  genes_highlight <- rbind(tail(down_reg,3), head(up_reg,3))
  
  p <- ggplot(order_fc, aes(logFC, -log10(P.Value))) +
        geom_point(aes(col=Significance)) +
        ggtitle(comparison_name) +  theme_bw() +
        scale_color_manual(values=c("red", "grey","blue"))+
        #geom_text(aes(label=genes_highlight))+
        xlab("log FC") +
        ylab("-log10 P Value") + theme(plot.title = element_text(hjust=0.5, size=4))+
        geom_vline(xintercept=c(-logFC_filt, logFC_filt), col="red") +
        geom_hline(yintercept=-log10(pval_filt), col="red")+
        #ggrepel::geom_label_repel(genes_highlight)  
        geom_text(data=genes_highlight, aes(logFC, -log10(P.Value),label=gene_symbol, hjust=0.5,vjust=-1.25),  size=2) 
      
        return(p)
}

volcano_plot(df,0.05,1.5,"FFPE5 vs FFPE2")
