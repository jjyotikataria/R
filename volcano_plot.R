## Inputs: design matrix and contrast matrix to perform differential expression


colnames(design) <- c("ffpe1","ffpe2","ffpe5")

# Create contrast matrix for the comparisons
contrast.matrix <- makeContrasts(ffpe5 - ffpe2, levels = design)
fit <- lmFit(voom.norm.data, design)
fit <- contrasts.fit(fit, contrast.matrix)
fit <- eBayes(fit)
df <- topTable(fit, coef = "ffpe5 - ffpe2", number = nrow(voom.norm.data))
df$gene_id <- row.names(df)
row.names(df)<-NULL

## Volcano plot function

volcano_plot <- function(df,pval_filt,logFC_filt,comparison_name){
  df$Significance <- ifelse(df$P.Value<pval_filt & df$logFC>logFC_filt,"Up",
                  ifelse(df$P.Value<pval_filt & df$logFC<logFC_filt,"Down",
                  "No"))
  order_pval <- df[order(df$P.Value),]
  order_fc  <- order_pval[order(order_pval$logFC, decreasing = T),]
  p <- ggplot(order_fc, aes(logFC, -log10(adj.P.Val), text=paste("geneID: ", gene_id))) +
        geom_point(aes(col=Significance)) +
        ggtitle(paste0("Differentially Expressed Genes for ", comparison_name) ) +
        scale_color_manual(values=c("grey", "red","blue"))+
        xlab("log FC") +
        ylab("-log10 P Value")+ ylim(-4,8)+ xlim(-8,8) +theme_bw() 
  ggplotly(p, text="tooltip")
}

volcano_plot(df,0.05,0,"ffpe5 vs ffpe2")
