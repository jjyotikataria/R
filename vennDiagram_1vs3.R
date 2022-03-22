data <- as.data.frame(voom.norm.data)
data<-tibble::rownames_to_column(data,"gene_id")

venn_diagram <- function(data,ref,x1,y1,z1){
  
  q1_ref<-quantile(data[,ref],0.25)
  q2_ref<-quantile(data[,ref],0.50)
  q3_ref<-quantile(data[,ref],0.75)

  q1_x1<-quantile(data[,x1],0.25)
  q2_x1<-quantile(data[,x1],0.50)
  q3_x1<-quantile(data[,x1],0.75)
  
  q1_y1<-quantile(data[,y1],0.25)
  q2_y1<-quantile(data[,y1],0.50)
  q3_y1<-quantile(data[,y1],0.75)
  
  q1_z1<-quantile(data[,z1],0.25)
  q2_z1<-quantile(data[,z1],0.50)
  q3_z1<-quantile(data[,z1],0.75)
  
  ref_q1_genes <-filter(data,data[,ref]<=q1_ref)$gene_id
  ref_q2_genes <-filter(data,data[,ref]>q1_ref,data[,ref]<=q2_ref)$gene_id
  ref_q3_genes <-filter(data,data[,ref]>q2_ref,data[,ref]<=q3_ref)$gene_id
  ref_q4_genes <-filter(data,data[,ref]>q3_ref,data[,ref]<=max(data[,ref]))$gene_id
  
  x1_q1_genes <-filter(data,data[,x1]<=q1_x1)$gene_id
  x1_q2_genes <-filter(data,data[,x1]>q1_x1,data[,x1]<=q2_x1)$gene_id
  x1_q3_genes <-filter(data,data[,x1]>q2_x1,data[,x1]<=q3_x1)$gene_id
  x1_q4_genes <-filter(data,data[,x1]>q3_x1,data[,x1]<=max(data[,x1]))$gene_id
  
  y1_q1_genes <-filter(data,data[,y1]<=q1_y1)$gene_id
  y1_q2_genes <-filter(data,data[,y1]>q1_y1,data[,y1]<=q2_y1)$gene_id
  y1_q3_genes <-filter(data,data[,y1]>q2_y1,data[,y1]<=q3_y1)$gene_id
  y1_q4_genes <-filter(data,data[,y1]>q3_y1,data[,y1]<=max(data[,y1]))$gene_id
  
  z1_q1_genes <-filter(data,data[,z1]<=q1_z1)$gene_id
  z1_q2_genes <-filter(data,data[,z1]>q1_z1,data[,z1]<=q2_z1)$gene_id
  z1_q3_genes <-filter(data,data[,z1]>q2_z1,data[,z1]<=q3_z1)$gene_id
  z1_q4_genes <-filter(data,data[,z1]>q3_z1,data[,z1]<=max(data[,z1]))$gene_id
  
  list1 <- list(ref_q1_genes,x1_q1_genes,y1_q1_genes,z1_q1_genes)
  list2 <- list(ref_q2_genes,x1_q2_genes,y1_q2_genes,z1_q2_genes)
  list3 <- list(ref_q3_genes,x1_q3_genes,y1_q3_genes,z1_q3_genes)
  list4 <- list(ref_q4_genes,x1_q4_genes,y1_q4_genes,z1_q4_genes)
  
  a1<-venn.diagram(list1, col=c("red", 'green', 'blue',"orange"), cex=0.8,alpha=0.50,filename = NULL,
                   category.names=c("Op1D1","Op1D2","Op1D3","Op2D1"),
                   print.mode=c("raw","percent"), main="Q1", cat.fontface = "bold",cat.cex=0.7)
  
  b1<-venn.diagram(list2, col=c("red", 'green', 'blue',"orange"), cex=0.8,alpha=0.50,filename = NULL,
                   category.names=c("Op1D1","Op1D2","Op1D3","Op2D1"),
                   print.mode=c("raw","percent"), main="Q2", cat.fontface = "bold",cat.cex=0.7)
    
  c1<-venn.diagram(list3, col=c("red", 'green', 'blue',"orange"), cex=0.8,alpha=0.50,filename = NULL,
                   category.names=c("Op1D1","Op1D2","Op1D3","Op2D1"),
                   print.mode=c("raw","percent"), main="Q3", cat.fontface = "bold",cat.cex=0.7)
  
  d1<-venn.diagram(list4, col=c("red", 'green', 'blue',"orange"), cex=0.8,alpha=0.50,filename = NULL,
                   category.names=c("Op1D1","Op1D2","Op1D3","Op2D1"),
                   print.mode=c("raw","percent"), main="Q4", cat.fontface = "bold", cat.cex=0.7)
  
  pushViewport(plotViewport(layout=grid.layout(2, 2)))
  pushViewport(plotViewport(layout.pos.col=1, layout.pos.row=1))
  grid.draw(a1)
  popViewport()
  pushViewport(plotViewport(layout.pos.col=2, layout.pos.row=1))
  grid.draw(b1)
  popViewport()
  pushViewport(plotViewport(layout.pos.col=1, layout.pos.row=2))
  grid.draw(c1)
  popViewport()
  pushViewport(plotViewport(layout.pos.col=2, layout.pos.row=2))
  grid.draw(d1)
  
}

```

### ML2010994  

```{r, echo=FALSE, fig.height=10, fig.width=10, warning=FALSE,message=FALSE}
venn_diagram(data,"BBRCOQ_001","BBRCOQ_024","BBRCOQ_033","BBRCOQ_042")
```
