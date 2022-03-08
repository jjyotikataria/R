## Input : voom normalized counts matrix converted into dataframe and rownames(gene ids) kept as first column
## Output : venn plots generated quarter wise for the comparisons

#---------------------------------------------------------------------------------------------------------------------

## Reading the raw counts file 
counts <- read.delim("inputs/GRCh38ERCC-ensembl91-genes.count")
pc_counts <- counts[counts$genes %in% pc_genes,]   ## 19847 pc genes

colnames(pc_counts)[2]<-"EA27-605289"
colnames(pc_counts)[3]<-"EA27-605290"
colnames(pc_counts)[4]<-"EA27-605291"
colnames(pc_counts)[5]<-"EA27-605292"

## Getting the names of the samples where FFPE BLOCK is control
controls <- c("BMS","NTC","UHRR")
samples <- meta[ !meta$FFPE_BLOCKS %in% controls,]$FASTQ_BASED_ID

# Considering only the counts of the non-control samples i.e. removing the control samples
pc_counts <- pc_counts[,c("genes",samples)]

# Pre-filtering step : Selecting only those protein coding genes with atleast five counts in atleast 10% of the samples i.e. 67 samples so 7
pc_counts_proc <- subset(pc_counts, rowSums(pc_counts[2:68] > 5) > 7)   #16838 genes

##  genes for 67 samples (76-9 controls)
rownames(pc_counts_proc) <- pc_counts_proc[,1]
pc_counts_proc <- pc_counts_proc[-1]

## Voom normalization of raw counts

## Using voom for normalization for raw counts dataframe
d0 <- DGEList(pc_counts_proc)   
d0 <- calcNormFactors(d0)     

# Creating the model matrix
target <- read.delim("target.csv",sep = ",")
group <- factor(target$group)
design <- model.matrix(~0 + group)

# Use voom to model the mean-variance relationship
voom.norm <- voom(pc_counts_proc, design, plot=TRUE)
voom.norm.data <- voom.norm$E

#-------------------------------------------------------

data <- as.data.frame(voom.norm.data)
data<-tibble::rownames_to_column(data,"gene_id")

venn_diagram <- function(data,ref,x1,y1){
  
  q1<-quantile(data[,ref],0.25)
  q2<-quantile(data[,ref],0.50)
  q3<-quantile(data[,ref],0.75)

  ref_q1<-filter(data,data[,ref]<=q1)$gene_id
  ref_q2<-filter(data,data[,ref]>q1,data[,ref]<=q2)$gene_id
  ref_q3<-filter(data,data[,ref]>q2,data[,ref]<=q3)$gene_id
  ref_q4<-filter(data,data[,ref]>q3,data[,ref]<=max(data[,ref]))$gene_id
  
  x1_q1<-filter(data,data[,x1]<=q1)$gene_id
  x1_q2<-filter(data,data[,x1]>q1,data[,x1]<=q2)$gene_id
  x1_q3<-filter(data,data[,x1]>q2,data[,x1]<=q3)$gene_id
  x1_q4<-filter(data,data[,x1]>q3,data[,x1]<=max(data[,x1]))$gene_id
  
  y1_q1<-filter(data,data[,y1]<=q1)$gene_id
  y1_q2<-filter(data,data[,y1]>q1,data[,y1]<=q2)$gene_id
  y1_q3<-filter(data,data[,y1]>q2,data[,y1]<=q3)$gene_id
  y1_q4<-filter(data,data[,y1]>q3,data[,y1]<=max(data[,y1]))$gene_id
  
  list1 <- list(ref_q1,x1_q1,y1_q1)
  list2 <- list(ref_q2,x1_q2,y1_q2)
  list3 <- list(ref_q3,x1_q3,y1_q3)
  list4 <- list(ref_q4,x1_q4,y1_q4)
  
  a1<-venn.diagram(list1, col=c("red", 'green', 'blue'), cex=0.8,alpha=0.50,filename = NULL,
                   category.names=c("FFPE5","FFPE2","FFPE1"),
                   print.mode=c("raw","percent"), main="Q1", cat.fontface = "bold",cat.cex=0.7)
  
  b1<-venn.diagram(list2, col=c("red", 'green', 'blue'), cex=0.8,alpha=0.50,filename = NULL,
                   category.names=c("FFPE5","FFPE2","FFPE1"),
                   print.mode=c("raw","percent"), main="Q2", cat.fontface = "bold",cat.cex=0.7)
    
  c1<-venn.diagram(list3, col=c("red", 'green', 'blue'), cex=0.8,alpha=0.50,filename = NULL,
                   category.names=c("FFPE5","FFPE2","FFPE1"),
                   print.mode=c("raw","percent"), main="Q3", cat.fontface = "bold",cat.cex=0.7)
  
  d1<-venn.diagram(list4, col=c("red", 'green', 'blue'), cex=0.8,alpha=0.50,filename = NULL,
                   category.names=c("FFPE5","FFPE2","FFPE1"),
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

venn_diagram(data,"EA5045830","EA5045791","EA5045806")
