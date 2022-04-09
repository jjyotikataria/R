## Taking those Ensembl gene ids who has same gene symbols and then take the mean of them and replace the gene id with one of the Ensembl Ids

# Fetching those gene names with more than one Ensembl Id
duplicated_gene_names <- pc_gtf[duplicated(pc_gtf$gene_name),]$gene_name

duplicated_gene_names
[1] "STPG4"    "PDE11A"   "PRSS50"   "TXNRD3NB" "ARL14EPL" "MATR3"    "RABGEF1"  "TMSB15B"  "EMG1"     "COG8"     "SCO2"    
[12] "H2BFS" 

# Fetching those rows with those duplicated gene symbols one by one 
genes_1<- filter(pc_gtf,pc_gtf[,"gene_name"]==duplicated_gene_names[1])$gene_id
genes_2<- filter(pc_gtf,pc_gtf[,"gene_name"]==duplicated_gene_names[2])$gene_id
genes_3<- filter(pc_gtf,pc_gtf[,"gene_name"]==duplicated_gene_names[3])$gene_id
genes_4<- filter(pc_gtf,pc_gtf[,"gene_name"]==duplicated_gene_names[4])$gene_id
genes_5<- filter(pc_gtf,pc_gtf[,"gene_name"]==duplicated_gene_names[5])$gene_id
genes_6<- filter(pc_gtf,pc_gtf[,"gene_name"]==duplicated_gene_names[6])$gene_id
genes_7<- filter(pc_gtf,pc_gtf[,"gene_name"]==duplicated_gene_names[7])$gene_id
genes_8<- filter(pc_gtf,pc_gtf[,"gene_name"]==duplicated_gene_names[8])$gene_id
genes_9<- filter(pc_gtf,pc_gtf[,"gene_name"]==duplicated_gene_names[9])$gene_id
genes_10<- filter(pc_gtf,pc_gtf[,"gene_name"]==duplicated_gene_names[10])$gene_id
genes_11<- filter(pc_gtf,pc_gtf[,"gene_name"]==duplicated_gene_names[11])$gene_id
genes_12<- filter(pc_gtf,pc_gtf[,"gene_name"]==duplicated_gene_names[12])$gene_id

genes_1
[1] "ENSG00000239605" "ENSG00000273269"


duplicated_genes <- rbind(genes_1,genes_2,genes_3,genes_4,genes_5,genes_6,genes_7,genes_8,genes_9,genes_10,genes_11,genes_12)

        [,1]              [,2]             
genes_1  "ENSG00000239605" "ENSG00000273269"
genes_2  "ENSG00000128655" "ENSG00000284741"
genes_3  "ENSG00000283706" "ENSG00000206549"

duplicated_genes <- as.character(unlist(duplicated_genes))

[1] "ENSG00000239605" "ENSG00000128655" "ENSG00000283706" "ENSG00000283374" "ENSG00000268714" "ENSG00000280987" "ENSG00000284461"
 [8] "ENSG00000158427" "ENSG00000126749" "ENSG00000213380" "ENSG00000284194" "ENSG00000274559" "ENSG00000273269" "ENSG00000284741"
[15] "ENSG00000206549" "ENSG00000206483" "ENSG00000268223" "ENSG00000015479" "ENSG00000154710" "ENSG00000269226" "ENSG00000268439"
[22] "ENSG00000272617" "ENSG00000130489" "ENSG00000234289"


## Function explanation: Fetching those ensembl id rows which have the same gene symbol in a df, adding a third row with the mean counts, replacing the row name with the first row ensembl id and then deleting the first and second row of the df. 

average_gene_ids <- function(counts_df, genes_list){
  x1<-counts_df[c(genes_list),]
  x1[nrow(x1)+1,] <- colMeans(x1)
  name<-row.names(x1)[1]
  y1<-x1[-c(1,2),]
  row.names(y1) <- name
  return(y1)
}

p1<- average_gene_ids(counts,genes_1)
p2<- average_gene_ids(counts,genes_2)
p3<- average_gene_ids(counts,genes_3)
p4<- average_gene_ids(counts,genes_4)
p5<- average_gene_ids(counts,genes_5)
p6<- average_gene_ids(counts,genes_6)
p7<- average_gene_ids(counts,genes_7)
p8<- average_gene_ids(counts,genes_8)
p9<- average_gene_ids(counts,genes_9)
p10<- average_gene_ids(counts,genes_10)
p11<- average_gene_ids(counts,genes_11)
p12<- average_gene_ids(counts,genes_12)

mean_counts <- rbind(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12)   

## Dropping the gene ids which were having same gene names and adding the average of both by taking one ensembl id as representative of the gene symbol(gene name)                              #counts 58394
counts_minus_ensids <- counts[!(rownames(counts) %in% duplicated_genes),]
counts_mean_added <- rbind(counts_minus_ensids,mean_counts)     
#dim(counts)

