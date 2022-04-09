gtf <- read.csv("/Users/katarij1/Projects/imp_files/only_genes_gtf.csv",header=TRUE) #dim(gtf)  58302 X 12
pc_gtf <- gtf[gtf$gene_biotype=="protein_coding",]    
pc_genes<-unique(pc_gtf$gene_id)

## FUNCTION1 - From the raw counts, removing duplicates of ensembl ids representing same gene symbol by taking mean
## INPUT : GTF & RAW_COUNTS FILE
## OUTPUT : UPDATED COUNTS FILE WITH DUPLICATED gene symbols ENSEMBL ID REMOVED

duplicate_delete_function <- function(pc_gtf_file, raw_counts_df){
  pc_genes<-unique(pc_gtf_file$gene_id)
  duplicated_gene_names <- pc_gtf_file[duplicated(pc_gtf_file$gene_name),]$gene_name
  list_ensemblids <- list()
  
  for(i in 1:length(duplicated_gene_names)){
    name <- duplicated_gene_names[i]
    tmp <- filter(pc_gtf_file,pc_gtf_file[,"gene_name"]==duplicated_gene_names[i])$gene_id
    list_ensemblids[[name]] <- tmp
    }

  duplicated_ensids <- unlist(list_ensemblids, use.names = F)
  
  avg_raw_counts <- list()
  
  for(j in 1:length(list_ensemblids)){
    dup_ens_ids <- raw_counts_df[c(list_ensemblids[[j]]),]
    dup_ens_ids[nrow(dup_ens_ids)+1,] <- colMeans(dup_ens_ids)
    name <- row.names(dup_ens_ids)[1]
    mean_row <- dup_ens_ids[-c(1,2),]
    row.names(mean_row) <- name
    avg_raw_counts[[j]] <- mean_row
    }
    
   avg_raw_counts <- do.call(rbind, avg_raw_counts)

   counts_minus_dup_ensids <- raw_counts_df[!(rownames(raw_counts_df) %in% duplicated_ensids),]
   counts_deleted_dup_added_avg <- rbind(counts_minus_dup_ensids,avg_raw_counts)  
   return(counts_deleted_dup_added_avg)
}

counts_deleted_dup_added_avg <- duplicate_delete_function(pc_gtf, counts_raw)

# Taking only protein coding genes
pc_counts <- counts_deleted_dup_added_avg[rownames(counts_deleted_dup_added_avg) %in% pc_genes,]   ## 19835 X 47 (all samples)
pc_counts_proc <- subset(pc_counts, rowSums(pc_counts > 5) > 4)  ### 16998 X 47
controls_file_name <- meta_all[meta_all$Replicate %in% controls, ]$BBRC_Id
