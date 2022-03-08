## ---------------------------------------------------------------------------------------------------------------------------------------------

## Input : gtf
## Output : Csv with the column names

## "chr" "start" "end" "source" "feature"   "gene_id" "gene_name" "gene_biotype" "transcript_id"  "transcript_name"  "transcript_biotype" "width"  

## ---------------------------------------------------------------------------------------------------------------------------------------------

# Reading gtf with rtracklayer

library(dplyr)
library(rtracklayer)
gtf <- rtracklayer::import("GRCh38ERCC.ensembl91.gtf")
gtf <- as.data.frame(gtf)


cleaned_gtf<- select(gtf,c("seqnames","start","end","strand","source","type","gene_id","gene_name","gene_biotype"))
only_gene <- cleaned_gtf[cleaned_gtf$type=="gene",]
write.csv(only_gene,"genes_gtf.csv", row.names=FALSE)
#only_pc_genes <- only_gene[only_gene$gene_biotype=="protein_coding",]


#--------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(rtracklayer)
gtf <- rtracklayer::import("GRCh38ERCC.ensembl91.gtf")

final <- as.data.frame(gtf@elementMetadata@listData)
final2 <- select(final,c("source","type","gene_id","gene_name","gene_biotype","transcript_id","transcript_id","transcript_name","transcript_biotype"))

# Adding chromosome info in a separate column
chr <- as.character(gtf@seqnames)
final2$chr <- chr
colnames(final2)[2] <- "feature"

# Adding start_end from the rtracklayer object
start_end <- as.data.frame(gtf@ranges)
final <- cbind(start_end,final2)

# Arranging using dplyr select
final <- select(final,c("chr","start","end","source","feature","gene_id","gene_name","gene_biotype","transcript_id","transcript_id","transcript_name","transcript_biotype","width"))
#write.csv(final,"cleaned_gtf.csv", row.names=FALSE)

# Selecting only genes from the gtf 
only_gene <- final[final$feature=="gene",]

# Selecting pc genes from the gene list
only_pc_genes <- only_gene[only_gene$gene_biotype=="protein_coding",]
write.csv(final,"only_pc_genes_gtf.csv", row.names=FALSE)


