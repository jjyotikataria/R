## ---------------------------------------------------------------------------------------------------------------------------------------------

## Input : gtf
## Output : Csv with the column names

## "chr" "start" "end" "source" "feature"   "gene_id" "gene_name" "gene_biotype" "transcript_id"  "transcript_name"  "transcript_biotype" "width"  

## ---------------------------------------------------------------------------------------------------------------------------------------------

# Reading gtf with rtracklayer

library(dplyr)
library(rtracklayer)
gtf <- rtracklayer::import("file.gtf")
final <- (as.data.frame(df@listData))
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
write.csv(final,"cleaned_gtf.csv", row.names=FALSE)
