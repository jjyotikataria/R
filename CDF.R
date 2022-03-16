## Input : TPM values (genes vs samples)

                  EA5045837 EA5045838 EA5045839 EA5045841 EA5045842 EA5045843 EA5045844 EA5045845 EA5045846 EA5053213 EA5053214
ENSG00000164627      0.53      0.93      0.23      0.28      0.35      1.20      0.41      0.78      0.40      0.00      0.00
ENSG00000186895      0.00      0.00      0.00      0.00      0.00      0.00      0.00      0.00      0.00      0.00      0.00
ENSG00000118898      6.60     11.21     12.04      8.72      8.17      7.00      4.88      7.18      8.56      1.24      4.16
ENSG00000115525     11.61     26.13     20.84     10.31     13.24     21.72     10.78     13.50     15.15      0.42      4.13
ENSG00000183258      7.71      7.23      8.19     10.78      9.00      8.33      6.05     26.19     15.55      3.28     39.03
ENSG00000101391      7.05      9.93      8.67      7.48      7.44      7.78      5.41      9.08      7.21      2.96      1.17

## Reading tpm file
counts_tpm  <- read.delim("inputs/GRCh38ERCC-ensembl91-genes.tpm",row.names = 1)

## Filtering for protein coding genes from the gtf
gtf <- read.csv("only_genes_gtf.csv",header=TRUE)
pc_gtf <- gtf[gtf$gene_biotype=="protein_coding",]    ## 19847 genes we get protein coding from the gtf
#dim(pc_gtf)  #19847
#length(unique(pc_gtf$gene_name)) #19835
pc_genes<-unique(pc_gtf$gene_id) 


## Getting the TPM of only these protein coding genes
counts <- read.delim("/Users/katarij1/Projects/2_NASH_technical_comparison_P-20210920-0001/inputs/GRCh38ERCC-ensembl91-genes.count",row.names = 1)
pc_counts <- counts[rownames(counts) %in% pc_genes,]   ## 19847 (after removing duplicates, 19835)

tpm_pc <- counts_tpm[rownames(counts_tpm) %in% rownames(pc_counts), ] 

## Melting and function for CDF

melted <- melt(tpm_pc) 
melted$FFPE_BLOCKS <- meta$FFPE_BLOCKS[match(melted$variable, meta$FASTQ_BASED_ID)] 
colnames(melted) <-c("Sample","Count","FFPE_BLOCKS")

barcodes_ffpe521 <- read.delim("barcodes_ffpe5_2_1.csv", header=FALSE)
colnames(barcodes_ffpe521) <- c("Barcode","FFPE1","FFPE2","FFPE5")

## Barcodes file

    Barcode     FFPE1     FFPE2     FFPE5
1  S14-02537 EA5045806 EA5045791 EA5045830
2  S14-06283 EA5045807 EA5045792 EA5045831

## Function

cdf_plot <- function(sample_list_file){
              for(i in 1:nrow(sample_list_file)){
                    print(i)
                    sample_list <- sample_list_file[,-1]
                    samplee_list <- as.character(sample_list[i,])
                    title_plot <- as.character(sample_list_file[i,][1])
                    data <- melted[which(melted$Sample %in% samplee_list),]
                    data$Count = log(data$Count + 1)
                    p2 <- ggplot(data, aes(x=Count, colour=FFPE_BLOCKS, group=FFPE_BLOCKS)) + stat_ecdf() + theme_bw() +         
                    ggtitle(paste0("ECDF from log TPM for ",title_plot)) +  theme(plot.title = element_text(hjust=0.5)) + My_Theme
                    ggsave(p2, file = paste0("CDF_",i,"_",title_plot,".png"),
                    device = "png",
                    width = 6,
                    height = 4,
                    dpi = 250, units = "in")
                    }
}

cdf_plot(barcodes_ffpe521)

## Plotting in grid

rl = lapply(list.files(pattern="^CDF_.*png"), png::readPNG)
gl = lapply(rl, grid::rasterGrob)
gridExtra::grid.arrange(grobs=gl)

