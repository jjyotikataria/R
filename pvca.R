
library(pvca)
library(BioBase)

wo_controls_meta <- meta_all[!(meta_all$File_based_name %in% controls),]
pdata <- wo_controls_meta[,c("File_based_name","Replicate_sample","Operator","Day")]
colnames(pdata) <- c("File_based_name","Sample","Operator","Day")
counts_tpm_pc_wc <- counts_tpm_pc[,!colnames(counts_tpm_pc) %in% controls]

## Creating expression set

exprs <- as.matrix(counts_tpm_pc_wc)
colnames(exprs) <- row.names(pdata)
phenoData <- new("AnnotatedDataFrame", data=pdata)
exampleSet <- ExpressionSet(assayData=exprs,phenoData=phenoData)

## Setting other parameters
pct_threshold <- 0.6
batch.factors <- c("Operator","Day","Sample")

## Running PVCA
pvcaObj <- pvcaBatchAssess(exampleSet, batch.factors, pct_threshold) 

x <- as.data.frame(pvcaObj$dat)
y <- as.data.frame(pvcaObj$label)
df <- cbind(t(x),y)
colnames(df) <- c("Effect","Label")


ggplot(df, aes(x=Label,y=Effect)) + geom_bar(stat="identity", fill="lightblue") +
   theme_bw() + xlab("Component") + ylab("PVCA Value") + theme(axis.text.x = element_text(angle = 45, hjust=0.5))  + geom_text(aes(label = round(Effect,2)), size = 3, hjust = 0.5, vjust = -1.5, position ="stack") 

