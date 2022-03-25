
suppressMessages(library(pvca))
suppressMessages(library(Biobase))
wo_controls_meta <- meta_all[!(meta_all$File_based_name %in% controls),]
pdata <- wo_controls_meta[,c("File_based_name","Replicate_sample","Operator","Day")]
colnames(pdata) <- c("File_based_name","Sample","Operator","Day")

counts_tpm_pc_wc_log <- log2(1+counts_tpm_pc[(!colnames(counts_tpm_pc) %in% controls)])


## Creating expression set

exprs <- as.matrix(counts_tpm_pc_wc_log)
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


df <- df %>% mutate( ToHighlight = ifelse( Effect >= 0.5, "yes", "no" ) )


ggplot(df, aes(x=reorder(Label,-Effect),y=Effect, fill=ToHighlight)) + geom_bar(stat="identity") +
   theme_bw() + xlab("\nComponent") + ylab("PVCA Value\n") + theme(axis.text.x = element_text(angle = 0))  + geom_text(aes(label = round(Effect,2)), size = 3, hjust = 0.5, vjust = -0.5) + scale_fill_manual( values = c( "yes"="blue", "no"="lightblue"), guide=FALSE) + ylim(0,1)
