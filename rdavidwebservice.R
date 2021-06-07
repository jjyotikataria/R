# USAGE:  
# Rscript RDavidWebservice.V2.R Significant.Differential_Expression_Genes_DESeq2_All.with.shrinkage.csv Group1_vs_Group2


args = commandArgs(trailingOnly=TRUE)

data=read.table(args[1],sep="\t",header=TRUE,check.names=FALSE)
signi1 <- data[ which(data$pvalue < 0.05 ), ]
signi_final <- signi1[ which(signi1$log2FoldChange >= 1 | signi1$log2FoldChange <=-1  ), ]
log_ordered=(signi.final[ order( signi.final$log2FoldChange ), ] )
log_ordered_PC <- log_ordered[ which(log_ordered$Gene_Biotype == "protein_coding" ), ]
ordered_all.up <- log_ordered_PC[ which(log_ordered_PC$log2FoldChange >= 1 ), ]
ordered_all.down <- log_ordered_PC[ which(log_ordered_PC$log2FoldChange <= -1 ), ]

function_david_pathway = function (genes) {
library("RDAVIDWebService")
#david <- DAVIDWebService(email="jyoti.kataria@medgenome.com",url="https://david-d.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")

  david <- DAVIDWebService(email="jyoti.kataria@medgenome.com",url="https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
                                                          ##https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/
#genes_david1=c(genes_david)
addList(david, genes$V1,idType="ENSEMBL_GENE_ID",listName="genes_david", listType="Gene")
#setAnnotationCategories(david, c("BBID","BIOCARTA","EC_NUMBER","REACTOME_PATHWAY","KEGG_PATHWAY","PANTHER_PATHWAY","INTERPRO","PIR_TISSUE_SPECIFICITY","PIR_SUPERFAMILY","SMART","OMIM_DISEASE","PANTHER_BP_ALL","GOTERM_MF_ALL","GOTERM_CC_ALL","GOTERM_BP_ALL","SP_PIR_KEYWORDS","OFFICIAL_GENE_SYMBOL","UP_SEQ_FEATURE","PUBMED_ID"))
getAllAnnotationCategoryNames(david)
#setAnnotationCategories(david)
getFunctionalAnnotationChart(david)
}

name=args[2]
nameUp=paste(name,".Up.FunctionalAnnotationChart.txt",sep="")
nameDown=paste(name,".Down.FunctionalAnnotationChart.txt",sep="")

Up.geneid=as.data.frame(ordered_all.up$Gene_ID)
colnames(Up.geneid)="V1"
chunk <- 2999
n <- nrow(Up.geneid)
r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
d <- split(Up.geneid,r)
dd=do.call(rbind,lapply(d,function_david_pathway))
write.table(dd,file=nameUp,sep="\t",row.names=FALSE,quote=FALSE)

Down.geneid=as.data.frame(ordered_all.down$Gene_ID)
colnames(Down.geneid)="V1"
chunk <- 2999
n <- nrow(Down.geneid)
r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
d <- split(Down.geneid,r)
dd=do.call(rbind,lapply(d,function_david_pathway))
write.table(dd,file=nameDown,sep="\t",row.names=FALSE,quote=FALSE)

quit()
