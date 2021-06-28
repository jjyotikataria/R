## Input file format

Gene_Name	  Ex316-0HR-1	 Ex316-0HR-2	Ex316-12HR-1	Ex316-12HR-2	Ex316-6HR-1
Lgals9	    0.502858	   0.473554	    0.84294	      0.852293	    2.45062
Irx2	      2.28513	     3.06269	    2.86422	      3.39616	      0.960061

-----------------------------------------------------
b=read.table("norm1.txt",sep="\t",header=T)

row.names(b) <- b$Gene_Name

b$Gene_Name <- NULL

b_t <- data.frame(t(as.matrix(b)))

b_log <- data.frame(log2(as.matrix(b_t)+1))

b_reshape <- melt(b_log)

b_reshape$variable <- factor(b_reshape$variable, levels=unique(b_reshape$variable))

colnames(b_reshape) <- c("Genes", "Expression")

jpeg("Time0_vs_11day-tp_vs_others_Normalized_Counts.jpeg",width=3000,height=2000,quality=500,res=400)

ggplot(data = b_reshape, aes(Genes, Expression)) + geom_boxplot() + geom_point() + scale_x_discrete(labels = function(x) str_wrap(x, width = 1)) + ggtitle("Time0 vs 11 day vs others") + ylab("log2(FPKM)") + xlab("Common genes") + theme(text = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2, size = 6)) + theme(plot.title = element_text(hjust = 0.5))
dev.off()
