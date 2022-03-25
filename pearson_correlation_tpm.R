## Given an input file with replicates or comparison information
## Input 

    Sample_ID       RepA       RepB       RepC       RepD       RepE       RepF
1 ML2010994 BBRCOQ_001 BBRCOQ_002 BBRCOQ_003 BBRCOQ_024 BBRCOQ_033 BBRCOQ_042
2 ML2010999 BBRCOQ_004 BBRCOQ_005 BBRCOQ_006 BBRCOQ_025 BBRCOQ_034 BBRCOQ_043
3 ML2011001 BBRCOQ_007 BBRCOQ_008 BBRCOQ_009 BBRCOQ_026 BBRCOQ_035 BBRCOQ_044
4 13000042B BBRCOQ_010 BBRCOQ_011 BBRCOQ_012 BBRCOQ_027 BBRCOQ_036 BBRCOQ_045
5 13000053B BBRCOQ_013 BBRCOQ_014 BBRCOQ_015 BBRCOQ_028 BBRCOQ_037 BBRCOQ_046


## TPM counts

data <- log2(1+counts_tpm_pc[(!colnames(counts_tpm_pc) %in% controls)])  ## No negatives left

replicates_list <- read.delim("replicates.final.csv", header=FALSE)
colnames(replicates_list) <- c("BBRC_ID","RepA","RepB","RepC","RepD","RepE","RepF")

    
corr_plot_rep6_tpm <- function(dataframe,replicates_file,row_num){
  
                      for(i in row_num){
                          x = as.character(replicates_file[row_num,][2])
                          y = as.character(replicates_file[row_num,][3])
                          res <- CCC(data[,x],data[,y],ci="z-transform",conf.level=0.95)
                          lcoeff <- paste0("CCC: ", round(res$rho.c$est,2))
                          p1 <- ggscatter(dataframe, x = x, y = y, size=1,add= "reg.line",add.params = list(color = "blue", fill = "lightgray")) + stat_cor(aes(label = ..r.label..), label.x = 1) + ggtitle(as.character(replicates_file[i,][1])) + theme(plot.title = element_text(hjust = 0.5)) + geom_abline( slope = 1, color="red")  + annotate("text", x = 1.6, y = 10.5, label = lcoeff )
                         
                          title_plot <- paste0("\n",as.character(replicates_list[row_num,][1]),"\n")
                          
                          x = as.character(replicates_file[row_num,][3])
                          y = as.character(replicates_file[row_num,][4])
                          res <- CCC(data[,x],data[,y],ci="z-transform",conf.level=0.95)
                          lcoeff <- paste0("CCC: ", round(res$rho.c$est,2))
                          p2 <- ggscatter(dataframe, x = x, y = y, size=1, add= "reg.line",add.params = list(color = "blue", fill = "lightgray")) + stat_cor(aes(label = ..r.label..), label.x = 1) + ggtitle(as.character(replicates_day1_op1[i,][1])) + theme(plot.title = element_text(hjust = 0.5)) + geom_abline( slope = 1, color="red")  + annotate("text", x = 1.6, y = 10.5, label = lcoeff )
                          
                          
                          
                          x = as.character(replicates_file[row_num,][2])
                          y = as.character(replicates_file[row_num,][4])
                          res <- CCC(data[,x],data[,y],ci="z-transform",conf.level=0.95)
                          lcoeff <- paste0("CCC: ", round(res$rho.c$est,2))
                          p3 <- ggscatter(dataframe, x = x, y = y,size=1, add= "reg.line",add.params = list(color = "blue", fill = "lightgray")) + stat_cor(aes(label = ..r.label..), label.x = 1) + ggtitle(as.character(replicates_day1_op1[i,][1])) + theme(plot.title = element_text(hjust = 0.5)) + geom_abline( slope = 1, color="red")  + annotate("text", x = 1.6, y = 10.5, label = lcoeff )
                      
                          
                          x = as.character(replicates_file[row_num,][2])
                          y = as.character(replicates_file[row_num,][5])
                          res <- CCC(data[,x],data[,y],ci="z-transform",conf.level=0.95)
                          lcoeff <- paste0("CCC: ", round(res$rho.c$est,2))
                          p4 <- ggscatter(dataframe, x = x, y = y,size=1, add= "reg.line",add.params = list(color = "blue", fill = "lightgray")) + stat_cor(aes(label = ..r.label..), label.x = 1) + ggtitle(as.character(replicates_day1_op1[i,][1])) + theme(plot.title = element_text(hjust = 0.5)) + geom_abline( slope = 1, color="red")  + annotate("text", x = 1.6, y = 10.5, label = lcoeff )
                           
                          
                          
                          x = as.character(replicates_file[row_num,][2])
                          y = as.character(replicates_file[row_num,][6])
                          res <- CCC(data[,x],data[,y],ci="z-transform",conf.level=0.95)
                          lcoeff <- paste0("CCC: ", round(res$rho.c$est,2))
                          p5 <- ggscatter(dataframe, x = x, y = y,size=1, add= "reg.line",add.params = list(color = "blue", fill = "lightgray")) + stat_cor(aes(label = ..r.label..), label.x = 1) + ggtitle(as.character(replicates_day1_op1[i,][1])) + theme(plot.title = element_text(hjust = 0.5)) + geom_abline( slope = 1, color="red")  + annotate("text", x = 1.6, y = 10.5, label = lcoeff )
                          
                          
                          x = as.character(replicates_file[row_num,][2])
                          y = as.character(replicates_file[row_num,][7])
                          res <- CCC(data[,x],data[,y],ci="z-transform",conf.level=0.95)
                          lcoeff <- paste0("CCC: ", round(res$rho.c$est,2))
                          p6 <- ggscatter(dataframe, x = x, y = y,size=1, add= "reg.line",add.params = list(color = "blue", fill = "lightgray")) + stat_cor(aes(label = ..r.label..), label.x = 1) + ggtitle(as.character(replicates_day1_op1[i,][1])) + theme(plot.title = element_text(hjust = 0.5)) + geom_abline( slope = 1, color="red")  + annotate("text", x = 1.6, y = 10.5, label = lcoeff )
                          
                                   
                      }
  
       plot <- ggarrange(p1, p2, p3, p4, p5,p6, ncol = 3, nrow = 2)
       return(annotate_figure(plot,top = text_grob(title_plot, color = "red", face = "bold", size = 14)))

}

p1 <- corr_plot_rep6_tpm(data,replicates_list,1)
p2 <- corr_plot_rep6_tpm(data,replicates_list,2)
p3 <- corr_plot_rep6_tpm(data,replicates_list,3)
p4 <- corr_plot_rep6_tpm(data,replicates_list,4)
p5 <- corr_plot_rep6_tpm(data,replicates_list,5)
p6 <- corr_plot_rep6_tpm(data,replicates_list,6)
p7 <- corr_plot_rep6_tpm(data,replicates_list,7)

p1
p2
p3
p4
p5
p6
p7
