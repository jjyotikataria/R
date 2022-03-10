## Input Data
  FASTQ_BASED_ID             CodingBases           UTRBases                 IntronicBases               IntergenicBases
1      EA5045787               0.210184            0.170134                 0.498779                   0.120900
2      EA5045788               0.161337            0.178110                 0.532524                   0.128025
3      EA5045789               0.176268            0.175394                 0.533643                   0.114692
4      EA5045790               0.216522            0.187333                 0.474069                   0.122072
5      EA5045806               0.178195            0.163764                 0.513715                   0.144322


# ---------------------------------------------------------------------------------------------------------------------
data <- meta_all[,c("FASTQ_BASED_ID","QC.FractionCodingBases","QC.FractionUTRBases","QC.FractionIntronicBases","QC.FractionIntergenicBases")]
melted<-melt(data, id = "FASTQ_BASED_ID")
melted$FFPE_BLOCKS <- meta$FFPE_BLOCKS[match(melted$FASTQ_BASED_ID, meta$FASTQ_BASED_ID)] 
colnames(melted)<-c("Samples","Feature","Fraction","FFPE_BLOCKS")

data_ffpe5 <- melted[melted$FFPE_BLOCKS == "FFPE5",]
q1 <- ggplot(data_ffpe5, aes(x = Samples, y = Fraction, fill =Feature, text=paste("Sample:",Samples))) + 
     geom_bar(position="fill", stat="identity") +
     scale_fill_brewer(palette = "Set2")  +  theme_bw() +
     theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))  + 
     scale_y_continuous(labels = percent_format()) + 
     ggtitle("Base Distribution across FFPE5 samples") + theme(plot.title = element_text(hjust=0.5)) +
     ylab("Base Distribution Percentage") 

data_ffpe2 <- melted[melted$FFPE_BLOCKS == "FFPE2",]

q2 <- ggplot(data_ffpe2, aes(x = Samples, y = Fraction, fill =Feature, text=paste("Sample:",Samples))) + 
     geom_bar(position="fill", stat="identity") +
     scale_fill_brewer(palette = "Set2")  +  theme_bw() +
     theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))  + 
     scale_y_continuous(labels = percent_format()) + 
     ggtitle("Base Distribution across FFPE2 samples") + theme(plot.title = element_text(hjust=0.5)) +
     ylab("Base Distribution Percentage") 

data_ffpe1 <- melted[melted$FFPE_BLOCKS == "FFPE1",]

q3 <- ggplot(data_ffpe1, aes(x = Samples, y = Fraction, fill =Feature, text=paste("Sample:",Samples))) + 
     geom_bar(position="fill", stat="identity") +
     scale_fill_brewer(palette = "Set2")  +  theme_bw() +
     theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))  + 
     scale_y_continuous(labels = percent_format()) + 
     ggtitle("Base Distribution across FFPE1 samples") + theme(plot.title = element_text(hjust=0.5)) +
     ylab("Base Distribution Percentage") 

remove_y <- theme(
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.title.y = element_blank()
)

ggarrange(q1, q2 + remove_y , q3 + remove_y,
          ncol = 3, nrow = 1, common.legend = TRUE, legend="right")
