## Generate box plot of one covariate and colour it according to groups
##       FASTQ_BASED_ID   FFPE_BLOCKS Total.Reads
## 1      Sample1         FFPE1   135996278
## 2      Sample2         FFPE2   122532512
## 3      Sample3         FFPE1   141983388
## 4      Sample4         FFPE2   142714614
## 5      Sample5         FFPE3   124827484
## 6      Sample6         FFPE3   112468616

data <- meta_all[,c("FASTQ_BASED_ID","FFPE_BLOCKS","Total.Reads")]

q1 <- ggboxplot(data, x = "FFPE_BLOCKS", y = "Total.Reads", 
          color = "FFPE_BLOCKS", palette = c("#00AFBB", "#E7B800","#FC4E07","#BB3099","#EE0099","darkgreen"), add="jitter",
          order = c("FFPE3", "FFPE2", "FFPE1","Control1","Control2","Control3"),
          ylab = "Total Reads in Millions", xlab = "Sample Type") +
          scale_y_continuous(labels = label_number(suffix=" M", scale= 1e-6))+
          geom_hline(yintercept=mean(data$Total.Reads),  linetype = 2, colour = "blue") 

data_minus_control <- data[!data$FFPE_BLOCKS %in% controls,]

q2 <- ggboxplot(data_minus_control, x = "FFPE_BLOCKS", y = "Total.Reads", 
          color = "FFPE_BLOCKS", palette = c("#00AFBB", "#E7B800", "#FC4E07"), add="jitter",
          order = c("FFPE5", "FFPE2", "FFPE1"),
          ylab = "Total Reads in Millions", xlab = "Sample Type") +
          scale_y_continuous(labels = label_number(suffix=" M", scale= 1e-6))+
          geom_hline(yintercept=mean(data_minus_control$Total.Reads), linetype = 2, colour = "blue") 

ggarrange(q1, q2, 
          ncol = 2, nrow = 1)
