## Replace multiple patterns together from a column
combined_bbrc_meta$sample <- mgsub::mgsub(combined_bbrc_meta$sample,c("_A","_B","_C","_1","_2","_7","S-"), c("","","","","","","S"))


## Partial string matching in a column in R
ffpe_info_bbrc <- ffpe_info_bbrc[!str_detect(ffpe_info_bbrc$Var1, "Ctrl"), ]


## Merging two columns by a certain common column columnwise
df<- merge(ffpe_info_bbrc,ffpe_info_ea, by="Var1")


## Changing a factor column into numeric column
read_len$Var1<- as.numeric(as.character(read_len$Var1))


## Changing 


## Finding columns which have NA in them

colSums(is.na(meta_all))
names(which(colSums(is.na(meta_all))>0))

## Grep the columns which have "Ctrl" pattern
controls <- unique(meta_all$Sample[grepl("Ctrl",meta_all$Sample)])

## Add a new column after a certain column

meta_all <- meta_all %>%
            add_column(QC.Percent.Reads.Aligned = meta_all$QC.FractionReadsAligned*100,.after="QC.FractionReadsAligned")

## Convert data type of selected columns all together
qc_columns <- c("QC.ForwardReadCount","QC.ReverseReadCount","QC.ReverseReadCount","QC.FractionReadsAligned","QC.FractionContaminatedReads","QC.PercentDuplication","QC.FractionRibosomalBases","QC.EstimatedLibrarySize","QC.DynamicRange","QC.FractionCodingBases","QC.FractionUTRBases","QC.FractionIntronicBases","QC.FractionIntergenicBases") 

meta_all %>% mutate_at(c(qc_columns), as.numeric)

## Mutate all columns

filt_tpm <- filt_tpm %>%
    mutate(across(everything(), ~ifelse( .x > 0.5, 1,0)))

## Fetching names not in controls vector
samples <- meta_all[ !meta_all$File_based_name %in% controls,]$File_based_name


## Convert all matrix items into a character vector
         [,1]              [,2]             
genes_1  "ENSG00000239605" "ENSG00000273269"
genes_2  "ENSG00000128655" "ENSG00000284741"
genes_3  "ENSG00000283706" "ENSG00000206549"

duplicated_genes <- as.character(unlist(duplicated_genes))

## Replacing the dot in the column names by hyphen
names(pc_counts) <- gsub("\\_","-",names(pc_counts))
names(pc_counts) <- gsub("00","",names(pc_counts))
names(pc_counts) <- gsub("-0","-",names(pc_counts))

## Replacing everything before something

melted$rep<- gsub(".*_E1_","",melted$Replicate_sample)
melted$rep <- gsub(".*[A-Z][0-9]+_","",melted$rep)
melted$rep <- gsub(".*[0-9][A-Z]+_","",melted$rep)

## Match and fetch another column
scores$day_op <- meta$Operator_Day[match(row.names(scores),meta$File_based_name)]

## Remove certain rows from the dataframe matching a certain columm value in a character vector

meta_all <- meta_all[ !meta_all$Sample.ID %in% controls,]

## Setting up ggplot theme

My_Theme = theme(
  axis.title.x = element_text(size = 10, face="bold"),
  axis.text.x = element_text(size = 8, hjust=1, vjust=1),
  axis.text.y = element_text(size = 8),
  axis.title.y = element_text(size = 10, face="bold"),
  plot.title = element_text(size=11, face="bold",hjust=0.5, colour="red"))

## Change y axis labels 
scale_y_continuous(labels = label_number(suffix=" M", scale= 1e-6))  
scale_y_continuous(labels = scales::comma)
scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) + 
scale_y_continuous(labels = paste0(ylab, "M"),
                     breaks = 10^6 * ylab) +

# Replacing selected values by something else in one column of the dataframe

project_samples$Sample_type <- ifelse(project_samples$Sample_type=="1","FFPE1",
                     ifelse(project_samples$Sample_type=="2","FFPE2",
                     ifelse(project_samples$Sample_type=="5","FFPE5",as.character(project_samples$Sample_type))))

# Deleting the environment variables
rm(list = ls())

# Create a new column named ffpe here according to the selected values with case_when in the new dataframe mod
mod <- melted %>% 
  mutate(ffpe =case_when(variable == "EA5045787" | variable ==  "EA5045788" | variable ==  "EA5045789" ~ "FFPE1",
                         variable ==  "EA5045803" | variable ==  "EA5045804" | variable ==  "EA5045805" ~ "FFPE2",
                         variable ==  "EA27-605291" | variable ==  "EA27-605292" ~ "FFPE5"))

# Make the labels inclined at 45 degree
+ theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1)) 

# Make the ggtitle centred
+ theme(plot.title = element_text(hjust=0.5))

# Using RColorBrewer palette
+ scale_fill_brewer(palette = "Set2")

# Using manual colors in ggplot
colors <- c("#00AFBB", "#E7B800","#EE0099","darkgreen","#FC4E07","#BB3099")
+ scale_fill_manual(values=colors)

# Change legend size and font and position

My_legend <- theme(legend.key.height = unit(0.6, 'cm'), #change legend key height
        legend.key.width = unit(0.6, 'cm'), #change legend key width
        legend.title = element_text(size=8, face="bold"), #change legend title font size
        legend.text = element_text(size=8),
        legend.position = "right")

## Font face = ("plain", "italic", "bold", "bold.italic")



# Filtering all the column values greater than threshold
pc_counts_proc <- pc_counts %>% filter_all(all_vars(.>5))  #6598 genes


# Match function to match only intersection genes in the dataframe
pc_counts_tpm_proc <- pc_counts_proc[rownames(pc_counts_proc) %in% intersection_genes,]

x <- matrix(rep(2:10), 3, 3)
> x
     [,1] [,2] [,3]
[1,]    2    5    8
[2,]    3    6    9
[3,]    4    7   10

## Converting a sparse > dense > dataframe
df <- as.data.frame(as.matrix(mat))


## Dataframe > excel

library(openxlsx)
write.xlsx(df, "name.xlsx")


## Filtering a dataframe

x_filt <- x[x[2] > 3,]

## Df >> List

list <- as.vector(x[["Var1"]])   ##The first col header is Var1

## Table >> Df

x <- as.data.frame(table(pred.hesc$labels))

## Find out the intersection of two dataframes(common columns) and then remove them from another dataframe
DF <- data.frame(
  x=1:10,
  y=10:1,
  z=rep(5,10),
  a=11:20
)
drops <- c("x","z")
DF[ , !(names(DF) %in% drops)]

## Colors

## http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
