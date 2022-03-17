## Add a new column after a certain column

meta_all <- meta_all %>%
            add_column(QC.Percent.Reads.Aligned = meta_all$QC.FractionReadsAligned*100,.after="QC.FractionReadsAligned")


## Convert all matrix items into a character vector
         [,1]              [,2]             
genes_1  "ENSG00000239605" "ENSG00000273269"
genes_2  "ENSG00000128655" "ENSG00000284741"
genes_3  "ENSG00000283706" "ENSG00000206549"

duplicated_genes <- as.character(unlist(duplicated_genes))


## Remove certain rows from the dataframe matching a certain columm value in a character vector

meta_all <- meta_all[ !meta_all$Sample.ID %in% controls,]

## Setting up ggplot theme

My_Theme = theme(
  axis.title.x = element_text(size = 10, face="bold"),
  axis.text.x = element_text(size = 8, hjust=1, vjust=1),
  axis.text.y = element_text(size = 8),
  axis.title.y = element_text(size = 10, face="bold"),
  plot.title = element_text(size=11, face="bold",hjust=0.5, colour="red"))

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
