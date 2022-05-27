# NOISeq pipeline for bestDEG
# Created by: Prasert Yodsawat
# Date created: 8 October 2020


# Global R variable
ref <- "untreated"
l2fc <- 2
fdr <- 0.01

# Load example data
load("DEG_pipeline/example_data_pasilla.RData")

# -------------------------------------------------------------------------------


# Modified input data
## modified sample information column name
colnames(coldata) <- "condition"
## modified the order of level in coldata$condition to specify reference level
if (ref == unique(as.vector(coldata$condition))[1]) {
  level_order <- c(unique(as.vector(coldata$condition))[2], ref)
} else{
  level_order <- c(unique(as.vector(coldata$condition))[1], ref)
}
coldata$condition <- factor(coldata$condition, levels = level_order)

# Input
## Converting data into a NOISeq object
mydata <- NOISeq::readData(data = cts, factors = coldata)

# Low count filtering, Normalization, Differential expression
## NOISeqBIO (Differential expression with biological replicates)
## (including TMM normalization and CPM > 1 low count filtering)
mynoiseqbio <- NOISeq::noiseqbio(
  input = mydata,
  norm = "tmm",
  factor = colnames(coldata),
  filter = 1,
  cpm = 1,
  random.seed = 1234
)

# Apply threshold to DEG result
## filter the DEG result by log2foldchange and FDR (prob = 1 - FDR)
mynoiseqbio_fil <- dplyr::filter(mynoiseqbio@results[[1]], 
                                 abs(log2FC) >= l2fc, 
                                 prob > (1 -fdr))
## sort the DEG result by FDR
mynoiseqbio_fil_sort <- dplyr::arrange(mynoiseqbio_fil, dplyr::desc(prob))
