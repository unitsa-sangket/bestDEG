# DESeq2 pipeline for bestDEG
# Created by: Prasert Yodsawat
# Date created: 9 August 2020


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

# Input
## construct the DESeq dataset
dds <- DESeq2::DESeqDataSetFromMatrix(countData = cts,
                                      colData = coldata,
                                      design = ~ condition)

# Pre-filtering
## keep only rows that have at least 10 reads in total
keep <- rowSums(DESeq2::counts(dds)) >= 10
dds <- dds[keep,]

# Note on factor levels
## Specify the reference level (e.g. which level represents the control group)
dds$condition <- relevel(dds$condition, ref = ref)

# Differential expression analysis
## Run DE analysis without parallel supported
dds <- DESeq2::DESeq(dds, quiet = TRUE)

## Export the DEG result to dataframe
res <- as.data.frame(DESeq2::results(dds))

# Apply threshold to DEG result
## filter the DEG result by log2foldchange and FDR
res_fil <- dplyr::filter(res, abs(log2FoldChange) >= l2fc, padj < fdr)
## sort the DEG result by FDR
res_fil_sort <- dplyr::arrange(res_fil, padj)
