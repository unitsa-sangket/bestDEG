# DESeq2 function for bestDEG
# Created by: Prasert Yodsawat
# Date created: 17 August 2020

run_DESeq2 <- function(cts, coldata, ref, l2fc, fdr) {
  # ---- Add the DEG pipeline here ---- #
  
  
  # Modified input data
  ## modified sample information column name
  colnames(coldata) <- "condition"
  
  # Input
  ## construct a DESeq dataset
  dds <- DESeq2::DESeqDataSetFromMatrix(
    countData = round(cts),
    colData = coldata,
    design = ~ condition
  )
  
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
  
  
  # ---- Return the result of DEG pipeline ---- #
  
  # Return DEG result (table)
  return(res_fil_sort)
}
