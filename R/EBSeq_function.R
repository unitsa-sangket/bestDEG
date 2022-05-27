# EBSeq function for bestDEG
# Created by: Prasert Yodsawat
# Date created: 2 February 2021

run_EBSeq <- function(cts, coldata, ref, l2fc, fdr) {
  # ---- Add the DEG pipeline here ---- #
  
  
  # Modified input data
  ## modified sample information column name
  colnames(coldata) <- "condition"
  
  # Input
  ## modified the order of level in coldata$condition to specify reference level
  if (ref == unique(as.vector(coldata$condition))[1]) {
    level_order <- c(unique(as.vector(coldata$condition))[2], ref)
  } else{
    level_order <- c(unique(as.vector(coldata$condition))[1], ref)
  }
  coldata$condition <- factor(coldata$condition, levels = level_order)
  
  ## Define sample condition
  Conditions <- coldata$condition
  
  # Normalization
  ## Obtained library size factor from raw count data
  ## (reproduces the median normalization approach in DESeq)
  Sizes <- EBSeq::MedianNorm(Data = cts)
  
  # Differential expression
  ## Run the EM algorithm + iterations
  EBOut <- EBSeq::EBTest(
    Data = as.matrix(cts),
    Conditions = Conditions,
    sizeFactors = Sizes,
    maxround = 5
  )
  
  # Output
  ## Obtain FC value from gene
  genefc <- EBSeq::PostFC(EBOut)
  ## Obtain list of DE genes and the posterior probabilities of being DE
  ## while using FDR control
  EBDERes <- EBSeq::GetDEResults(EBPrelim = EBOut,
                                 FDR = fdr,
                                 Threshold_FC = 2 ^ (-l2fc)) # ****
  ## Final DE result (Log2FC filter applied)
  ### Create new result table and filter only DE genes with
  ### Log2FC above 'l2fc' value
  EBDERes_out <- tibble::rownames_to_column(.data = as.data.frame(EBDERes$PPMat))
  EBDERes_out <- dplyr::filter(.data = EBDERes_out, rowname %in% EBDERes$DEfound)
  EBDERes_out <- tibble::column_to_rownames(.data = EBDERes_out, var = "rowname")
  ### Sort by PPDE value
  EBDERes_out_sort <- dplyr::arrange(.data = EBDERes_out, dplyr::desc(PPDE))
  ### Final DE result + RealFC + Log2FC
  EBDERes_out_sort <- 
    tibble::add_column(.data = EBDERes_out_sort, 
                       RealFC = genefc$RealFC[rownames(EBDERes_out_sort)])
  EBDERes_out_sort <- tibble::add_column(.data = EBDERes_out_sort,
                                         Log2FC = log2(EBDERes_out_sort$RealFC))
  
  
  # ---- Return the result of DEG pipeline ---- #
  
  # Return DEG result (table)
  return(EBDERes_out_sort)
}
