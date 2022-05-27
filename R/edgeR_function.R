# edgeR function for bestDEG
# Created by: Prasert Yodsawat
# Date created: 16 October 2020

run_edgeR <- function(cts, coldata, ref, l2fc, fdr) {
  # ---- Add the DEG pipeline here ---- #
  
  
  # Modified input data
  ## modified sample information column name
  colnames(coldata) <- "group"
  
  # Input
  ## construct edgeR data class (The DGEList)
  y <- edgeR::DGEList(counts = cts, group = coldata$group)
  
  # Filtering
  ## filter out lowly expressed genes (total count per condition < 10)
  keep <- edgeR::filterByExpr(y = y, min.total.count = 10)
  y <- y[keep, , keep.lib.sizes = FALSE]
  
  ## recalculate the library sizes of the DEGlist object after the filtering
  y <- edgeR::DGEList(counts = y$counts, group = y$samples$group)
  
  # Normalization
  ## normalizes the library sizes (TMM normalization)
  y <- edgeR::calcNormFactors(object = y, method = "TMM")
  
  # Pairwise comparisons between two or more groups
  # (Differential expression analysis)
  ## estimating dispersion (common & tagwise dispersion estimation)
  y <- edgeR::estimateDisp(y = y)
  ## Testing for DE genes
  ### *** create the vector variable for define pair of group to be compare
  ## (eg. "A", "B" >>> expression of B compare to A)
  if (unique(as.vector(y$samples$group))[1] == ref) {
    pair_sample = c(ref, unique(as.vector(y$samples$group))[2])
  } else{
    pair_sample = c(ref, unique(as.vector(y$samples$group))[1])
  }
  ### testing
  et <- edgeR::exactTest(object = y, pair = pair_sample)
  ### extract all DEG results and value
  et_topTags <- edgeR::topTags(object = et,
                               n = length(rownames(cts)),
                               adjust.method = "fdr")
  
  # Apply threshold to DEG result
  ## filter the DEG result by log2foldchange and FDR
  et_fil <- dplyr::filter(et_topTags$table, abs(logFC) >= l2fc, FDR < fdr)
  ## sort the DEG result by FDR
  et_fil_sort <- dplyr::arrange(et_fil, FDR)
  
  
  # ---- Return the result of DEG pipeline ---- #
  
  # Return DEG result (table)
  return(et_fil_sort)
}
