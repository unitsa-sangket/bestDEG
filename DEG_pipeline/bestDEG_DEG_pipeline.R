# bestDEG DEG pipeline for bestDEG
# Created by: Prasert Yodsawat
# Date created: 14 October 2020


# Global R variable
internal <- list()
input <- list()
input$ref_select <- "untreated"
input$l2fc <- 2
input$fdr <- 0.01
input$DEG_methods = c("edgeR", "DESeq2", "NOISeq", "EBSeq")

# Load example data
internal$count_data <- read.table(file = "cts.tsv", header = TRUE, row.names = 1)
internal$sample_information <- read.table(file = "coldata.tsv", header = TRUE, row.names = 1, stringsAsFactors = TRUE)

# Source functions
source(file = "R/edgeR_function.R", local = TRUE)
source(file = "R/DESeq2_function.R", local = TRUE)
source(file = "R/NOISeq_function.R", local = TRUE)
source(file = "R/EBSeq_function.R", local = TRUE)

# ----------------------------------------------------------------------------------

# Modified the column order of sample with rownames of sample information
cat("Prepare input\n")
internal$count_data <- internal$count_data[, rownames(internal$sample_information)]


# Run DEG analysis
cat("Run DEG analysis\n")
## Create empty list for store DEG results
result_all <- list()
## Check the input DEG methods to use, run DEG analysis, mark "Up/Down" and store the results to "result_all" list
if ("edgeR" %in% input$DEG_methods) {
  cat("- edgeR\n")
  result_all$result_edgeR <- run_edgeR(cts = internal$count_data, 
                                       coldata = internal$sample_information, 
                                       ref = input$ref_select, 
                                       l2fc = input$l2fc, 
                                       fdr = input$fdr)
  ### Mark "Up/Down" DEG
  result_all$result_edgeR <- tibble::add_column(result_all$result_edgeR, Regulate = dplyr::if_else(result_all$result_edgeR$logFC > 0, true = "Up", false = dplyr::if_else(result_all$result_edgeR$logFC < 0, true = "Down", false = "Zero_value")))
}
if ("DESeq2" %in% input$DEG_methods) {
  cat("- DESeq2\n")
  result_all$result_DESeq2 <- run_DESeq2(cts = internal$count_data, 
                                         coldata = internal$sample_information, 
                                         ref = input$ref_select, 
                                         l2fc = input$l2fc, 
                                         fdr = input$fdr)
  ### Mark "Up/Down" DEG
  result_all$result_DESeq2 <- tibble::add_column(result_all$result_DESeq2, Regulate = dplyr::if_else(result_all$result_DESeq2$log2FoldChange > 0, true = "Up", false = dplyr::if_else(result_all$result_DESeq2$log2FoldChange < 0, true = "Down", false = "Zero_value")))
}
if ("NOISeq" %in% input$DEG_methods) {
  cat("- NOISeq\n")
  result_all$result_NOISeq <- run_NOISeq(cts = internal$count_data, 
                                         coldata = internal$sample_information, 
                                         ref = input$ref_select, 
                                         l2fc = input$l2fc, 
                                         fdr = input$fdr)
  ### Mark "Up/Down" DEG
  result_all$result_NOISeq <- tibble::add_column(result_all$result_NOISeq, Regulate = dplyr::if_else(result_all$result_NOISeq$log2FC > 0, true = "Up", false = dplyr::if_else(result_all$result_NOISeq$log2FC < 0, true = "Down", false = "Zero_value")))
}
if ("EBSeq" %in% input$DEG_methods) {
  cat("- EBSeq\n")
  result_all$result_EBSeq <- run_EBSeq(cts = internal$count_data, 
                                       coldata = internal$sample_information, 
                                       ref = input$ref_select, 
                                       l2fc = input$l2fc, 
                                       fdr = input$fdr)
  ### Mark "Up/Down" DEG
  result_all$result_EBSeq <- tibble::add_column(result_all$result_EBSeq, Regulate = dplyr::if_else(result_all$result_EBSeq$Log2FC > 0, true = "Up", false = dplyr::if_else(result_all$result_EBSeq$Log2FC < 0, true = "Down", false = "Zero_value")))
}


#

# Find intersection genes
cat("Find intersection genes\n")
## select rownames of all DEG results
if ("edgeR" %in% input$DEG_methods) {
  result_all$DEG_gene$edgeR <- dplyr::pull(tibble::rownames_to_column(.data = result_all$result_edgeR), rowname)
}
if ("DESeq2" %in% input$DEG_methods) {
  result_all$DEG_gene$DESeq2 <- dplyr::pull(tibble::rownames_to_column(.data = result_all$result_DESeq2), rowname)
}
if ("NOISeq" %in% input$DEG_methods) {
  result_all$DEG_gene$NOISeq <- dplyr::pull(tibble::rownames_to_column(.data = result_all$result_NOISeq), rowname)
}
if ("EBSeq" %in% input$DEG_methods) {
  result_all$DEG_gene$EBSeq <- dplyr::pull(tibble::rownames_to_column(.data = result_all$result_EBSeq), rowname)
}
## find intersection
intersec <- gplots::venn(data = result_all$DEG_gene, show.plot = FALSE, intersections = TRUE)
intersec <- attributes(intersec)
intersec_method <- paste(input$DEG_methods, collapse = ":")
result_all$intersection <- unname(intersec$intersections[intersec_method])
result_all$intersection <- result_all$intersection[[1]]


#

# Create table of intersection result with statistics value and DEG regulate label
## Extract the "log2fc", "fdr" ,and "regulate" value of intersection gene from each DEG result
result_all$raw_intersection_table <- data.frame(intersection = result_all$intersection)
if ("edgeR" %in% input$DEG_methods) {
  result_all$raw_intersection_table <- dplyr::bind_cols(result_all$raw_intersection_table, 
                                                        dplyr::select(.data = result_all$result_edgeR[result_all$intersection,], edgeR_log2FC = "logFC"), 
                                                        dplyr::select(.data = result_all$result_edgeR[result_all$intersection,], edgeR_fdr = "FDR"),
                                                        dplyr::select(.data = result_all$result_edgeR[result_all$intersection,], edgeR_regulate = "Regulate")
  )
}
if ("DESeq2" %in% input$DEG_methods) {
  result_all$raw_intersection_table <- dplyr::bind_cols(result_all$raw_intersection_table, 
                                                        dplyr::select(.data = result_all$result_DESeq2[result_all$intersection,], DESeq2_log2FC = "log2FoldChange"), 
                                                        dplyr::select(.data = result_all$result_DESeq2[result_all$intersection,], DESeq2_fdr = "padj"),
                                                        dplyr::select(.data = result_all$result_DESeq2[result_all$intersection,], DESeq2_regulate = "Regulate")
  )
}
if ("NOISeq" %in% input$DEG_methods) {
  result_all$raw_intersection_table <- dplyr::bind_cols(result_all$raw_intersection_table, 
                                                        dplyr::select(.data = result_all$result_NOISeq[result_all$intersection,], NOISeq_log2FC = "log2FC"), 
                                                        dplyr::select(.data = result_all$result_NOISeq[result_all$intersection,], NOISeq_prob = "prob"),
                                                        dplyr::select(.data = result_all$result_NOISeq[result_all$intersection,], NOISeq_regulate = "Regulate")
  )
}
if ("EBSeq" %in% input$DEG_methods) {
  result_all$raw_intersection_table <- dplyr::bind_cols(result_all$raw_intersection_table, 
                                                        dplyr::select(.data = result_all$result_EBSeq[result_all$intersection,], EBSeq_log2FC = "Log2FC"), 
                                                        dplyr::select(.data = result_all$result_EBSeq[result_all$intersection,], EBSeq_PPDE = "PPDE"),
                                                        dplyr::select(.data = result_all$result_EBSeq[result_all$intersection,], EBSeq_regulate = "Regulate")
  )
}

## Create final data table
result_all$intersection_table <- data.frame(result_all$raw_intersection_table$intersection)
colnames(result_all$intersection_table) <- c("GENE_NAME")

### Check for "Up/Down" regulation consistent among results from each DEG methods in intersection genes
DEG_regulate <- dplyr::select(result_all$raw_intersection_table, dplyr::contains(match = "_regulate"))
DEG_regulate_status <- NULL
for (i in 1:length(rownames(DEG_regulate))) {
  if (!all(as.character(DEG_regulate[i,])[1] == as.character(DEG_regulate[i,]))) {
    cat("Inconsistent DEG regulation status at row:", i, "of variable \"result_all$raw_intersection_table\"\n")
    DEG_regulate_status[i] <- "Inconsistent"
  }else{
    DEG_regulate_status[i] <- as.character(DEG_regulate[i,])[1]
  }
}
result_all$intersection_table <- dplyr::bind_cols(result_all$intersection_table, DEG_REGULATE = DEG_regulate_status)

### Calculate 'Mean' and 'SD'
cal_mean_sd <- dplyr::select(result_all$raw_intersection_table, dplyr::contains("_log2FC"))
result_all$intersection_table <- dplyr::bind_cols(result_all$intersection_table, 
                                                  MEAN_LOG2FC = apply(X = cal_mean_sd, MARGIN = 1, FUN = mean))
result_all$intersection_table <- dplyr::bind_cols(result_all$intersection_table, 
                                                  SD_LOG2FC = apply(X = cal_mean_sd, MARGIN = 1, FUN = sd))
### Select other column (FDR & Probability value)
if ("edgeR" %in% input$DEG_methods) {
  result_all$intersection_table <- dplyr::bind_cols(result_all$intersection_table, 
                                                    dplyr::select(.data = result_all$raw_intersection_table[result_all$intersection_table$GENE_NAME,], FDR_edgeR = "edgeR_fdr")
  )
}
if ("DESeq2" %in% input$DEG_methods) {
  result_all$intersection_table <- dplyr::bind_cols(result_all$intersection_table, 
                                                    dplyr::select(.data = result_all$raw_intersection_table[result_all$intersection_table$GENE_NAME,], ADJPVALUE_DESeq2 = "DESeq2_fdr")
  )
}
if ("NOISeq" %in% input$DEG_methods) {
  result_all$intersection_table <- dplyr::bind_cols(result_all$intersection_table, 
                                                    dplyr::select(.data = result_all$raw_intersection_table[result_all$intersection_table$GENE_NAME,], PROB_NOISeq = "NOISeq_prob")
  )
}
if ("EBSeq" %in% input$DEG_methods) {
  result_all$intersection_table <- dplyr::bind_cols(result_all$intersection_table, 
                                                    dplyr::select(.data = result_all$raw_intersection_table[result_all$intersection_table$GENE_NAME,], PPDE_EBSeq = "EBSeq_PPDE")
  )
}


cat("Done!\n---------------------------------------------------------------------\n")
