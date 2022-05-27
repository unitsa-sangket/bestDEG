# Script to generate read count and sample information table from pasilla package
# Created by: Prasert Yodsawat
# Date created: 5 October 2020


# Original datasets generation code from DESeq2 manual
library("pasilla")
pasCts <- system.file("extdata",
                      "pasilla_gene_counts.tsv",
                      package = "pasilla",
                      mustWork = TRUE)
pasAnno <- system.file(
  "extdata",
  "pasilla_sample_annotation.csv",
  package = "pasilla",
  mustWork = TRUE
)
cts <- as.matrix(read.csv(pasCts, sep = "\t", row.names = "gene_id"))
coldata <- read.csv(pasAnno, row.names = 1)
coldata <- coldata[, c("condition", "type")]
coldata$condition <- factor(coldata$condition)
coldata$type <- factor(coldata$type)
## chop off the "fb" character of the row names of coldata
rownames(coldata) <- sub("fb", "", rownames(coldata))


# Modify before used in bestDEG
## correct the order of colnames in "cts" (count matrix) with the
## order of rownames of "coldata"
cts <- cts[, rownames(coldata)]
## remove column "type" from "coldata"
coldata$type = NULL
## remove unused R variable before save to .RData
rm(pasCts, pasAnno)
## save R varible to .RData
save.image("DEG_pipeline/example_data_pasilla.RData")


# Save example datasets from "pasilla" packages
## count table (cts.tsv)
transcript_name <- rownames(cts)
cts <- cbind(transcript_name, cts)
write.table(
  x = cts,
  file = "cts.tsv",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE,
  fileEncoding = "utf-8"
)
## sample information (coldata.tsv)
sample_name <- rownames(coldata)
coldata <- cbind(sample_name, coldata)
write.table(
  x = coldata,
  file = "coldata.tsv",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE,
  fileEncoding = "utf-8"
)
