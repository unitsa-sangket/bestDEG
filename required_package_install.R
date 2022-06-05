# Script to install packages used in bestDEG
# Created by: Prasert Yodsawat
# Date created: 5 October 2020


# install.packages("shiny")
# install.packages("dplyr")
# install.packages("BiocManager")
# BiocManager::install("DESeq2")
# BiocManager::install("edgeR")
# BiocManager::install("NOISeq")
# BiocManager::install("EBSeq")
# install.packages("gplots")
# install.packages("ggvenn")
# install.packages("DT")
# install.packages("shinyjs")
# 
# # Dataset for pipeline development
# BiocManager::install("pasilla")


# New

# Create a vector of package name
package_name <- c(
  "shiny",
  "dplyr",
  "gplots",
  "ggvenn",
  "DT",
  "shinyjs",
  "BiocManager",
  "waiter"
)

package_name2 <- c(
  "DESeq2",
  "edgeR",
  "NOISeq",
  "EBSeq",
  "pasilla"
)

# Install packages if does not exist
for (i in package_name) {
  if (!requireNamespace(i, quietly = TRUE)) {
    install.packages(i)
    # if (i == "BiocManager") {
    #   library(i, character.only = TRUE, quietly = TRUE)
    # }
  }
}

# Install package from Bioconductor
for (i in package_name2) {
  if (!requireNamespace(i, quietly = TRUE)) {
    install.packages(paste0("bioc::", i))
  }
}

rm(i, package_name, package_name2)

options(repos = BiocManager::repositories())
