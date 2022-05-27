# Program: bestDEG (server.R)
# Developer: Prasert Yodsawat
# Date created: 1 October 2020


# App Server ----

server <- function(input, output){
  
  # DEBUG zone
  # output$debug <- renderPrint(
  #   {
  #     input$count_data
  #   }
  # )
  
  
  # For shinyjs
  
  shinyjs::hide("resultvenndiv")
  shinyjs::hide("resultdownloaddiv")
  shinyjs::hide("resultdegtablediv")
  shinyjs::hide("ResultAreadiv")
  
  # files upload warning
  observe(
    
    ## For count data
    if (is.null(input$count_data)) {
      shinyjs::show("warn_count_data")
    } else {
      shinyjs::hide("warn_count_data")
    }
    
  )
  
  # files upload warning
  observe(
    
    ## For sample information
    if (is.null(input$sample_information)) {
      shinyjs::show("warn_sample_information")
    } else {
      shinyjs::hide("warn_sample_information")
    }
    
  )
  
  
  # Check for input (all files and parameters)
  observe(
    ## Check for both count data and sample information files are uploaded?
    if (is.null(input$count_data) || is.null(input$sample_information)) {
      
      shinyjs::disable("ui-parameters")
      shinyjs::hide("warn_DEG_methods")
      shinyjs::disable("run_bestDEG")
      
    } else {
      
      shinyjs::enable("ui-parameters")
      
      ## Disable when select < 2 DEG method
      if (length(input$DEG_methods) < 2) {
        
        shinyjs::show("warn_DEG_methods")
        shinyjs::disable("fdr")
        shinyjs::disable("l2fc")
        shinyjs::disable("optionrefdiv")
        shinyjs::disable("run_bestDEG")
        
      } else {
        
        shinyjs::hide("warn_DEG_methods")
        shinyjs::enable("fdr")
        shinyjs::enable("l2fc")
        shinyjs::enable("optionrefdiv")
        shinyjs::enable("run_bestDEG")
        
      }
      
    }
  )
  
  
  
  # Setup ----
  
  ## Create reactive value to store the value in server side
  internal <- reactiveValues()
  
  
  # Upload & Preview variable ----
  
  ## Read the count data file to internal reactive value and preview the data
  observeEvent(eventExpr = input$count_data, ignoreInit = TRUE, ignoreNULL = TRUE,
               handlerExpr = {
                 # Show result area if there is hidden
                 shinyjs::show("ResultAreadiv")
                 
                 # Hide UI if there are show
                 shinyjs::hide("resultvenndiv")
                 shinyjs::hide("resultdownloaddiv")
                 shinyjs::hide("resultdegtablediv")
                 
                 # Remove UI if it already exists
                 removeUI(selector = "#div-preview_count", immediate = TRUE)
                 
                 # Remove UI of the results if it already exists
                 removeUI(selector = "#div-result_header", immediate = TRUE)
                 removeUI(selector = "#div-result_venn", immediate = TRUE)
                 removeUI(selector = "#div-result_download", immediate = TRUE)
                 removeUI(selector = "#div-result_deg_table", immediate = TRUE)
                 
                 
                 # Read the sample information to internal reactive value
                 internal$count_data <- read.table(file = input$count_data$datapath, header = TRUE, row.names = 1)
                 # For preview data only
                 internal$count_data2 <- read.table(file = input$count_data$datapath, header = TRUE)
                 
                 # Output preview count data
                 output$count_data_preview <- renderTable(bordered = TRUE, striped = TRUE,
                                                          expr = {
                                                            # Preview first 5 rows
                                                            return(head(internal$count_data2))
                                                          }
                 )
                 
                 # Insert UI (Count data)
                 insertUI(
                   selector = "#previewcountdiv",
                   where = "beforeEnd",
                   ui = div(
                     class = "BoxArea",
                     id = "div-preview_count",
                     h5("Count data preview"),
                     tableOutput(outputId = "count_data_preview")
                   )
                 )
                 
               }
  )
  
  ## Read the sample information file to internal reactive value, create the selector option at parameters section and preview the data
  observeEvent(eventExpr = input$sample_information, ignoreInit = TRUE, ignoreNULL = TRUE,
               handlerExpr = {
                 # Show result area if there is hidden
                 shinyjs::show("ResultAreadiv")
                 
                 # Hide UI if there are show
                 shinyjs::hide("resultvenndiv")
                 shinyjs::hide("resultdownloaddiv")
                 shinyjs::hide("resultdegtablediv")
                 
                 # Remove UI if it already exists
                 removeUI(selector = "#div-preview_sample", immediate = TRUE)
                 removeUI(selector = "#div-ref_select", immediate = TRUE)
                 
                 # Remove UI of the results if it already exists
                 removeUI(selector = "#div-result_header", immediate = TRUE)
                 removeUI(selector = "#div-result_venn", immediate = TRUE)
                 removeUI(selector = "#div-result_download", immediate = TRUE)
                 removeUI(selector = "#div-result_deg_table", immediate = TRUE)
                 
                 # Read the sample information to internal reactive value
                 internal$sample_information <- read.table(file = input$sample_information$datapath, header = TRUE, row.names = 1, stringsAsFactors = TRUE)
                 # For preview data only
                 internal$sample_information2 <- read.table(file = input$sample_information$datapath, header = TRUE, stringsAsFactors = TRUE)
                 
                 # Output preview sample information data
                 output$sample_information_preview <- renderTable(bordered = TRUE, striped = TRUE,
                                                                  expr = {
                                                                    # Preview
                                                                    return(internal$sample_information2)
                                                                  }
                 )
                 
                 # Insert UI (Sample information)
                 insertUI(
                   selector = "#previewsamplediv",
                   where = "beforeEnd",
                   ui = div(
                     class = "BoxArea",
                     id = "div-preview_sample",
                     h5("Sample information preview"),
                     tableOutput(outputId = "sample_information_preview")
                   )
                 )
                 
                 
                 # Create the vector value of selector option
                 ref_option <- unique(as.vector(internal$sample_information$condition))
                 
                 # Insert UI (Selector option)
                 insertUI(
                   selector = "#optionrefdiv",
                   where = "beforeEnd",
                   ui = div(id = "div-ref_select",
                            h6("Specify the reference level"),
                            selectInput(inputId = "ref_select", label = NULL, choices = c(ref_option))
                   )
                 )
                 
               }
  )
  
  
  # Main pipeline  ----
  
  ## Function to run bestDEG if press the button
  Result_bestDEG <- eventReactive(eventExpr = input$run_bestDEG, 
                                  valueExpr = {
                                    if (!is.null(internal$count_data) && !is.null(internal$sample_information)) {
                                      # Show modal dialog during DEG analysis
                                      showModal(
                                        modalDialog(
                                          div(
                                            id = "modal-text",
                                            h5(style = "text-align: center;", "Please wait"),
                                            div(
                                              style = "margin-top: 10px;",
                                              div(class = "spinner-border")
                                            ),
                                            style = "text-align: center;"
                                          ),
                                          title = NULL,
                                          size = "s",
                                          footer = NULL,
                                          easyClose = FALSE,
                                          fade = TRUE
                                        )
                                      )
                                      
                                      # Remove UI before showing the results
                                      removeUI(selector = "#div-preview_count", immediate = TRUE)
                                      removeUI(selector = "#div-preview_sample", immediate = TRUE)
                                      
                                      removeUI(selector = "#div-result_header", immediate = TRUE)
                                      removeUI(selector = "#div-result_venn", immediate = TRUE)
                                      removeUI(selector = "#div-result_download", immediate = TRUE)
                                      removeUI(selector = "#div-result_deg_table", immediate = TRUE)
                                      
                                      # Hide UI if there are showed
                                      shinyjs::hide("resultvenndiv")
                                      shinyjs::hide("resultdownloaddiv")
                                      shinyjs::hide("resultdegtablediv")
                                      
                                      # Go to top page
                                      shinyjs::runjs("window.scrollTo({top: 0, behavior: 'smooth'})")
                                      
                                      
                                      # ---- Add the bestDEG DEG pipeline here ---- #
                                      
                                      
                                      # Modified the column order of sample with rownames of sample information
                                      cat("Prepare input\n")
                                      internal$count_data <- internal$count_data[, rownames(internal$sample_information)]
                                      
                                      
                                      #
                                      
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
                                          cat("Error DEG regulation status at row:", i, "of variable \"result_all$raw_intersection_table\"\n")
                                          DEG_regulate_status[i] <- "Error"
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
                                      
                                      
                                      # ---- Remove modal ---- #
                                      removeModal()
                                      
                                      # Show UI if there are hidden
                                      shinyjs::show("resultvenndiv")
                                      shinyjs::show("resultdownloaddiv")
                                      shinyjs::show("resultdegtablediv")
                                      
                                      # ---- Return the result of DEG pipeline ---- #
                                      return(result_all)
                                    }
                                    
                                  }
  )
  
  
  # Display result ----
  
  ## Render UI ----
  
  ## Display the results after press submit button
  observeEvent(eventExpr = Result_bestDEG(), ignoreInit = FALSE, ignoreNULL = TRUE,
               handlerExpr = {
                 
                 # Insert UI (Header)
                 # insertUI(
                 #   selector = "#resultheaderdiv",
                 #   ui = div(id = "div-result_header",
                 #            h4(strong("Result"))
                 #   )
                 # )
                 
                 # Insert UI (Venn plot)
                 insertUI(
                   selector = "#resultvenndiv",
                   ui = div(id = "div-result_venn",
                            h5(strong("Venn diagram"), style = "text-align: center;"),
                            plotOutput(outputId = "venn")
                            
                   )
                 )
                 
                 # Insert UI (Download button)
                 insertUI(
                   selector = "#resultdownloaddiv",
                   ui = div(id = "div-result_download",
                            h5(strong("Files download"), style = "text-align: center;"),
                            
                            div(
                              style = "text-align: center; height: 400px;",
                              # Venn diagram plot
                              downloadButton(outputId = "Result_plot", label = "Venn diagram plot", class = "DownloadButton"),
                              br(),
                              
                              # Intersection
                              downloadButton(outputId = "Result_file_intersec", label = "Consensus result", class = "DownloadButton"),
                              br(),
                              
                              # Each selected DEG methods
                              if ("edgeR" %in% input$DEG_methods) {
                                downloadButton(outputId = "Result_file_edgeR", label = "edgeR result", class = "DownloadButton")
                              },
                              br(),
                              if ("DESeq2" %in% input$DEG_methods) {
                                downloadButton(outputId = "Result_file_DESeq2", label = "DESeq2 result", class = "DownloadButton")
                              },
                              br(),
                              if ("NOISeq" %in% input$DEG_methods) {
                                downloadButton(outputId = "Result_file_NOISeq", label = "NOISeq result", class = "DownloadButton")
                              },
                              br(),
                              if ("EBSeq" %in% input$DEG_methods) {
                                downloadButton(outputId = "Result_file_EBSeq", label = "EBSeq result", class = "DownloadButton")
                              }
                            )
                   )
                 )
                 
                 # Insert UI (DEG table) (intersection)
                 insertUI(
                   selector = "#resultdegtablediv",
                   ui = div(id = "div-result_deg_table",
                            h5(strong("Consensus DEG result")),
                            br(),
                            DT::DTOutput(outputId = "Result_bestDEG")
                   )
                 )
                 
               }
  )
  
  
  ## Render plot ----
  
  ### Cache Venn diagram plot ----
  cache_venn <- eventReactive(
    eventExpr = Result_bestDEG(),
    valueExpr = {
      
      vennplot <- ggvenn::ggvenn(data = Result_bestDEG()$DEG_gene, columns = names(Result_bestDEG()$DEG_gene), show_percentage = FALSE, 
                     fill_color = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF"),
                     stroke_size = 0.5, set_name_size = 6, text_size = 6)
      
      vennplot + theme(plot.background = element_rect(fill = '#00000000', color = '#00000000'),
                       panel.background = element_rect(fill = '#00000000', color = '#00000000'))
      
    }
  )
  
  ### Venn diagram plot ----
  output$venn <- renderPlot(
    {
      if (!is.null(Result_bestDEG())) {
        cache_venn()
      }
    }, res = 84, bg = "#00000005"
  )
  
  ### Output table ----
  output$Result_bestDEG <- DT::renderDT(options = list(pageLength = 10, searchHighlight = TRUE, scrollX = TRUE), 
                                        expr = {
                                          # Reduce value to 3 significant digits
                                          cbind(Result_bestDEG()$intersection_table[, 1:2], 
                                                signif(Result_bestDEG()$intersection_table[, 3:length(Result_bestDEG()$intersection_table)], digits = 3))
                                        },
                                        rownames = FALSE, selection = "none"
  )
  
  
  # File download function ----
  
  ## Venn diagram ----
  output$Result_plot <- downloadHandler(
    filename = "bestDEG_venn_plot.png",
    content = function(file){
      png(filename = file, res = 84)
      print(cache_venn())
      dev.off()
    }
  )
  
  ## Consensus result ----
  output$Result_file_intersec <- downloadHandler(
    filename = "bestDEG_consensus_result.tsv", 
    content = function(file){
      write.table(as.data.frame(Result_bestDEG()$intersection_table), file = file, sep = "\t", row.names = FALSE)
    }
  )
  
  ## edgeR result ----
  output$Result_file_edgeR <- downloadHandler(
    filename = "bestDEG_edgeR_result.tsv", 
    content = function(file){
      write.table(tibble::rownames_to_column(as.data.frame(Result_bestDEG()$result_edgeR), var = "GENE_NAME"), file = file, sep = "\t", row.names = FALSE)
    }
  )
  
  ## DESeq2 result ----
  output$Result_file_DESeq2 <- downloadHandler(
    filename = "bestDEG_DESeq2_result.tsv", 
    content = function(file){
      write.table(tibble::rownames_to_column(as.data.frame(Result_bestDEG()$result_DESeq2), var = "GENE_NAME"), file = file, sep = "\t", row.names = FALSE)
    }
  )
  
  ## NOISeq result ----
  output$Result_file_NOISeq <- downloadHandler(
    filename = "bestDEG_NOISeq_result.tsv", 
    content = function(file){
      write.table(tibble::rownames_to_column(as.data.frame(Result_bestDEG()$result_NOISeq), var = "GENE_NAME"), file = file, sep = "\t", row.names = FALSE)
    }
  )
  
  ## EBSeq result ----
  output$Result_file_EBSeq <- downloadHandler(
    filename = "bestDEG_EBSeq_result.tsv", 
    content = function(file){
      write.table(tibble::rownames_to_column(as.data.frame(Result_bestDEG()$result_EBSeq), var = "GENE_NAME"), file = file, sep = "\t", row.names = FALSE)
    }
  )
  
  # Example files in tutorial ----
  
  ## Read count table ----
  output$ex_read_count <- downloadHandler(
    filename = "read_count_table.tsv",
    content = function(file){
      file.copy(from = "cts.tsv", to = file)
    }
  )
  
  ## Sample information ----
  output$ex_sample_info <- downloadHandler(
    filename = "sample_information.tsv",
    content = function(file){
      file.copy(from = "coldata.tsv", to = file)
    }
  )
  
}
