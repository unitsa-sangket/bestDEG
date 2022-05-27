# Program: bestDEG (ui.R)
# Developer: Prasert Yodsawat
# Date created: 1 October 2020


# App UI ----

## Navigation bar ----
ui <- tagList(
  
  ### Initialize 
  includeCSS("www/css/styles_mod.css"),
  shinyjs::useShinyjs(),
  waiter::useWaiter(), 
  waiter::waiterPreloader(
    html = tagList(
      waiter::spin_loaders(
        id = 2,
        color = "black"
      ),
      br(),
      div("Loading", style = "color:black;")
    ),
    color = "white",
    fadeout = TRUE
  ),
  
  ### Navigation bar
  navbarPage(title = img(src = "images/bestdeg_logo.gif"), inverse = TRUE, fluid = TRUE, theme = bslib::bs_theme(version = 4), windowTitle = "bestDEG app",
             # header = div("header test", style = "text-align: center"),
             
             
             
             ### Home ----
             tabPanel(title = "Home",
                      
                      # Sidebar layout
                      sidebarLayout(
                        
                        #### Sidebar panel ----
                        sidebarPanel = sidebarPanel(width = 4,
                                                    
                                                    # "DEBUG zone",
                                                    # verbatimTextOutput("debug"),
                                                    
                                                    ### Upload
                                                    div(id = "ui-upload",
                                                        
                                                        h5("Upload data"),
                                                        
                                                        h6("Read count table:", class = "HeadWithWarn"),
                                                        # Warning message
                                                        div(id = "warn_count_data",
                                                            icon("fas fa-exclamation-triangle"),
                                                            "Required",
                                                            class = "Warn"
                                                        ),
                                                        fileInput(inputId = "count_data", label = NULL, multiple = FALSE, accept = ".tsv", buttonLabel = "Choose File", placeholder = ""),
                                                        
                                                        h6("Sample information:", class = "HeadWithWarn"),
                                                        # Warning message
                                                        div(id = "warn_sample_information",
                                                            icon("fas fa-exclamation-triangle"),
                                                            "Required",
                                                            class = "Warn"
                                                        ),
                                                        fileInput(inputId = "sample_information", label = NULL, multiple = FALSE, accept = ".tsv", buttonLabel = "Choose File", placeholder = ""),
                                                        
                                                    ),
                                                    
                                                    hr(),
                                                    
                                                    ### Parameters
                                                    div(id = "ui-parameters", 
                                                        
                                                        h5("Parameter selection"),
                                                        h6("DEG methods:", class = "HeadWithWarn"),
                                                        div(id = "warn_DEG_methods",
                                                            icon("fas fa-exclamation-triangle"),
                                                            "Select more than 1 method", 
                                                            class = "Warn"
                                                        ),
                                                        checkboxGroupInput(inputId = "DEG_methods", label = NULL, inline = TRUE, choices = c("edgeR", "DESeq2", "NOISeq", "EBSeq"), selected = c("edgeR", "DESeq2")),
                                                        h6("False Discovery Rate:", class = "HeadWithWarn"),
                                                        numericInput(inputId = "fdr", label = NULL, value = 0.01, step = 0.01, min = 0.01),
                                                        h6("Log 2 fold change:", class = "HeadWithWarn"),
                                                        numericInput(inputId = "l2fc", label = NULL, value = 2, step = 0.5, min = 0),
                                                        #### Selector option
                                                        div(id = "optionrefdiv")
                                                        
                                                    ),
                                                    
                                                    hr(),
                                                    
                                                    ### Run
                                                    actionButton(inputId = "run_bestDEG", label = "Run bestDEG"),
                                                    
                        ),
                        
                        #### Main panel ----
                        mainPanel = mainPanel(
                          
                          div(
                            id = "ResultAreadiv",
                            # class = "ResultArea",
                            
                            ## Preview
                            ### Count data
                            div(id = "previewcountdiv"),
                            ### Sample information
                            div(id = "previewsamplediv"),
                            
                            
                            ## Results
                            ### Header
                            # div(id = "resultheaderdiv"),
                            
                            
                            splitLayout(
                              ### Venn plot
                              div(id = "resultvenndiv", class = "BoxArea"),
                              
                              ### Download button
                              div(id = "resultdownloaddiv", class = "BoxArea")
                              
                            ),
                            
                            # br(),
                            ### DEG table
                            div(id = "resultdegtablediv", class = "BoxArea")
                            
                          )
                          
                        )
                        
                      )
                      
             ),
             
             
             ### Tutorial ----
             tabPanel(title = "Tutorial", 
                      
                      div(style = "min-height: 550px;",
                        
                        h5("Tutorial"),
                        
                        "The analysis workflow of bestDEG was describe below", br(),
                        
                        div(
                          "1) In", strong("\"Upload data\""), "section, upload your read count table and sample information files to the app.",
                          br(),
                          "2) In", strong("\"DEG methods\""), "section, select the DEG method used to find the consensus DEG result.",
                          br(),
                          "3) Specify the false discovery rate value.",
                          br(),
                          "4) Specify the log 2-fold change value.",
                          br(),
                          "5) Select the reference condition.",
                          br(),
                          "6) Click", strong("\"Run bestDEG\""), "button.",
                          br(),
                          "7) Wait 1 - 5 minutes and the results will presented."
                        ),
                        
                        br(),
                        "The example data can be downloaded by clicking the links below:", br(),
                        
                        tags$li(
                          "Read count table: ",
                          downloadLink(outputId = "ex_read_count", label = "link")
                        ),
                        tags$li(
                          "Sample information: ",
                          downloadLink(outputId = "ex_sample_info", label = "link")
                        )
                      )

             ),
             
             # footer
             footer = div(
               hr(),
               "bestDEG created by Prasert Yodsawat",
               br(),
               "Division of Biological Science, Faculty of Science, Prince of Songkla University, Songkhla, Thailand",
               br(),
               "(C) 2022 Copyright: Prasert Yodsawat, All right reserved",
               class = "Footer"
             )
             
  )
)
