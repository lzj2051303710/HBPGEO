
#######################
ui <- navbarPage(
  title = "HBPGEO",
  theme = shinytheme("cosmo"),
  tabPanel("sample analysis",
           sidebarLayout(
           sidebarPanel(width = 5,
                 h4("Data options"),
                     fluidRow(
                            column(width = 12, 
                                   selectInput("UserDataChoice1",
                                               "Please select the dataset:", datalistfiles, 
                                               selected =  datalistfiles[1])
                                   ),
                            ),
                 fluidRow(
                   column(width = 8, 
                          fileInput('exprSet', 'Choose Gene expression matrix (CSV)', 
                                    accept=c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv')) 
                   ),
                 ),

                 fluidRow(
                   column(width = 8, 
                          fileInput('group_list', 'Choose group File(CSV)', 
                                    accept=c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv')) 
                   ),
                 ),
                 
                 fluidRow(
                   column(width = 8, 
                          fileInput('Traits', 'Choose Traits File(CSV)', 
                                    accept=c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv')) 
                   ),
                 ),
                 

                 hr(),
                 h4("The drawing options"),
                 
                 fluidRow(
                   

                   column(width = 4, 
                          selectInput("UserPanelLayout", 
                                      "Panel layout:", choices = list("automatic", "manual"),
                                      selected = "automatic")
                   )
                 ),
                 fluidRow(
                   column(width = 3, 
                          selectInput("UserPrintHeight",
                                      "Plot height:", choices = c(1:10*100), selected = 500)
                   ),
                   column(width = 3, 
                          selectInput("UserPrintWidth",
                                      "Plot width:", choices = c("auto", 1:10*100), selected = "auto")
                   ),
                   column(width = 3, 
                          conditionalPanel(condition = "input.UserPanelLayout == 'manual'",
                                           numericInput("UserPanelLayoutCols", 
                                                        "Columns:", value = 4)
                          )
                   ),
                   column(width = 3,  
                          conditionalPanel(condition = "input.UserPanelLayout == 'manual'",
                                           numericInput("UserPanelLayoutRows", 
                                                        "Rows:", value = 4)
                          )
                   )
                   
                 ),
                          
                          hr(),
                 h4("Download options"),
                 radioButtons('extTable', 'Table output format',
                              choices = c("CSV"='csv', 'TXT'='txt'), 
                              inline = T),
                 radioButtons('extPlot', 'Plot output format',
                              choices = c("PNG"='png', 'PDF'='pdf','JPEG'='jpeg'),
                              inline = T),
                 helpText('Please choose the format of table and plot that you need, the download 
                 buttons are placed in respective tabs'),
                 hr(),
                 fluidRow(
                   column(width = 6, 
                          br(), 
                          submitButton("submit")
                         )
                        ),
             ),
###############################
        column(width = 7,
                    wellPanel(
                      tabsetPanel(
                        tabPanel("Gene expression matrix", 
                                 br(), br(),
                                 DT::DTOutput("table"),
                                 downloadButton("UserDownloadTable1", "Download table")
                        ),
                        tabPanel("Box-plot", 
                                 
                                 uiOutput("barchart.ui"),
                                 downloadButton("Downloadbarchart", "Download Plot"),
                                 br(), br()
                        ),
                        
                        tabPanel("PCA", 
                                 
                                 uiOutput("PCA.ui"),
                                 downloadButton("DownloadPCA", "Download Plot"),
                                 br(), br()
                                 ),
              
                       tabPanel("clustering", 
                                uiOutput("clustering.ui"),
                                downloadButton("Downloadclustering", "Download Plot")
                                )
                        
                       
                           )
                       ) 
                  )
           )
),
  
########################################################### 
  
  tabPanel("DEG",
           sidebarLayout(
           sidebarPanel(width = 5,
                     h4("Data options"),

                     fluidRow(
                       column(width = 6, 
                              numericInput("inlogFC", label = h4("Please enter the fold change"), value = " "),
                       )
                     ),
                     fluidRow(
                       column(width = 6, 
                              numericInput("diffLab_p", label = h4("Please enter the P.Value"), value = 0.05),
                       )
                     ), 
                     h4("The drawing options"),
                     
                     fluidRow(
                       
                       column(width = 4, 
                              selectInput("UserPanelLayout1", 
                                          "Panel layout:", choices = list("automatic", "manual"),
                                          selected = "automatic")
                       )
                     ),
                     fluidRow(
                       column(width = 3, 
                              selectInput("UserPrintHeight1",
                                          "Plot height:", choices = c(1:10*100), selected = 500)
                       ),
                       column(width = 3, 
                              selectInput("UserPrintWidth1",
                                          "Plot width:", choices = c("auto", 1:10*100), selected = "auto")
                       ),
                       column(width = 3, 
                              conditionalPanel(condition = "input.UserPanelLayout1 == 'manual'",
                                               numericInput("UserPanelLayoutCols1", 
                                                            "Columns:", value = 4)
                              )
                       ),
                       column(width = 3,  
                              conditionalPanel(condition = "input.UserPanelLayout1 == 'manual'",
                                               numericInput("UserPanelLayoutRows1", 
                                                            "Rows:", value = 4)
                              )
                       )
                       
                     ),
                          hr(),
                       
                     h4("Download options"),
                     radioButtons('extTable2', 'Table output format',choices = c("CSV"='csv', 'TXT'='txt'), inline = T),
                     radioButtons('extPlot2', 'Plot output format',choices = c("PNG"='png', 'PDF'='pdf','JPEG'='jpeg'), inline = T),
                     helpText('Please choose the format of table and plot that you need, the download 
               buttons are placed in respective tabs'),
                     hr(),
                     fluidRow(
                       column(width = 6, 
                              br(), 
                              submitButton("submit")
                       )
                     )
                     
                     ),
           
##################          
        column(width = 7,
                    wellPanel(
                      tabsetPanel(
                        tabPanel("DEG", 
                                br(), br(),
                                 DT::DTOutput("DEGtable"),
                                 downloadButton("UserDownloadDEGTable2", "Download Table")
                                
                        ),
                          tabPanel("heatmap", 
                                  uiOutput("DEGheatmap.ui"),
                                  downloadButton("DownloadDEGheatmap", "Download Plot"),
                                 br(), br()
                        ),
                        tabPanel("Volcano", 
                                 uiOutput("vocano.ui"),
                                 downloadButton("Downloadvocano", "Download Plot"),
                                 br(), br()
                        ),
                        tabPanel("DEG_Matrix", 
                                 br(), br(),
                                 DT::DTOutput("DEGMatrix"),
                                 downloadButton("UserDownloadDEGMatrix", "Download Table")
                                 
                        )

                        
                      )
                    ) 
             )
           )
  ),
######################################
tabPanel("Gene module analysis",
         sidebarLayout(
           sidebarPanel(width = 5,
                        h4("Data options"),


                        fluidRow(
                          column(width = 6, 
                                 textInput("off_text", "Please enter the GSM of the outlier sample:", "")
                          )
                        ), 
                       
                        fluidRow(
                          column(width = 6, 
                                 numericInput("Genes_data", label = h4("Please set the number of genes to be analyzed"), value = 6000),
                          )
                        ),
                        fluidRow(
                          column(width = 6, 
                                 numericInput("cutoff_soft", label = h4("Please Set the optimal soft threshold"), value = 6),
                          )
                        ), 
                        fluidRow(
                          column(width = 6, 
                                 numericInput("minModule_Size", label = h4("Please set minClusterSize"), value = 30),
                          )
                        ), 
                        h4("The drawing options"),
                        
                        fluidRow(
                          
                         
                          column(width = 4, 
                                 selectInput("UserPanelLayout2", 
                                             "Panel layout:", choices = list("automatic", "manual"),
                                             selected = "automatic")
                          )
                        ),
                        fluidRow(
                          column(width = 3, 
                                 selectInput("UserPrintHeight2",
                                             "Plot height:", choices = c(1:10*100), selected = 500)
                          ),
                          column(width = 3, 
                                 selectInput("UserPrintWidth2",
                                             "Plot width:", choices = c("auto", 1:10*100), selected = "auto")
                          ),
                          column(width = 3, 
                                 conditionalPanel(condition = "input.UserPanelLayout2 == 'manual'",
                                                  numericInput("UserPanelLayoutCols2", 
                                                               "Columns:", value = 4)
                                 )
                          ),
                          column(width = 3,  
                                 conditionalPanel(condition = "input.UserPanelLayout2 == 'manual'",
                                                  numericInput("UserPanelLayoutRows2", 
                                                               "Rows:", value = 4)
                                 )
                          )
                          
                        ),
                        hr(),
                        
                        h4("Download options"),
                        radioButtons('extTable3', 'Table output format',choices = c("CSV"='csv', 'TXT'='txt'), inline = T),
                        radioButtons('extPlot3', 'Plot output format',choices = c("PNG"='png', 'PDF'='pdf','JPEG'='jpeg'), inline = T),
                        helpText('Please choose the format of table and plot that you need, the download 
               buttons are placed in respective tabs'),
                        hr(),
                        fluidRow(
                          column(width = 6, 
                                 br(), 
                                 submitButton("submit")
                          )
                        )
                        
           ),
           
#######################################         
           column(width = 7,
                  wellPanel(
                    tabsetPanel(
                      tabPanel("Clustering of sample ", 
                               
                               uiOutput("sampleclust.ui"),
                               downloadButton("Downloadsampleclust", "Download Plot"),
                               br(), br()
                      ),
                      tabPanel("Gene expression matrix", 
                               br(), br(),
                               DT::DTOutput("DatExpr"),
                               downloadButton("UserDownloadWGCNA", "Download Table")
                               
                      ),
                      tabPanel("threshold", 
                               uiOutput("Soft_Threshold.ui"),
                               downloadButton("DownloadSoft_Threshold", "Download Plot"),
                               br(), br(),
                              
                               
                      ),
                      tabPanel("Dynamic cut tree", 
                               uiOutput("Dynamic.ui"),
                               downloadButton("DownloadDynamic", "Download Plot"),
                               br(), br()
                      ),
                      tabPanel("Module Relevance Clustering Hot Map", 
                               uiOutput("Modules.ui"),
                               downloadButton("DownloadModules", "Download Plot"),
                               br(), br()
                      ),
                      tabPanel("Part of the gene topology", 
                               uiOutput("GenesModules.ui"),
                               downloadButton("DownloadGenesModules", "Download Plot"),
                               br(), br()
                      ),
                      tabPanel("Heatmap of correlation between modules and phenotypes", 
                               uiOutput("Module_trait.ui"),
                               downloadButton("DownloadModule_trait", "Download Plot"),
                               br(), br()
                      ),
                      
                      tabPanel("The Significance of Gene and Module", 
                               uiOutput("MSModules.ui"),
                               downloadButton("DownloadMS", "Download Plot"),
                               br(), br()
                      ),
                      
                      tabPanel("Gene module download", 
                               fluidRow(
                                 column(width = 12,
                                        h4("Gene modules download:"), 
                                        h5("Export module data with Cytoscape readable file format, 
                                           file will be downloaded to the Files folder"),
                                        downloadButton("DownloadAllNetworkToCytoscape", "Download modules data")
                                 )
                               ),
                              
                             
                              )
                   
                
                    )
                  ) 
           )
         )
),
######################################################
tabPanel("Hub gene analysis",
         sidebarLayout(
           sidebarPanel(width = 5,
                        h4("Data options"),
                        fluidRow(
                          column(width = 6, 
                                 textInput("Module_text", "Please enter the key module color:", "")
                          )
                        ), 
                        
                        fluidRow(
                          column(width = 6, 
                                 numericInput("GS_soft", label = h4("Please set the threshold of GS"), value = 0.2),
                          )
                        ),
                        fluidRow(
                          column(width = 6, 
                                 numericInput("MM_soft", label = h4("Please set the threshold of MM"), value = 0.8),
                          )
                        ), 
                        fluidRow(
                          column(width = 6, 
                                 numericInput("weighted_soft", label = h4("Please set the threshold of P.weighted"), value = 0.05),
                          )
                        ), 
                        fluidRow(
                          column(width = 6, 
                                 numericInput("qweighted_soft", label = h4("Please set the threshold of q.weighted"), value = 0.05),
                          )
                        ), 
                        
                        fluidRow(
                          column(width = 6, 
                                 textInput("gene_text", "Please enter a gene:", "USP8")
                          )
                        ), 
                        fluidRow(
                          column(width = 6, 
                                 textInput("group1_text", "Please enter the name of the case group:", "hyper")
                          )
                        ),
                        fluidRow(
                          column(width = 6, 
                                 textInput("group2_text", "Please enter the name of the normal group:", "normal")
                          )
                        ),
                        h4("Download options"),
                        radioButtons('extTable4', 'Table output format',choices = c("CSV"='csv', 'TXT'='txt'), inline = T),
                        radioButtons('extPlot4', 'Plot output format',choices = c("PNG"='png', 'PDF'='pdf','JPEG'='jpeg'), inline = T),
                        helpText('Please choose the format of table and plot that you need, the download 
               buttons are placed in respective tabs'),
                        hr(),
                        fluidRow(
                          column(width = 6, 
                                 br(), 
                                 submitButton("submit")
                          )
                        )
                        
           ),
           
#######################################         
           column(width = 7,
                  wellPanel(
                    tabsetPanel(

                      tabPanel("Hub-genes", 
                               br(), 
                               h5('Enter a key module name, click "Submit" button, filter Hub-genes in the key module'),
                               br(),
                               DT::DTOutput("hubGenesData"),
                               downloadButton("UserDownloadhubGenes", "Download Table")
                               
                      ),
                      tabPanel("Boxpot", 
                               br(),
                               h5('Please enter a gene name, click the "Submit" button,and
                                  to see the expression of genes in different groups.'),
                               br(),
                               uiOutput("GeneExp.ui"),
                               downloadButton("DownloadGeneExp", "Download Plot"),
                      
                      ),
                      tabPanel("Violinogram", 
                               br(),
                               h5('Please enter a gene name, click the "Submit" button,
                                  and to see the expression of genes in different groups.'),
                               br(),
                               uiOutput("GeneExp2.ui"),
                               downloadButton("DownloadGeneExp2", "Download Plot"),
                               
                      ),
                      
                       )
                    ) 
           )
         )
), 


######################################################
tabPanel("Gene function analysis",
         sidebarLayout(
           sidebarPanel(width = 5,
                        h4("Data options"),
                        fluidRow(
                          column(width = 8, 
                                 fileInput('genenamesfile', 'Upload genelist', 
                                           accept=c('text/csv', 
                                                    'text/comma-separated-values,text/plain', 
                                                    '.csv')) 
                          ),
                        ),
                        
                        fluidRow(
                          column(width = 6, 
                                 numericInput("GO_pCutoff", label = h4("Please set the P-value threshold of GO"), value = 0.05),
                          )
                        ),
                       fluidRow(
                          column(width = 6, 
                                 numericInput("KEGG_pCutoff", label = h4("Please set the P-value threshold of KEGG"), value = 0.05),
                          )
                        ), 
                        fluidRow(
                          column(width = 6, 
                                 textInput("organism_text", "Please enter the Organism:", "human")
                          )
                        ), 
                        h4("The drawing options"),
                        
                        fluidRow(
                         
                          column(width = 4, 
                                 selectInput("UserPanelLayout3", 
                                             "Panel layout:", choices = list("automatic", "manual"),
                                             selected = "automatic")
                          )
                        ),
                        fluidRow(
                          column(width = 3, 
                                 selectInput("UserPrintHeight3",
                                             "Plot height:", choices = c(1:10*100), selected = 500)
                          ),
                          column(width = 3, 
                                 selectInput("UserPrintWidth3",
                                             "Plot width:", choices = c("auto", 1:10*100), selected = "auto")
                          ),
                          column(width = 3, 
                                 conditionalPanel(condition = "input.UserPanelLayout3 == 'manual'",
                                                  numericInput("UserPanelLayoutCols3", 
                                                               "Columns:", value = 4)
                                 )
                          ),
                          column(width = 3,  
                                 conditionalPanel(condition = "input.UserPanelLayout3 == 'manual'",
                                                  numericInput("UserPanelLayoutRows3", 
                                                               "Rows:", value = 4)
                                 )
                          )
                          
                        ),
                        hr(),
                        h4("Download options"),
                        radioButtons('extTable5', 'Table output format',choices = c("CSV"='csv', 'TXT'='txt'), inline = T),
                        radioButtons('extPlot5', 'Plot output format',choices = c("PNG"='png', 'PDF'='pdf','JPEG'='jpeg'), inline = T),
                        helpText('Please choose the format of table and plot that you need, the download 
               buttons are placed in respective tabs'),
                        hr(),
                        fluidRow(
                          column(width = 6, 
                                 br(), 
                                 submitButton("submit")
                          )
                        )
                        
           ),
           
#######################################         
column(width = 7,
       wellPanel(
         tabsetPanel(
           tabPanel("Gene list", 
                    br(), 
                    h5('Please upload gene list'),
                    br(),
                    DT::DTOutput('GeneNames'), 
                    downloadButton("UserDownloadGeneNames", "Download table")
           ),
           tabPanel("GO analysis", 
                    uiOutput("GenesGO.ui"),
                    downloadButton("DownloadGenesGO", "Download Plot"),
                    br(), br()
           ),
           
           tabPanel("KEGG analysis", 
                    uiOutput("GenesKEGG.ui"),
                    downloadButton("DownloadGenesKEGG", "Download Plot"),
                    br(), br()
           ),
           tabPanel("The Relational network of pathway and genes", 
                    uiOutput("Genespathway.ui"),
                    downloadButton("DownloadGenespathway", "Download Plot"),
                    br(), br()
           )
           
         )
       ) 
      )
    )
), 

########################################################
 tabPanel("About this system", 
          sidebarLayout(
            sidebarPanel(
                         fluidRow(
                             column(width = 12, 
                                    fundbox(width = 12)  
                                   ),
                                 ),

                       ),
            column(width = 12,
                   wellPanel(
                     tabsetPanel(
                       
                     tabPanel("The HBPGEO workflow", 
                              fluidRow(
                                  helpbox(width = 12)
                                       
                                        ),
                                    
                                ),
                             )
                           ) 
                 )
              )
        ),

           
)

