fluidPage(
  theme = shinytheme("yeti"),
  navbarPage(
    title = ">> MVApp <<",

   # Tab 1 = = = = = = = = = = = = = = >> BACKGROUND INFORMATION << = = = = = = = = = = = = = = = = = = 
   
   tabPanel(
      "Background information",
      icon = icon("info"),
      h2("About the MVApp"),
      "This App is a result of hard work of a KAUST team, originating from SaltLab, lead by Prof. Mark Tester.",
      br(),
      "Here we will put some helpful and encouraging text that is refering to the paper that will be (by then) published in a high impact and open-access journal ;) For now, we will leave this bit as is"
      # end of Tab1
    ),
   
   # Tab 2 = = = = = = = = = = = = = = >> DATA UPLOAD << = = = = = = = = = = = = = = = = = =   
    
   tabPanel(
      "Upload your data",
      icon = icon("table"),
      sidebarPanel(
        fluidRow(
          helpText("Upload your data or choose one of the example datasets:"),
          fileInput(
            "your_data",
            label = "Upload your file",
            accept = c('text / csv', '.csv', 'text/comma-separated-values')
          ),
          uiOutput("CustomGeno"),
          uiOutput("CustomIndepVar"),
          uiOutput("CustomPheno"),
          checkboxInput("IdCheck", label = "Do you have a column with Sample ID?", value = F),
          uiOutput("CustomID"),
          checkboxInput("TimeCheck", label = "Do you have a column with Time values?", value = F),
          uiOutput("CustomTimepoint"),
          actionButton("Go_Data", label = "Click to set the data", icon = icon("play-circle"))
          
          # end of sidebarPanel
        )
      ),
      
      mainPanel(navbarPage(
        ">> Data Magic <<",
        tabPanel("Original data", icon = icon("flask"),
                 verbatimTextOutput("uploaded_data_report"),
                 dataTableOutput("Data_tabl")),
        tabPanel("New data", icon = icon("magic"),
                 verbatimTextOutput("selected_data_report"),
                 dataTableOutput("my_data"))
      ))
      # end of Tab2
    ),
   
# Tab 3 = = = = = = = = = = = = = = >> MODELING DATA << = = = = = = = = = = = = = = = = = = 
   
   tabPanel("Fitting curves to the data",icon = icon("wrench"),
            sidebarPanel(
              fluidRow(
                navbarPage("",
                           tabPanel("Modeling",
                                    helpText("Please select which phenotype you would like to model"),
                                    uiOutput("Pheno_to_model"),
                                    uiOutput("IV_to_model"),
                                    uiOutput("IV_subset_model"),
                                    helpText("Click on >>unleash the model estimation<< for estimating which model is the best for the Dependent Variable that you selected"),
                                    actionButton("Go_HelpModel", label = "Unleash the model estimation"),
                                    helpText("Here you can select which modeling type you'd like to do"),
                                    selectInput("model",
                                                label = "Select method",
                                                choices = list(
                                                  "linear" = "lin",
                                                  "quadratic" = "quad",
                                                  "exponential" = "exp",
                                                  "square root" = "sqr")),
                                    actionButton("Go_Model", label = "Unleash the model", icon = icon("play-circle"))
                                    # actionButton("Go_SaveModelData", label = "Lock this modelled data for use in next steps", icon=icon("hand-o-right")),
                                    # helpText("If you are satisfied with the results of the modeling, you can add them to the dataset that can be used in the 'Data curation' tab"),
                                    ),
                           tabPanel("Fit Plot options",
                                    "some gadgets?")   
                ))),
            # end of side Panel
            mainPanel(
              navbarPage("",
                         tabPanel("Estimate best model",
                                  verbatimTextOutput("best_model_advice"),
                                  dataTableOutput("Model_estimation")),
                         tabPanel("Modelled data",
                                  verbatimTextOutput("model_warning"),
                                  uiOutput("Model_download_button"),
                                  dataTableOutput("Model_data")),
                         tabPanel("Fit-Plot",
                                  uiOutput("Select_modelPlot"),
                                  plotOutput("Model_plot"))
              ))
            # end of Tab3
   ),

# Tab 4 = = = = = = = = = = = = = = >> DATA CURATION << = = = = = = = = = = = = = = = = = =   

tabPanel("Data curation", icon = icon("gavel"),
         sidebarPanel(
           fluidRow(
             navbarPage("",
                        tabPanel("outlier selection",            
                                 checkboxInput("Go_omitna", label = "Remove rows containing missing data prior to outlier selection"),
                                 br(),
                                 uiOutput("IV_outliers_selection"),
                                 selectizeInput("Out_pheno_single_multi", label = "Would you like to select outliers based on", choices=c("All phenotypes", "Single phenotype"), multiple = F),
                                 
                                 selectizeInput("outlier_method", label="Select the method for the outlier selection", 
                                                choices = list(
                                                  "1.5*IQR away from the mean",
                                                  "Cook's Distance",
                                                  "Bonferonni outlier test",
                                                  "1xStDev from the median", 
                                                  "2xStDev from the median", 
                                                  "2.5xStDev from the median", 
                                                  "3xStDev from the median" 
                                                ), multiple = F),
                                 br(),
                                 uiOutput("Pheno_outliers"),
                                 uiOutput("Outliers_selection_pheno"),
                                 br(),
                                 actionButton("Go_outliers", label = "Unleash outlier highlight tool"),  
                                 br(),
                                 uiOutput("Outliers_save")),
                        
                        
                        
                        tabPanel("graph tweaks",
                                 uiOutput("Pheno_graph_outliers"),
                                 radioButtons("outlier_graph_type", "Type of graph", choices = c("box plot", "scatter plot", "bar plot")),
                                 sliderInput("out_plot_length", label = "increase the plot length", 200, 2000, 400),
                                 br(),
                                 checkboxInput("outlier_colour", "would you like to colour-code one of your Indepentent Variables in the graph?"),
                                 uiOutput("Q_colour"),
                                 checkboxInput("outlier_facet", "would you like to facet the graph?"),
                                 uiOutput("Q_facet"),
                                 uiOutput("Facet_user_input_columns"),
                                 uiOutput("Facet_outlier_scale")),
                        tabPanel("summary statistics",
                                 uiOutput("Data_for_SummaryStats"), # Select the dataset to be used for Summary Stats - <3<3<3 MMJ <3<3<3
                                 uiOutput("CustomSumm"), ### <<< Added this,   Hashed out selectize  below       %% Mitch %%
                                 actionButton("Go_SummaryStat", label = "unleash Summary Statistics")
                        )
             ))),
         mainPanel(
           navbarPage("Get it OUT",
                      tabPanel("The outliers test", icon=icon("hand-pointer-o"),
                               verbatimTextOutput("na_report"),
                               verbatimTextOutput("Outlier_report"),
                               br(),
                               uiOutput("Full_outlier_download"),
                               br(),
                               DT::dataTableOutput("Outlier_overview_table")),
                      tabPanel("Meet your outliers", icon=icon("bug"),
                               plotlyOutput("outlier_graph"),
                               br(),
                               uiOutput("Pheno_outlier_download"),
                               br(),
                               dataTableOutput("Outlier_only_table")),
                      
                      tabPanel("The graphs with outliers removed", icon=icon("birthday-cake"),
                               plotlyOutput("no_outliers_graph"),
                               br(),
                               uiOutput("Pheno_outlier_free_download"),
                               br(),
                               dataTableOutput("Outlier_free_table")),
                      tabPanel("summary data", icon=icon("flask"),
                               uiOutput("Sum_download_button"),
                               br(),
                               dataTableOutput("sum_data"))
           ))
         # end of Tab#4         
), 

# Tab 5 = = = = = = = = = = = = = = >> DATA EXPLORATION << = = = = = = = = = = = = = = = = = =    

tabPanel("Data exploration", icon=icon("binoculars"),
         sidebarPanel(
           fluidRow(
             uiOutput("Histo_Pheno_data"),
             uiOutput("HisIV"),
             uiOutput("HisDV"),
             uiOutput("Chosenthreshold"),
             #uiOutput("Chosenmultipletest"),
             checkboxInput("plot_facet", "Would you like to facet the graph?"),
             uiOutput("Plotfacets")
           )),
         
         mainPanel(
           navbarPage("",
                       tabPanel("Histograms", icon=icon("area-chart"),
                               uiOutput("HistType"),
                               verbatimTextOutput("Bartlett"),
                               verbatimTextOutput("Levene"),
                               plotlyOutput("HistPlot")
                       ),
                       
                       #tabPanel("Variance analysis", icon=icon("bar-chart-o"),
                       #        verbatimTextOutput("Bartlett")
                       #        #verbatimTextOutput("Levene")
                       #),
                       
                       tabPanel("Boxplots", icon=icon("sun-o"),
                                # actionButton("Go_Boxplot", label = "Plot boxplots"),
                                uiOutput("Shapiro"),
                                verbatimTextOutput("ANOVAtest"),
                                plotlyOutput("Boxes"))
                      
                       #tabPanel("ANOVA plots", icon=icon("snowflake-o"),
                       #        verbatimTextOutput("ANOVAtest"))
                      
           ))
         # end of Tab#5
),
# Tab 6 = = = = = = = = = = = = = = >> CORRELATION ANALYSIS << = = = = = = = = = = = = = = = = = =      

tabPanel(
  "Establish correlations between traits",
  icon = icon("compress"),
  
  navbarPage(
    "",
    tabPanel("General Correlations",
             plotOutput("corrplot")),
    tabPanel(
      "Subsetted correlations",
      sidebarPanel(# select which IV do they want to subset the data? especially if there are more IVs
        uiOutput("CorSpecIV"),
        # select what value(s) of IV1 to display (control or salt?)
        uiOutput("CorSpecIV_val")),
      
      mainPanel(
        "Subset correlations",
        "here another plot for subset correlations",
        plotOutput("corrplot2")
      )
    ),
    
    tabPanel(
      "Scatterplots",
      sidebarPanel(uiOutput("Pheno1"),uiOutput("Pheno2"),uiOutput("colorby")),
      mainPanel(
        textOutput("corrsq"),
        textOutput("corpval"),
        plotlyOutput("scatterplot")
      )
    )
  )
  # end of Tab#6
),

# Tab 7 = = = = = = = = = = = = = = >> PCA ANALYSIS << = = = = = = = = = = = = = = = = = = 

tabPanel(
  "PCA",
  icon = icon("object-group"),
  sidebarPanel(
    fluidRow(
      helpText("Which phenotype you would like to use for the PCA?"),
      uiOutput("PCA_Pheno_data"), # which phenotype data (summarized / na / original) selectize, multiple = F
      checkboxInput("PCA_data_avg", label = "Check if you would like to perform PCA on mean values per genotype / IVs / time"),
      checkboxInput("PCA_data_subset", label = "Check if you would like to perform PCA on specific subset of your data"),
      uiOutput("PCA_subset_trait"),
      uiOutput("PCA_subset_specific"),
      actionButton("Go_PCAdata", label = "set the dataset"),
      uiOutput("PCA_Select_pheno"), # which traits would you like to use? selectize, multiple = T
      # uiOutput("SelectGroup"), # How would you like to colour, selectize (input$SelectGeno, input$SelectDV, input$SelectTime, multiple = F)
      br(),
      actionButton("Go_PCA", label = "Unleash the PCA monster",icon = icon("play-circle")),
      
      # selectInput("Select the principle components", "Select the principle components:",
      uiOutput("PCA1_select"),
      uiOutput("PCA2_select")
    )),
  mainPanel(
    navbarPage("PCA the crazy",
               tabPanel("Selected dataset",
                        dataTableOutput("PCA_raw_table")),
               tabPanel("Final data for PCA",
                        dataTableOutput("PCA_final_table")),
               tabPanel("Eigen Plot",
                        plotlyOutput("PCA_eigen_plot")),
               tabPanel("Contribution Plot",
                        plotlyOutput("PCA_contribution_plot")),
               tabPanel("Scatter Plot",
                        plotlyOutput("PCA_scatter_plot"))
    ))
  # end Tab 7
),

# Tab 8 = = = = = = = = = = = = = = >> CLUSTERING ANALYSIS << = = = = = = = = = = = = = = = = = = 

tabPanel("Clustering",
         icon = icon("sitemap"),
         sidebarPanel(
           fluidRow(
             navbarPage("",
                        tabPanel("Select data",
                                 helpText("In here, you can perform a cluster analysis by grouping your data based on the phenotypic traits and validating the clusters"),
                                 uiOutput("Select_data_cluster"),
                                 uiOutput("Select_phenotypes_cluster"),
                                 checkboxInput("Cluster_pre_calc", label = "Check if you would like to perform cluster analysis on mean values"),
                                 checkboxInput("Cluster_subset_Q", label = "Check if you would like to subset the data before cluster analysis"),
                                 uiOutput("Cluster_subset_trait"),
                                 uiOutput("Cluster_subset_specific"),
                                 uiOutput("Select_cluster_method"),
                                 uiOutput("Select_clustering_method"),
                                 actionButton("Go_cluster", "Unleash cluster analysis")),
                        tabPanel("Determine the clusters",
                                 helpText("Have a look at the dendrogram and choose the value at which you would like to split into individual clusters"),
                                 textInput("Split_cluster", "Enter the numeric value here")),
                        tabPanel("Cluster validation",
                                 helpText("Please choose the phenotype which you would like to further examine for the cluster validation"),
                                 uiOutput("Select_data_cluster_validation"))
             ))        
         ),
         mainPanel(
           navbarPage("Cluster analysis",
                      tabPanel("Cluster HOT HOT Heatmap",
                               #dataTableOutput("Data_cluster_table"),
                               #dataTableOutput("Final_cluster_table"),
                               plotOutput("HotHeatMap")),
                      tabPanel("Cluster dendrogram",
                               verbatimTextOutput("Dendro_sentence"),
                               plotOutput("ClusterTree"),
                               uiOutput("Cluster_download_button"),
                               br(),
                               dataTableOutput("Cluster_table")
                               ),
                      tabPanel("Cluster validation",
                               helpText("Significant effect of clusters was observed on the followind traits:"),
                               htmlOutput("HotAnovaNews"),
                               plotOutput("HotANOVA"))
           ))
         # end of Tab #8
)
# end of App - final brackets
  )
)