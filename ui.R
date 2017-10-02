fluidPage(
  theme = shinytheme("yeti"),
  navbarPage(
    title = ">> MVApp <<",

    tabPanel(
      "Background information",
      icon = icon("info"),
      h2("About the MVApp"),
      "Here we will put some helpful and encouraging text that is refering to the paper that will be (by then) published in a high impact and open-access journal ;) For now, we will leave this bit as is"
      # end of Tab1
    ),
    
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
                 dataTableOutput("Data_tabl")),
        tabPanel("New data", icon = icon("magic"),
                 dataTableOutput("my_data"))
      ))
      # enter main panel text here:
      # end of Tab2
    ),
  
    # Tab#3
    tabPanel(
      "Fitting curves to the data",
      icon = icon("wrench"),
      sidebarPanel(
        fluidRow(
          helpText("Please select which phenotype you would like to model"),
          uiOutput("Pheno_to_model"),
          uiOutput("IV_to_model"),
          uiOutput("IV_subset_model"),
          helpText("Click on >>unleash the model estimation<< for estimating which model is the best for the Dependent Variable that you selected"),
          actionButton("Go_HelpModel", label = "Unleash the model estimation"),
          helpText("Here you can select which modeling type you'd like to do"),
          selectInput(
            "model",
            label = "Select method",
            choices = list(
              "linear" = "lin",
              "quadratic" = "quad",
              "exponential" = "exp",
              "square root" = "sqr"
            )
          ),
          actionButton("Go_Model", label = "Unleash the model", icon = icon("play-circle")),
          helpText("If you are satisfied with the results of the modeling, you can add them to the dataset that can be used in the 'Data curation' tab"),
          actionButton("Go_SaveModelData", label = "Save model data", icon = icon("play-circle"))
        )
      ),
      # end of side Panel
      mainPanel(
        navbarPage("",
        tabPanel("Estimate best model",
                 dataTableOutput("Model_estimation")),
        tabPanel("Modelled data",
                 dataTableOutput("Model_data")),
        tabPanel("Fit-Plot",
                 uiOutput("Select_modelPlot"),
                 actionButton("Go_modelPlot", label = "Update the fit-plot"),
                 plotOutput("Model_plot")),
        tabPanel("Complete Modelled data",
                 dataTableOutput("Complete_model_data")))
      )
      # end of Tab3
    ),
    
    
    # Tab#4 
    tabPanel("Data curation", icon = icon("gavel"),
             sidebarPanel(
               fluidRow(
                helpText("Here you can have a look at the data and remove the values that are odd or missing... 
                         if we can fix some issues at least"),
                         br(),
                uiOutput("time_cast"),
                actionButton("Go_outliers", label = "Unleash outlier selection tool"),  
                br(),
                br(),
                
                helpText("Select the outliers based on"),
              # >>> Here should be a button for selecting the method of outlier selection
              # We should include methods such as X * StDev, 1.5* IQR
              # And also provide a table where the outliers are highlighted in a different colour
               # selectizeInput("outlier_method", options = c("1xStDev from the median" = "1SD", "2xStDev from the median" = "2SD", "2.5xStDev from the median" = "2.5SD", "3xStDev from the median"="3SD", "1.5*IQR away from the mean" = "1.5IQ"))
              br(),
              br(),
              helpText("Would you like to remove the rows containing missing values?"),
              actionButton("Go_omitna", label = "Omit rows with NA")  
              )),
             mainPanel(
               navbarPage("",
                tabPanel("original data", icon=icon("folder"),
                         dataTableOutput("Outlier_data_table")),
                tabPanel("outliers", icon=icon("bug"),
                         dataTableOutput("Outlier_data")),
                tabPanel("outliers removed", icon=icon("birthday-cake"),
                         "no outliers data here")
             ))
    # end of Tab#4         
    ),
    
    #Tab#5
    tabPanel("Data exploration", icon=icon("binoculars"),
             sidebarPanel(
               fluidRow(
                 uiOutput("CustomSumm"), ### <<< Added this,   Hashed out selectize  below       %% Mitch %%
                 actionButton("Go_SummaryStat", label = "unleash Summary Statistics"),
                 uiOutput("Sum_download_button"),
                 uiOutput("HisIV"),
                 uiOutput("HisDV")
               )),
             
             mainPanel(
               tabsetPanel(
                 tabPanel("summary data", icon=icon("flask"),
                          dataTableOutput("sum_data"),
                          textOutput("total_na")),
                 
                 tabPanel("Histograms", icon=icon("area-chart"),
                          uiOutput("HistType"),
                          plotlyOutput("HistPlot")
                          ),
                 
                 tabPanel("Boxplots", icon=icon("sun-o"),
                         # actionButton("Go_Boxplot", label = "Plot boxplots"),
                          plotlyOutput("Boxes")),
                 
                 tabPanel("ANOVA plots", icon=icon("snowflake-o")),
                 tabPanel("Variance analysis")
                  ))
  # end of Tab#5
    ),
    
     
    # Tab #6
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
    
    # Tab 7
    tabPanel(
      "PCA",
      icon = icon("object-group"),
      sidebarPanel(
        fluidRow(
          helpText("Which phenotype you would like to use for the PCA?"),
          uiOutput("PCA_Pheno_data"), # which phenotype data (summarized / na / original) selectize, multiple = F
          actionButton("Go_PCAdata", label = "set the dataset"),
          uiOutput("PCA_Select_pheno"), # which traits would you like to use? selectize, multiple = T
         # uiOutput("SelectGroup"), # How would you like to colour, selectize (input$SelectGeno, input$SelectDV, input$SelectTime, multiple = F)
         br(),
         actionButton("Go_PCA", label = "Unleash the PCA monster",icon = icon("play-circle")),
         
         # selectInput("Select the principle components", "Select the principle components:",
         radioButtons("radio", label = h6("Select the principle components to display"), 
         choices = list("PC1 and PC2" = 1, "PC2 and PC3" = 2, "PC1 and PC3" = 3), selected = 1)
          )),
      mainPanel(
        navbarPage("PCA the crazy",
            tabPanel("Selected dataset",
                dataTableOutput("PCA_raw_table")),
            tabPanel("Final data for PCA",
                     dataTableOutput("PCA_final_table")),
            tabPanel("Eigen Plot",
                    plotOutput("PCA_eigen_plot")),
            tabPanel("Contribution Plot",
                     plotlyOutput("PCA_contribution_plot")),
            tabPanel("Scatter Plot",
                     plotlyOutput("PCA_scatter_plot"))
            ))
      # end Tab 7
    ),
    
    # Tab 8
    tabPanel("Clustering",
      icon = icon("sitemap"),
      sidebarPanel(fluidRow(
        helpText("In here, you can perform a cluster analysis by grouping your data based on the phenotypic traits and validating the clusters"),
        navbarPage(  
          tabPanel("Select data",
            uiOutput("Select_data_cluster"),
            uiOutput("Select_phenotypes_cluster"),
            uiOutput("Select_cluster_method"),
            actionButton("Go_cluster", "Unleash cluster analysis")),
          tabPanel("Chose the clusters",
            helpText("Have a look at the dendrogram and choose the value at which you would like to split into individual clusters"),
            textInput("Split_cluster", "Enter the numeric value here")),
          tabPanel("Validate your clusters",
            helpText("Please choose the phenotype which you would like to further examine for the cluster validation"),
            uiOutput("Select_data_cluster_validation")
          ))        
        )),
      mainPanel(
      navbarPage("Cluster analysis",
          tabPanel("Cluster HOT HOT Heatmap",
                   "heatmap here"),
          tabPanel("Cluster dendrogram",
                   "dendrogram here"),
          tabPanel("Cluster validation",
                   "ANOVA charts here")
          ))
      # end of Tab #8
    )
    # end of App - final brackets
  )
)
