fluidPage(
  theme = shinytheme("yeti"),
  navbarPage(
    title = ">> MVApp <<",

    tabPanel(
      "Background information",
      icon = icon("info"),
      h2("About the App"),
      "Here we will put some helpfull and encouraging text later, refering to the paper that will be (by then) published in high impact and open access journal ;) For now, we leave this bit as is"
      # end of Tab1
    ),
    
    tabPanel(
      "Upload your data",
      icon = icon("table"),
      sidebarPanel(
        fluidRow(
          helpText("Upload your data or chose one of the example datasets:"),
          fileInput(
            "your_data",
            label = "Upload your file",
            accept = c('text / csv', '.csv', 'text/comma-separated-values')
          ),
          uiOutput("CustomGeno"),
          uiOutput("CustomIndepVar"),
          uiOutput("CustomPheno"),
          checkboxInput("IdCheck", label = "Do you have a column with Sample ID", value = F),
          uiOutput("CustomID"),
          checkboxInput("TimeCheck", label = "Do you have a column with Time values", value = F),
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
          helpText("Please select which phenotype you would like to use for modeling"),
          uiOutput("Pheno_to_model"),
          uiOutput("IV_to_model"),
          uiOutput("IV_subset_model"),
          helpText("Here we can select what kind of modeling you want to do"),
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
          actionButton("Go_Model", label = "Unleash the model", icon = icon("play-circle"))
        )
      ),
      # end of side Panel
      mainPanel(
        "here we will have the goodness of fit values - such as r-squared and p-values for the chosen model",
        br(),
        "AND There will be a plot here",
        dataTableOutput("Model_data")
      )
      # end of Tab3
    ),
    
    
    # Tab#4
    tabPanel("Data curation", icon=icon("check"),
             sidebarPanel(
               fluidRow(
                 helpText("Remove pesky NAs"),
                 actionButton("Go_omitna", label = "Omit rows with NA"),
                 #helpText("Restore pesky NAs"),
                 #actionButton("Go_restorena", label = "Restore rows with NA"),
                 uiOutput("CustomSumm"), ### <<< Added this,   Hashed out selectize  below       %% Mitch %%
                 #selectizeInput("SummTest", label="Select what you want to calculate", choices=c("Mean", "Median", "StdDev", "StdError", "Min", "Max"), multiple=T),
                 actionButton("Go_SummaryStat", label = "unleash Summary Statistics"),
                 uiOutput("HisIV"),
                 uiOutput("HisDV")
               )),
             
             
             #mainPanel(
             #tableOutput("sum_data"),
             #textOutput("total_na"))
             
             
             
             mainPanel(
               tabsetPanel(
                 tabPanel("summary data", icon=icon("flask"),
                          tableOutput("sum_data"),
                          textOutput("total_na")),
                 
                 tabPanel("Histograms", icon=icon("magic"),
                          uiOutput("HistType"),
                          plotlyOutput("HistPlot")
                          ),
                 
                 tabPanel("Boxplots", icon=icon("magic"),
                          #actionButton("Go_Boxplot", label = "Plot boxplots"),
                          plotlyOutput("Boxes")),
                  tabPanel("Table with outliers", icon=icon("magic"),
                           actionButton("Go_Outliers", label = "Table of outliers"),
                    dataTableOutput("Outlier_data")
                  
                 #         textOutput("total_outliers"))
                        )))
    ),
             
             
             # end of Tab#4
  
   
               
    
       
    # Tab #5
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
            "Subset Correlations",
            "here another plot for subset correlations",
            plotOutput("corrplot2")
          )
        ),
        
        tabPanel(
          "Scatter plots",
          sidebarPanel(uiOutput("Pheno1"),uiOutput("Pheno2"),uiOutput("colorby")),
          mainPanel(
            "The R square value is XX and P value is XX",
            plotlyOutput("scatterplot")
          )
        )
      )
      # end of Tab#5
    ),
    
    # Tab 6
    tabPanel(
      "PCA",
      icon = icon("object-group"),
      sidebarPanel(
        fluidRow(
          helpText("Please select which phenotype you would like to use for the PCA"),
          uiOutput("PCA_Pheno_data"), # which phenotype data (summarized / na / original) selectize, multiple = F
          actionButton("Go_PCAdata", label = "set the dataset"),
          
          uiOutput("PCA_Select_pheno"), # which traits would you like to use? selectize, multiple = T
          # user esthetics to differentiate between different geno / treatment
         # uiOutput("SelectGroup"), # How would you like to colour, selectize (input$SelectGeno, input$SelectDV, input$SelectTime, multiple = F)
          actionButton("Go_PCA", label = "Unleash the PCA monster",icon = icon("play-circle"))
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
                     plotOutput("PCA_contribution_plot")),
            tabPanel("Scatter Plot",
                     plotOutput("PCA_scatter_plot"))
            ))
      # end Tab 6
    ),
    # Tab 7
    tabPanel(
      "Clustering",
      icon = icon("sitemap"),
      sidebarPanel(fluidRow(helpText("Something usefull"),
                            "widgets")),
      mainPanel("Cluster the phenotypes in different groups")
      # end of Tab #7
    )
    # end of App - final brackets
  )
)
