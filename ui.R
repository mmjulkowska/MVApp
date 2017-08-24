fluidPage(
  theme = shinytheme("yeti"),
  navbarPage(
    title = ">> MVApp_ss4 <<",
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
                 uiOutput("HisDV"),
                 actionButton("Go_PlotHist", label = "Plot histograms")
                 
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
                          plotlyOutput("Hiss"))))
             
             
             # end of Tab#4
    ),
    
    # Tab #5
    tabPanel(
      "Variation Examination",
      icon = icon("area-chart"),
      sidebarPanel(fluidRow(
        helpText("Something helpfull"),
        "Widgets here"
      )),
      mainPanel(
        "looking at the histograms",
        br(),
        "- test for normality",
        br(),
        "- test for equal variance",
        br(),
        "- first ANOVA? or other tests (like T-test)"
      )
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
            "Subset Correlations",
            "here another plot for subset correlations",
            plotOutput("corrplot2")
          )
        ),
        
        tabPanel(
          "Scatter plots",
          sidebarPanel("Widgets shait"),
          mainPanel(
            "R square value is XX and P value is XX",
            br(),
            "and here look at the correlation plot"
          )
        )
      )
      # end of Tab#6
    ),
    
    # Tab 7
    tabPanel(
      "PCA",
      icon = icon("object-group"),
      sidebarPanel(fluidRow(
        helpText("Something helpfull"),
        "widgets"
      )),
      mainPanel("PCA the crazy")
      # end Tab 7
    ),
    # Tab 8
    tabPanel(
      "Clustering",
      icon = icon("sitemap"),
      sidebarPanel(fluidRow(helpText("Something usefull"),
                            "widgets")),
      mainPanel("Cluster the phenotypes in different groups")
      # end of Tab #8
    )
    # end of App - final brackets
  )
)