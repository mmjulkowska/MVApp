fluidPage(
  theme = shinytheme("yeti"),
  navbarPage( ">> MVApp <<",

   # Tab 1 = = = = = = = = = = = = = = >> BACKGROUND INFORMATION << = = = = = = = = = = = = = = = = = = 
   
   tabPanel(
      "Background information",
      icon = icon("info"),
      sidebarPanel(
        h2("About us"),
        "This App is a result of hard work of a KAUST team, originating from ",a("The Salt Lab", href="https://cda.kaust.edu.sa/pages/Salt%20Lab.aspx", target = "_blank"), "lead by ", a("Prof. Mark Tester.", href="https://scholar.google.com/citations?user=FTvzOtkAAAAJ&hl=en", target= "_blank") ,  br(),
        br(),
        "This app is meant to streamline the data analysis that is common in many biological studies - especially when screaning large populations such as diversity panels or comparing multiple mutant lines to wild type.", 
        "Our background is plant biology - so you know where our bias is ;).",br(),br(),
        "If you have any problems / questions / ", span("suggestions how we can improve this APP so that YOU can do your analysis smoother", style="color:red") ,"- or simply you would like to tell us how amazing the App is - please contact ",span("Magdalena.Julkowska@kaust.edu.sa", style="color:blue")
      ),
      mainPanel(
      h2("About the MVApp"),
      br(),
      "We are aiming streamline the analysis of experiments containing multiple phenotypical measurements of the same sample, but you can easily use the app even if you have one phenotype.",
      
      h3("Our App empowers you to easily perform:"),
      br(),
      h4("1. Fitting the curves using simple functions (linear, quadratic, exponential and square root) as well as by fitting cubic and smoothed splines"),
      br(),
      h4("2. Automatically detect the outliers based on all traits or single trait"),
      br(),
      h4("3. Perform summary statistics on the data with / without the outliers"),
      br(),
      h4("4. Automatically determine whether your data is normally distributed and the variances between your samples are equal"),
      br(),
      h4("5. Examine your data for significant effects of the Genotype, Treatment or any other independent variable you wish"),
      br(),
      h4("6. Examine the correlations for all traits as well as for subsets of your data and easily determine the correlations that are changing depending on the Genotype, Treatment or any other selected independent variable."),
      br(),
      h4("7. Perform PCA analysis, examine which traits are contributing significantly to the most informative PCs and retrieve the coordinates of your samples."),
      br(),
      h4("8. Cluster your samples based on the selected traits and perform cluster validation analysis."),
      br(),
      br(),
      "Although our App is super cool and everything is now just ONE click away from you, please remember that the final output will depend on your data - as a great philosopher once said", tags$b(">> bullshit in - bullshit out <<"),
      br(),
      br(),
      img(src="bullshit_out.jpg", align="center"))
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
                                    helpText("Please select which phenotype you would like to model"),
                                    uiOutput("Pheno_to_model"),
                                    uiOutput("IV_to_model"),
                                    uiOutput("IV_subset_model"),
                                    actionButton("Go_HelpModel", label = "Unleash the model estimation"),
                                    helpText("If you have more than 10 time points it is worthwhile to consider fitting the polynomial functions to your data",br(),"Cubical splines where you determine the breakpoints yourself.", br()," By using smoothed splines - the breakpoints are determined automatically."),
                                    selectInput("model",
                                                label = "Select method",
                                                choices = list(
                                                  "linear" = "lin",
                                                  "quadratic" = "quad",
                                                  "exponential" = "exp",
                                                  "square root" = "sqr",
                                                  "cubic spline" = "cubic",
                                                  "smoothed spline" = "smooth")),
                                    uiOutput("if_cubic_knots"),
                                    actionButton("Go_Model", label = "Unleash the model", icon = icon("play-circle"))
                                    )),
            # end of side Panel
            mainPanel(
              navbarPage("",
                         tabPanel("Modelled data",
                                  verbatimTextOutput("best_model_advice"),
                                  dataTableOutput("Model_estimation"), 
                                  verbatimTextOutput("model_warning"),
                                  uiOutput("Model_download_button"),
                                  dataTableOutput("Model_data")),
                         tabPanel("Fit-Plot",

                                  column(4, uiOutput("Select_model_plot_type")),
                                  column(4, uiOutput("Select_modelPlot")),
                                  column(4, uiOutput("Model_graph_fit_select_multi_input")),
                                  column(4, uiOutput("Fit_plot_slider_input")),

                                  uiOutput("Go_fitplot_model"),
                                  plotOutput("Fit_plot_only_graph",height = 750)),
                         tabPanel("Examine differences",
                                  verbatimTextOutput("model_comparison_report"),
                                  plotlyOutput("model_comparison_plotski"),
                                  hr(),
                                  column(4, uiOutput("Select_model_trait_to_plot"), 
                                            uiOutput("Select_model_graph_to_plot")),
                                  column(4, uiOutput("Select_model_color_to_plot"),
                                            uiOutput("Select_model_facet_to_plot")),
                                  column(4, selectizeInput(
                                              inputId = "Model_threshold",
                                              label = "Select the p-value threshold",
                                              choices = c(0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.05, 0.1),
                                              selected = 0.05,
                                              multiple = F),
                                            uiOutput("Select_model_facet_scale"),
                                            uiOutput("Select_model_error_bar_to_plot")),
                                  hr(),
                                  column(12, verbatimTextOutput("model_comparison_Tukey"),
                                         uiOutput("Model_summ_download_button")),
                                  dataTableOutput("model_comparison_summary")
                                  )
                                  
                                  #plotOutput("Fit_plot_multi_graphs",  width = 1000))
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
                                 hr(),
                                 uiOutput("Outliers_save")),
                        
                        tabPanel("Outlier graphs tweaks",
                                 uiOutput("Pheno_graph_outliers"),
                                 radioButtons("outlier_graph_type", "Type of graph", choices = c("box plot", "scatter plot", "bar plot")),
                                 uiOutput("Outlier_error_bar"),
                                 br(),
                                 checkboxInput("outlier_colour", "would you like to colour-code one of your Indepentent Variables in the graph?"),
                                 uiOutput("Q_colour"),
                                 checkboxInput("outlier_facet", "would you like to facet the graph?"),
                                 uiOutput("Q_facet"),
                                 uiOutput("Facet_user_input_columns"),
                                 uiOutput("Facet_outlier_scale"))
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
                               column(6,uiOutput("Data_for_SummaryStats"),
                                        actionButton("Go_SummaryStat", label = "unleash Summary Statistics")), 
                               column(6, uiOutput("CustomSumm"),
                                      uiOutput("Sum_download_button")), 
                               hr(),
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
                      tabPanel("Normality test", icon=icon("area-chart"),
                               uiOutput("HistType"),
                               plotlyOutput("HistPlot", width = 600),
                               br(),
                               verbatimTextOutput("Shapiro"),
                               br(),
                               column(4, checkboxInput("showShapirotest", "Would you like to see detailed Shapiro-Wilk test and QQplots?")),
                               column(4, uiOutput("QQplot_slider")),
                               column(4, uiOutput("QQplot_slider2")),
                               column(12,plotOutput("QQplot", height=1000))
                      ), 
                      
                      tabPanel("Variance test", icon=icon("area-chart"),
                               verbatimTextOutput("Bartlett"),
                               verbatimTextOutput("Levene")
                               
                      ),
                      
                      #tabPanel("Variance analysis", icon=icon("bar-chart-o"),
                      #        verbatimTextOutput("Bartlett")
                      #        #verbatimTextOutput("Levene")
                      #),
                      
                      tabPanel("Boxplots", icon=icon("sun-o"),
                               # actionButton("Go_Boxplot", label = "Plot boxplots"),
                               
                               verbatimTextOutput("ANOVAtest"),
                               plotlyOutput("Boxes"),
                               #plotOutput("BoxesTukey", height=1000),
                               #dataTableOutput("Tukeylisting")
                               verbatimTextOutput("Tukeylisting")
                      )
                      
                      #tabPanel("ANOVA plots", icon=icon("snowflake-o"),
                      #        verbatimTextOutput("ANOVAtest"))
                      
           ))
         # end of Tab#5
),

# Tab 6 = = = = = = = = = = = = = = >> CORRELATION ANALYSIS << = = = = = = = = = = = = = = = = = =

tabPanel(
  "Establish correlations between traits",
  icon = icon("compress"),
  navbarPage("",
    tabPanel("General Correlations",
             plotOutput("corrplot")),
    tabPanel(
      "Correlation Plot",
      
      sidebarPanel(
        uiOutput("cor_Pheno_data"),
        # which data set to use (summarized / na / original) selectize, multiple = F
        checkboxInput("cor_data_subset", label = "Calculate correlation on specific subset of your data"),
        uiOutput("cor_subset"),
        uiOutput("CorSpecIV_val"),
        selectInput("corMethod", "Correlation Method", choices = c("pearson", "kendall", "spearman")),
        selectInput("corrplotMethod", "Plot Method", choices = c("circle", "square", "ellipse", "number", "shade", "color", "pie")),
        selectInput("corType", "Plot Type", choices = c("full", "lower", "upper")),
        selectInput("corOrder", "Order of the lable", choices = c("original", "AOE",  "FPC",  "hclust", "alphabet"))
        #actionButton("Go_table", label = "Click to see the correlation table with p value", icon = icon("play-circle"))
      ),
      
      
      mainPanel(
        verbatimTextOutput("tricky_table"),
        downloadButton('downloadCorrplot', icon("download")),
        plotOutput("corrplot"),
        dataTableOutput("cor_table")
      )
    ),
    
    tabPanel(
      "Scatterplots",
      sidebarPanel(uiOutput("Pheno1"), uiOutput("Pheno2"), uiOutput("colorby")),
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
      uiOutput("PCA_Pheno_data"), # which phenotype data (summarized / na / original) selectize, multiple = F
      actionButton("Go_PCAdata", label = "set the dataset"),
      uiOutput("PCA_Select_pheno"), # which traits would you like to use? selectize, multiple = T
      selectizeInput("PCA_data_avg", label = "Perform PCA on", choices=c("individual values", "average values per genotype / IVs / time")),
      selectizeInput("PCA_data_subset", label = "Would like to perform PCA on", choices=c("full dataset", "subsetted dataset")),
      uiOutput("PCA_subset_trait"),
      uiOutput("PCA_subset_specific"),
      
      # uiOutput("SelectGroup"), # How would you like to colour, selectize (input$SelectGeno, input$SelectDV, input$SelectTime, multiple = F)
      br(),
      actionButton("Go_PCA", label = "Unleash the PCA monster",icon = icon("play-circle"))
    )),
  mainPanel(
    navbarPage("PCA the crazy",
               tabPanel("Selected dataset",
                        dataTableOutput("PCA_raw_table")),
               tabPanel("Final data for PCA",
                        dataTableOutput("PCA_final_table")),
               tabPanel("Eigenvalues",
                        plotOutput("PCA_eigen_plot"),
                        uiOutput("Eigen_download_button"),
                        dataTableOutput("Eigen_data_table")),
               tabPanel("Contribution of variables",
                        uiOutput("PCA1_select"),
                        uiOutput("PCA2_select"),
                        plotOutput("PCA_contribution_plot")),
               tabPanel("Scatterplot of individuals",
                        uiOutput("PCA_colorby"),
                        plotlyOutput("PCA_scatterplot"),
                        uiOutput("Contrib_download_ind"),
                        dataTableOutput("PCA_contribution_ind")),
               tabPanel("Contribution per PC",
                        uiOutput("PCA_contrib_select"),
                        plotOutput("Contrib_trait_plot"),
                        uiOutput("Contrib_download_var"),
                        dataTableOutput("PCA_contribution_var"))
    ))
  # end Tab 7
),

# Tab 8 = = = = = = = = = = = = = = >> CLUSTERING ANALYSIS << = = = = = = = = = = = = = = = = = = 

tabPanel("Clustering",
         icon = icon("sitemap"),
         sidebarPanel(
                                 helpText("In here, you can perform a cluster analysis by grouping your data based on the phenotypic traits and validating the clusters"),
                                 uiOutput("Select_data_cluster"),
                                 uiOutput("Select_phenotypes_cluster"),
                                 checkboxInput("Cluster_pre_calc", label = "Check if you would like to perform cluster analysis on mean values"),
                                 checkboxInput("Cluster_subset_Q", label = "Check if you would like to subset the data before cluster analysis"),
                                 uiOutput("Cluster_subset_trait"),
                                 uiOutput("Cluster_subset_specific"),
                                 uiOutput("Select_cluster_method"),
                                 uiOutput("Select_clustering_method"),
                                 actionButton("Go_cluster", "Unleash cluster analysis"),
                                 hr(),
                                 textInput("Split_cluster", "Please enter a numaric value at which you would like to cut the dendrogram to segregate the samples into separate clusters.")),
         mainPanel(
           navbarPage("Cluster analysis",
                      tabPanel("Clustering your HOT HOT Data",
                               #dataTableOutput("Data_cluster_table"),
                               #dataTableOutput("Final_cluster_table"),
                               plotOutput("HotHeatMap"),
                               br(),
                               hr(),
                               verbatimTextOutput("Dendro_sentence"),
                               plotOutput("ClusterTree"),
                               uiOutput("Cluster_download_button"),
                               br(),
                               dataTableOutput("Cluster_table")),
                               
                      tabPanel("Cluster validation",
                               verbatimTextOutput("HotAnovaNews"),
                               
                               plotOutput("HotANOVA"),
                               hr(),
                               column(4, uiOutput("Select_data_cluster_validation")))
           ))
         # end of Tab #8
)
# end of App - final brackets
  )
)