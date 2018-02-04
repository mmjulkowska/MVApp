fluidPage(
  theme = shinytheme("yeti"),
  navbarPage(
    title = ">> MVApp <<",

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
        "The app is not published anywhere (yet - we are working on it), but if you wish to cite the app, please use the following:",
        br(), br(),
        "Julkowska, M.M., Saade, S., Aragwal, G., Gao, G., Morton, M.J.L., Awlia, M., Paillies, Y., Tester, M., MVApp.pre-release_v2.0 mmjulkowska/MVApp: MVApp.pre-release_v2.0, DOI: 10.5281/zenodo.1067974",
        br(), br(),
        "The example dataset is available ", a("here", href = "https://drive.google.com/file/d/0B08MX6N7rXcZX0xQamtweUZCbDQ/view?usp=sharing", target = "_blank"),br(), br(),
        "If you have any problems / questions / ", span("suggestions how we can improve this APP so that YOU can do your analysis smoother", style="color:red") ,"- or simply you would like to tell us how amazing the App is - please contact ",span("Magdalena.Julkowska@kaust.edu.sa", style="color:blue")
      ),
      mainPanel(
      h2("About the MVApp"),
      br(),
      "We are aiming streamline the analysis of experiments containing multiple phenotypical measurements of the same sample, but you can easily use the app even if you have one phenotype.",
      br(),
      "Using the MVApp is completely safe! The MVApp will not save any of your data on the server, ensuring that your rights to the uploaded data are perserved.",
      br(),
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
      h4("8. Perform Multidimentional Scaling to detect the patterns in your data based on the relationships between your samples"),
      br(),
      h4("9. Cluster your samples based on the selected traits and perform cluster validation analysis."),
      br(),
      h4("10. Use Quantile Regression to explore how individual phenotypes contribute to traits of major interest in different quantile classes"),
      br(),
      h3("User guide (still in making) is available ", a("HERE", href="https://mmjulkowska.github.io/MVApp/", target = "_blank"), "and you can leave the feedback ", a("HERE!!!", href = "https://goo.gl/forms/kpMdpMpswdbb9BJv2", target = "_blank")),
      br(),
      "Although our App is super cool and everything is now just ONE click away from you, please remember that the final output will depend on your data - as they say", tags$b(">> rubbish in - rubbish out <<"), ", but of course we hope your data is all glam, glitter and rainbows! Happy Data analysis!",
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
          fileInput(
            "your_data",
            label = "Upload your file",
            accept = c('text / csv', '.csv', 'text/comma-separated-values')
          ),
          "If your data is too large, try running the MVApp locally - click ", a("HERE", href="https://mmjulkowska.github.io/MVApp/", target = "_blank"), " to see how",
          br(),br(),
          uiOutput("CustomGeno"),
          uiOutput("CustomIndepVar"),
          uiOutput("CustomPheno"),
          checkboxInput("IdCheck", label = "Data contains Sample ID information?", value = F),
          uiOutput("CustomID"),
          checkboxInput("TimeCheck", label = "Data contains Time / gradient that can be used for modeling", value = F),
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
                uiOutput("Pheno_to_model"),
                uiOutput("IV_to_model"),
                uiOutput("IV_subset_model"),
                actionButton("Go_HelpModel", label = "Unleash model estimation"),
                selectInput("model",
                            label = "Select method for modeling",
                            choices = list(
                              "linear" = "lin",
                              "quadratic" = "quad",
                              "exponential" = "exp",
                              "square root" = "sqr",
                              "cubic spline" = "cubic",
                              "smoothed spline" = "smooth")),
                uiOutput("if_cubic_knots"),
                uiOutput("Spline_df_select"),
                uiOutput("Spline_user_df"),
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
                                  dataTableOutput("Model_data"),
                                  uiOutput("Download_good_r2_table_button"),
                                  checkboxInput("Remove_samples_based_on_r2", label = "Lock the data with removed outliers based on poor r2 fit (< 0.7) for further analysis"),
                                  dataTableOutput("Good_r2_table")),
                         tabPanel("Fit-Plot",
                                  column(4, uiOutput("Select_model_plot_type"),
                                         uiOutput("Go_fitplot_model"),
                                            uiOutput("Model_fit_graph_download_button")),
                                  column(4, uiOutput("Select_modelPlot")),
                                  column(4, uiOutput("Model_graph_fit_select_multi_input")),
                                  column(4, uiOutput("Fit_plot_slider_input")),
                                  plotOutput("Fit_plot_only_graph",height = 750)),
                         tabPanel("Examine differences",
                                  verbatimTextOutput("model_comparison_report"),
                                  uiOutput("downl_plot_MCP_ui"),
                                  plotlyOutput("model_comparison_plotski"),
                                  hr(),
                                  
                                  column(4, uiOutput("Select_model_trait_to_plot"),
                                         uiOutput("Model_Selection_of_colors"),
                                         uiOutput("Select_number_of_colors"),
                                         uiOutput("Select_portion_of_color")),
                                  column(4, uiOutput("Select_model_color_to_plot"),
                                         uiOutput("Select_model_facet_to_plot"),
                                         #uiOutput("Select_model_color_scale_to_plot"),
                                         uiOutput("Select_model_background_color_to_plot"),
                                         uiOutput("Select_model_maj_grid_to_plot")),
                                  column(4, numericInput(
                                    inputId = "Model_threshold",
                                    label = "Select the p-value threshold",
                                    value = 0.05),
                                    uiOutput("Select_model_graph_to_plot"),
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
                           tabPanel("Outlier selection",            
                                    selectizeInput("Outlier_on_data", label = "Use the following dataset for outlier selelction", 
                                                   choices =c("raw data", "r2 fitted curves curated data", "missing values removed data", "r2 fitted and missing values removed data")),
                                    br(),
                                    uiOutput("IV_outliers_selection"),
                                    selectizeInput("Out_pheno_single_multi", label = "Select outliers based on", choices=c("All phenotypes", "Single phenotype"), multiple = F),
                                    
                                    selectizeInput("outlier_method", label="Method for the outlier selection", 
                                                   choices = list(
                                                     "1.5*IQR away from the mean (default)",
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
                                    actionButton("Go_outliers", label = "Unleash outlier highlighter"),  
                                    hr(),
                                    uiOutput("Outliers_save")),
                           
                           tabPanel("Tweak the graphs",
                                    uiOutput("Pheno_graph_outliers"),
                                    radioButtons("outlier_graph_type", "Type of graph", choices = c("box plot", "scatter plot", "bar plot")),
                                    uiOutput("Outlier_error_bar"),
                                    br(),
                                    uiOutput("Outlier_Selection_of_colors"),
                                    uiOutput("Select_number_of_colors_outl"),
                                    uiOutput("Select_portion_of_color_outl"),
                                    br(),
                                    checkboxInput("outlier_colour", "Color code Independent Variables?"),
                                    uiOutput("Q_colour"),
                                    checkboxInput("outlier_facet", "Split the graph?"),
                                    uiOutput("Q_facet"),
                                    uiOutput("Facet_user_input_columns"),
                                    uiOutput("Facet_outlier_scale"),
                                    uiOutput("Select_outlier_background_color_to_plot"),
                                    uiOutput("Select_outlier_maj_grid_to_plot"))
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
                         tabPanel("Graph containing outliers", icon=icon("bug"),
                                  uiOutput("downl_plot_OutlPlot_ui"),
                                  plotlyOutput("outlier_graph"),
                                  br(),
                                  uiOutput("Pheno_outlier_download"),
                                  br(),
                                  dataTableOutput("Outlier_only_table")),
                         
                         tabPanel("Graph with outliers removed", icon=icon("birthday-cake"),
                                  uiOutput("downl_plot_NoOutlPlot_ui"),
                                  plotlyOutput("no_outliers_graph"),
                                  br(),
                                  uiOutput("Pheno_outlier_free_download"),
                                  br(),
                                  dataTableOutput("Outlier_free_table")),
                         tabPanel("Summary data", icon=icon("flask"),
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
      navbarPage("",
        tabPanel("Numerical data analysis",
         sidebarPanel(
           fluidRow(
             uiOutput("Histo_Pheno_data"),
             uiOutput("HisIV"),
             uiOutput("HisDV"),
             uiOutput("Chosenthreshold"),
             #uiOutput("Chosenmultipletest"),
             checkboxInput("plot_facet", "Split the graph?"),
             uiOutput("Plotfacets")
           )),
         mainPanel(
           navbarPage("",
                      tabPanel("Testing normal distribution", icon=icon("area-chart"),
                               uiOutput("HistType"),
                               downloadButton("download_HistPlot", "Download plot"),
                               plotlyOutput("HistPlot", width = 1000),
                               br(),
                               verbatimTextOutput("Shapiro"),
                               br(),
                               column(4, checkboxInput("showShapirotest", "Would you like to see detailed Shapiro-Wilk test and QQplots?")),
                               column(4, uiOutput("QQplot_slider")),
                               column(4, uiOutput("QQplot_slider2")),
                               column(12,plotOutput("QQplot", height=1000))), 
                      
                      tabPanel("Testing equal variance", icon=icon("area-chart"),
                               uiOutput("downl_Variance_ui"),
                               uiOutput("subset_Variance"), 
                               plotOutput("Var_graph"),
                               verbatimTextOutput("Bartlett"),
                               verbatimTextOutput("Levene")
                           ),
                      tabPanel("One / two sample tests", icon = icon("hand-peace-o"),
                               column(3,uiOutput("OT_test")),
                               column(3,uiOutput("OT_what_mu")),
                               column(3,uiOutput("OT_grouping_IVs")),
                               column(3,uiOutput("OT_which_compare")),
                               column(12,verbatimTextOutput("OT_test_results")),
                               column(12,uiOutput("OT_graph_download_ui")),
                               column(12,plotlyOutput("OT_graph"))),
                      tabPanel("Testing significant differences", icon=icon("sun-o"),
                               selectizeInput(inputId = "Sig_diff_test", 
                                              label = "Test significant differences with:",
                                              choices = c("ANOVA (parametric)", "Kruskal-Wallis (non-parametric)")),
                               verbatimTextOutput("ANOVAtest"),
                               uiOutput("ANOVA_graph_download_ui"),
                               plotlyOutput("Boxes"),
                               verbatimTextOutput("Tukeylisting")),
                      tabPanel("two-way ANOVA", icon=icon("snowflake-o"),
                              column(4, uiOutput("TWANOVA_IV1")),
                              column(4, uiOutput("TWANOVA_IV2")),
                              column(4, actionButton("Go_TWANOVA", "Unleash two way ANOVA")),
                              column(12, 
                                     downloadButton("TW_ANOVA_graph_download", "Download plot"),
                                     plotOutput("TW_ANOVA_interaction_plot")),
                              verbatimTextOutput("two_ANOVA_report"),
                              plotOutput("TW_ANOVA_QQ_plot"))
           ))),
      tabPanel("Categorical data analysis",
               sidebarPanel(
                 "gadgets",
                 "data_input",
                 "Select Categorical variable",
                 "Select p-value threshold"
               ),
               mainPanel(
                 navbarPage("",
                   tabPanel("Mosaic plots",
                            "include 3-way contingency tables", br(), br(),
                            "This tab is currently under construction - main contributor is Mitchell Morton with sateline help from Magdalena Julkowska", br(),
                            "Please be patient and send us an e mail in case you have some analysis you would like to include in the App", br(), br(),
                            "Chers & glittes data-analyst!"),
                   tabPanel("Two-way ANOVA?",
                            "can we do it? Really???",
                            br(), br(),
                            "This tab is currently under construction - main contributor is Mitchell Morton with sateline help from Magdalena Julkowska", br(),
                            "Please be patient and send us an e mail in case you have some analysis you would like to include in the App", br(), br(),
                            "Chers & glittes data-analyst!")
                 ))))
         # end of Tab#5
),

# Tab 6 = = = = = = = = = = = = = = >> CORRELATION ANALYSIS << = = = = = = = = = = = = = = = = = =

tabPanel("Correlations",
  icon = icon("compress"),
  navbarPage("",
    tabPanel(
      "Correlation Plot",
      sidebarPanel(
        uiOutput("cor_Pheno_data"),
        # which data set to use (summarized / na / original) selectize, multiple = F
        checkboxInput("cor_data_subset", label = "Subset your data for correlation analysis?"),
        uiOutput("cor_subset"),
        uiOutput("CorSpecIV_val"),
        checkboxInput("cor_sig_show", label = "Indicate non-significant correlations"),
        uiOutput("cor_sig_level_output"),
        selectInput("corMethod", "Correlation Method:", choices = c("pearson","spearman")),
        selectInput("corrplotMethod", "Plot Method:", choices = c("circle", "square", "ellipse", "number", "shade", "color", "pie")),
        selectInput("corType", "Plot Type:", choices = c("full", "lower", "upper")),
        selectInput("corOrder", "Order the lables by:", choices = list("Original order" = "original","Angular order of eigenvectors" = "AOE", "First Principal Component order"  = "FPC", "Hierarchical clustering order"  = "hclust", "Alphabetical order" = "alphabet")),
        numericInput("Cor_Big_steps", "Number of levels:", value = 10, min = 3, max = 10),
        selectizeInput("Cor_color_palette", "Palette:", choices = list("Spectral" = "Spectral", "Red-Yellow-Green" = "RdYlGn", "Red-Yellow-Blue" = "RdYlBu", "Red-Grey" = "RdGy", "Red-Blue" = "RdBu", "Purple-Orange" = "PuOr", "Purple-Green" = "PRGn", "Pink-Green" = "PiYG", "Brown-Blue-Green" = "BrBG"))
        #actionButton("Go_table", label = "Click to see the correlation table with p value", icon = icon("play-circle"))
      ),
      mainPanel(
        verbatimTextOutput("tricky_table"),
        downloadButton("download_corrplot", label="Download Plot"),
        plotOutput("corrplot"),
        uiOutput("cortable_button"),
        verbatimTextOutput("cor_table_text"),
        dataTableOutput("cor_table"))
    ),
    tabPanel(
      "Scatterplots",
      sidebarPanel(uiOutput("Pheno1"), uiOutput("Pheno2"), uiOutput("colorby")),
      mainPanel(
        textOutput("corrsq"),
        textOutput("corpval"),
        uiOutput("downl_scatter_corr_ui"),
        plotlyOutput("scatterplot")
      )))
  # end of Tab#6
),
# Tab 7 = = = = = = = = = = = = = = >> PCA ANALYSIS << = = = = = = = = = = = = = = = = = = 

tabPanel("Reduction of dimentionality", icon = icon("object-group"),
navbarPage("",
  tabPanel("PCA",
  sidebarPanel(
    fluidRow(
      uiOutput("PCA_Pheno_data"), # which phenotype data (summarized / na / original) selectize, multiple = F
      actionButton("Go_PCAdata", label = "set the dataset"),
      uiOutput("PCA_Select_pheno"), # which traits would you like to use? selectize, multiple = T
      checkboxInput("PCA_Scale_Q", "Scale the data?"),
      selectizeInput("PCA_data_subset", label = "Dataset", choices=c("full dataset", "subsetted dataset")),
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
                        uiOutput("downl_PCA_eigen_plot_ui"),
                        plotOutput("PCA_eigen_plot"),
                        uiOutput("Eigen_download_button"),
                        dataTableOutput("Eigen_data_table")),
               tabPanel("Contribution of variables",
                        uiOutput("downl_PCA_contribution_plot_ui"),
                        plotOutput("PCA_contribution_plot"),
                        hr(),
                        column(4,uiOutput("PCA1_select")),
                        column(4,uiOutput("PCA2_select")),
                        column(4, uiOutput("PCA_colorby")),
                        column(12,uiOutput("downl_PCA_scatterplot_ui"), 
                                  plotlyOutput("PCA_scatterplot"),
                                  uiOutput("Coord_download_ind"),
                                  dataTableOutput("PCA_coordinates_ind"))),
               tabPanel("Contribution per PC",
                        uiOutput("PCA_contrib_select"),
                        uiOutput("downl_Contrib_trait_plot_ui"),
                        plotOutput("Contrib_trait_plot"),
                        uiOutput("Contrib_download_var"),
                        dataTableOutput("PCA_contribution_var"))
    ))),
  tabPanel("Multidimensional Scaling",
           sidebarPanel(
             
             fluidRow(
               uiOutput("MDS_Pheno_data"), #
               actionButton("Go_MDSdata", label = "Set the data for MDS"),
               uiOutput("MDS_Select_pheno"), #
               checkboxInput("MDS_Scale_Q", "Scale the data?"),
               selectizeInput("MDS_subset_Q", label = "Perform MDS on:", choices=c("Full dataset", "Subsetted dataset")),
               uiOutput("MDS_subset_trait"),
               uiOutput("MDS_subset_specific"),
               
               br(),
               checkboxInput("MDS_KMC_Q", label = "Cluster the samples using k-means?"),
               uiOutput("MDS_KMC_number"),
               actionButton("Go_MDS", label = "Unleash the power of Multidimensional Scaling"))),
           
           
           mainPanel(
             navbarPage("MDS",
                        tabPanel("Selected dataset",
                                 dataTableOutput("MDS_raw_table")),
                        tabPanel("Final dataset for MDS",
                                 dataTableOutput("MDS_final_table")),
                        tabPanel("MDS of the samples",
                                 downloadButton("MDS_plot_download", "Download plot"),
                                 plotlyOutput("MDS_sample_graph"),
                                 uiOutput("MDS_download_samples"),
                                 dataTableOutput("MDS_table_samples")),
                        tabPanel("Scaling of the Dependent Variables",
                                 downloadButton("MDS_plot_download_transposed", "Download plot"),
                                 plotOutput("MDS_sample_graph_transposed")
                                 #uiOutput("MDS_download_transposed"),
                                 #dataTableOutput("MDS_sample_table_transposed_dt")
                        )
             ))))
  # end Tab 7
),

# Tab 8 = = = = = = = = = = = = = = >> CLUSTERING ANALYSIS << = = = = = = = = = = = = = = = = = = 

tabPanel("Clustering", icon = icon("sitemap"),
      navbarPage("",
      tabPanel("Hierarchical clustering", 
         
         sidebarPanel(
                                 uiOutput("Select_data_cluster"),
                                 uiOutput("Select_phenotypes_cluster"),
                                 checkboxInput("Cluster_pre_calc", label = "Perform cluster analysis on mean values?"),
                                 checkboxInput("Cluster_subset_Q", label = "Subset the data for cluster analysis?"),
                                 uiOutput("Cluster_subset_trait"),
                                 uiOutput("Cluster_subset_specific"),
                                 uiOutput("Select_clustering_method"),
                                 actionButton("Go_cluster", "Unleash cluster analysis"),
                                 hr(),
                                 numericInput("Split_cluster", "Separate clusters at:", value = 4)),
         mainPanel(
           navbarPage("Happy clustering",
                      tabPanel("Clustering your HOT HOT Data",
                               #dataTableOutput("Data_cluster_table"),
                               #dataTableOutput("Final_cluster_table"),
                               uiOutput("HotHeatMap_ui"),
                               plotOutput("HotHeatMap"),
                               br(),
                               hr(),
                               verbatimTextOutput("Dendro_sentence"),
                               uiOutput("ClusterTree_ui"),
                               plotOutput("ClusterTree"),
                               uiOutput("Cluster_download_button"),
                               br(),
                               dataTableOutput("Cluster_table")),
                               
                      tabPanel("Cluster validation",
                               verbatimTextOutput("HotAnovaNews"),
                               uiOutput("HotANOVA_ui"),
                               plotOutput("HotANOVA"),
                               hr(),
                               column(4, uiOutput("Select_data_cluster_validation")))
           ))),
      tabPanel("K-means Clustering", icon = icon("barcode"),
            sidebarPanel(
                          uiOutput("Select_data_K_mean_cluster"),
                          uiOutput("Select_DV_KMC"),
                          checkboxInput(inputId = "KMCluster_scale_Q", label = "Scale the data prior to clustering"),
                          checkboxInput("KMC_use_means", label = "Perform K-means cluster analysis on mean values?"),
                          actionButton(inputId= "Select_data_KMC", label = "Set the dataset"),
                          br(),br(),
                          actionButton(inputId="Go_KMClustering_advise", label = "Unleash optimal cluster number estimation"),
                          hr(),
                          uiOutput("Select_numer_of_cluster_to_perform"),
                          numericInput("kmclusters", "Cluster number", 3,
                                       min = 1, max = 9),
                          actionButton(inputId="Go_KMClustering", label = "Unleash k-means clustering")
                        ),
                        
             mainPanel(
                          navbarPage("KMC",
                                     tabPanel("Optimal number of clusters",
                                              dataTableOutput("KMC_data_table"),
                                              verbatimTextOutput("indices_majority_KMC"),
                                              #dataTableOutput("KMCluster_test"),
                                              column(12,  uiOutput("downl_indices_plots_KMC_3_ui"),
                                                          plotOutput("indices_plots_KMC_3")),
                                              column(6, uiOutput("downl_elbow_graph_KMC_ui"),
                                                        plotOutput("elbow_graph_KMC")),
                                              column(6, uiOutput("downl_silhouette_graph_KMC_ui"),
                                                        plotOutput("silhouette_graph_KMC"))
                                              #plotOutput("gapstat_graph_KMC"),
                                             # This worked with Yveline but isnt working now - dont know why - I ll hash it for now 
                                              # column(12,uiOutput("downl_indices_plots_KMC_1_ui"),
                                             #           plotOutput("indices_plots_KMC_1")),
                                              # column(12, uiOutput("downl_indices_plots_KMC_2_ui"),
                                              #            plotOutput("indices_plots_KMC_2"))
                                     ),
                                     tabPanel("K means clustering results",
                                              column(4, uiOutput("Select_KMC_trait")),
                                              column(4, uiOutput("facet_barplot_of_KMC"),
                                                        uiOutput("Select_KMC_facet_barplot")),
                                              column(4, uiOutput("Select_KMC_scale_barplot"),
                                              #actionButton(inputId= "Show_table", label = "Show barplot table"),
                                              #dataTableOutput("KMC_test1"),
                                                        uiOutput("Select_KMC_background_barplot"),
                                                        uiOutput("Select_KMC_grid_barplot")),
                                              column(12,uiOutput("downl_kmeans_barplots_ui")),
                                              column(12,plotlyOutput("kmeans_barplots")),
                                              hr(),
                                              column(4,uiOutput("xcol_kmeans_scatter"),
                                                       uiOutput("ycol_kmeans_scatter")),
                                              column(4,uiOutput("facet_scatterplot_of_KMC"),
                                                       uiOutput("Select_KMC_facet_to_plot")),
                                              column(4,uiOutput("Select_KMC_facet_scale"),
                                                       uiOutput("Select_KMC_background_to_plot"),
                                                       uiOutput("Select_KMC_grid_to_plot")),
                                              #dataTableOutput("KMC_test2"),
                                              column(12, uiOutput("downl_kmeans_scatter_plot_ui")),
                                              column(12,plotlyOutput("kmeans_scatter_plot")),
                                              column(12,uiOutput("downl_KMC_test_ui")),
                                              dataTableOutput("KMC_test")
                                     )
                                     #   tabPanel("Cluster validation",
                                     #         verbatimTextOutput("kmcAnovaNews"),
                                     
                                     #   plotOutput("kmcANOVA"),
                                     #   hr(),
                                     #     column(4, uiOutput("Select_data_cluster_validation")))
                                     
                          ))         
               ))
         # end of Tab #8
),

# Tab 9 = = = = = = = = = = = = = = >> HERITABILITY << = = = = = = = = = = = = = = = = = = 

tabPanel("Heritability", icon=icon("pagelines"),
         
         sidebarPanel("",
                      fluidRow(
                        uiOutput("Herit_Pheno_data"),
                        uiOutput("YearID"),
                        uiOutput("LocationID"),
                        numericInput("RepID", label="Type in the number of replications per location per year", value = 4, min=1),
                        uiOutput("HeritabilityDV"),
                        checkboxInput("herit_facet", "Would you like to subset the data?"),
                        uiOutput("Heritfacets")
                      )),
         mainPanel(
                               dataTableOutput("Heri_table"),
                               verbatimTextOutput("HeritValue")
                               
                      )
         # end of Tab #9 
),                           

# Tab 10 = = = = = = = = = = = = = = >> QUANTILE ANALYSIS << = = = = = = = = = = = = = = = = = = 

tabPanel("Quantile regression", icon = icon("paper-plane-o"),
         sidebarPanel(
           fluidRow(
             uiOutput("QA_data_type"),
             uiOutput("Pheno_Response"),
             uiOutput("QA_subset_trait"),
             uiOutput("Pheno_explanatory"),
             uiOutput("p_value_thresh"),
             checkboxInput("Scale_QA", label = "Scale the data for quantile analysis?"),
             actionButton("Go_data", label = "Click to set the data ")
           )), # end of sidebar panel
         mainPanel(
           navbarPage("Analyze it",
                      tabPanel("Selected Dataset", 
                               dataTableOutput("QA_raw_table")
                      ),
                      tabPanel("Final data for analysis", 
                               dataTableOutput("QA_final_table")
                      ),
                      tabPanel("Modelled data", 
                               uiOutput("Choose_subset_specific"),
                               #actionButton("Go_QA", label = "Unleash model estimation"),
                               verbatimTextOutput("significant_variables"),
                               uiOutput("table_download_button"),
                               dataTableOutput("Result_table")
                      ),
                      
                      tabPanel("Quantile plots", 
                               fluidRow(
                                 column(4,uiOutput("Group_plot")),
                                 column(4,uiOutput("subset_plot"))
                               ),
                               br(),
                               
                               fluidRow(
                                 column(4,uiOutput("model_plot_type")),
                                 column(4,uiOutput("Select_model_variable")),
                                 column(4,uiOutput("QA_plot_slider_input"))
                                 
                               ),
                               actionButton("Go_plot", label = "View plot(s):"),
                               
                               plotOutput("QA_plot", height = 425),
                               downloadButton("downl_plot_QA", "Download plot")
                               #          br(),
                               #           uiOutput("QA_plot_download")
                               
                      )
           )  
         ) # end of main panel
         
) # end of quantile tab

# end of App - final brackets
  )
)
