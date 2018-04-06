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
        "Julkowska, M.M., Saade, S., Agarwal, G., Gao, G., Morton, M.J.L., Awlia, M., Paillies, Y., Tester, M., MVApp.pre-release_v2.0 mmjulkowska/MVApp: MVApp.pre-release_v2.0, DOI: 10.5281/zenodo.1067974",
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
                actionButton("Go_Model", label = "Unleash the model", icon = icon("play-circle")),
                hr(),
                numericInput("rsq_limit", label = "r2 value to be used as cut-off", value = 0.7)
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
                                  dataTableOutput("Good_r2_table")),
                         tabPanel("Fit-Plot",
                                  column(3, uiOutput("Select_model_plot_type"),
                                            uiOutput("Model_fit_graph_download_button")),
                                  column(3, uiOutput("Select_modelPlot"),
                                            uiOutput("Model_graph_fit_select_multi_input")),
                                  column(6, uiOutput("Fit_plot_slider_input")),
                                  column(12,plotOutput("Fit_plot_only_graph")),
                                  column(12,checkboxInput("show_fit_legend", "Show the figure legend"),
                                  uiOutput("Fit_plot_legend_show"))),
                         tabPanel("Examine differences",
                                  selectizeInput("ModelSum_data", label = "Use the following dataset for analysis", 
                                                 choices =c("raw data", "r2 fitted curves curated data")),
                                  verbatimTextOutput("model_comparison_report"),
                                  uiOutput("downl_plot_MCP_ui"),
                                  plotlyOutput("model_comparison_plotski"),
                                  hr(),
                                  column(12,checkboxInput("show_modelSum_legend", "Show the figure legend"),
                                         uiOutput("ModelSum_plot_legend_show")),
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
                                    selectizeInput("Out_pheno_single_multi", label = "Select outliers based on", choices=c("All phenotypes", "Some phenotypes", "Single phenotype"), multiple = F),
                                    
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
                                    selectInput("What_happens_to_outliers", label = "Points identified as outliers:", choices = c("replaced by NA", "removed together with entire row")),
                                    uiOutput("Pheno_outliers"),
                                    uiOutput("Outliers_selection_pheno"),
                                    br(),
                                    actionButton("Go_outliers", label = "Unleash outlier highlighter")),
                           
                           tabPanel("Tweak the graphs",
                                    uiOutput("Pheno_graph_outliers"),
                                    selectizeInput("outlier_graph_type", "Type of graph", choices = c("box plot", "scatter plot", "bar graph")),
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
                                  hr(),
                                  column(12,checkboxInput("show_bad_stuff_legend", "Show the figure legend"),
                                         uiOutput("bad_stuff_legend_show")),
                                  br(),
                                  uiOutput("Pheno_outlier_download"),
                                  br(),
                                  uiOutput("Outlier_only_table")),
                         
                         tabPanel("Graph with outliers removed", icon=icon("birthday-cake"),
                                  uiOutput("downl_plot_NoOutlPlot_ui"),
                                  plotlyOutput("no_outliers_graph"),
                                  hr(),
                                  column(12,checkboxInput("show_good_stuff_legend", "Show the figure legend"),
                                         uiOutput("good_stuff_legend_show")),
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
                               checkboxInput("plot_subs", "Subset the data?"),
                               uiOutput("Plotsubs"),
                               uiOutput("Plotsubs_choice"),
                               downloadButton("download_HistPlot", "Download plot"),
                               plotlyOutput("HistPlot", width = 1000),
                               br(),
                               #dataTableOutput("histo_crazy_table"),
                               br(),
                               checkboxInput("show_hist_legend", "Show the figure legend"),
                               uiOutput("legend_hist_show"),
                               br(),
                               verbatimTextOutput("Shapiro"),
                               br(),
                               column(4, checkboxInput("showShapirotest", "Would you like to see detailed Shapiro-Wilk test and QQplots?")),
                               column(4, uiOutput("QQplot_slider")),
                               column(4, uiOutput("QQplot_slider2")),
                               uiOutput("QQ_normal_download_ui"),
                               column(12,plotOutput("QQplot", height=1000)),
                               br(),
                               column(12,uiOutput("if_legend_QQ_show"))), 
                      
                      tabPanel("Testing equal variance", icon=icon("area-chart"),
                               column(4,uiOutput("subset_Variance")), 
                               column(4,checkboxInput("plot_subsVar", "Subset the data?"),
                               uiOutput("PlotsubsVar"),
                               uiOutput("Plotsubs_choiceVar")),
                               column(4,uiOutput("downl_Variance_ui")),
                               column(12,plotOutput("Var_graph")),
                               checkboxInput("show_Varplot_legend", "Show the figure legend"),
                               uiOutput("legend_Varplot_show"),
                               verbatimTextOutput("Bartlett"),
                               verbatimTextOutput("Levene")
                           ),
                      tabPanel("One / two sample tests", icon = icon("hand-peace-o"),
                               column(6,uiOutput("OT_test"),
                                        uiOutput("OT_what_mu"),
                                        uiOutput("OT_grouping_IVs")),
                               column(6,uiOutput("OT_which_compare"),
                                      uiOutput("OT_graph_download_ui")),
                               column(12,verbatimTextOutput("OT_test_results")),
                               column(12,plotlyOutput("OT_graph")), 
                               checkboxInput("show_OT_legend", "Show the figure legend"),
                               uiOutput("legend_OT_show")
                               ),
                      tabPanel("Testing significant differences", icon=icon("sun-o"),
                               checkboxInput("plot_subsANOVA", "Subset the data?"),
                               uiOutput("PlotsubsANOVA"),
                               uiOutput("Plotsubs_choiceANOVA"),
                               selectizeInput(inputId = "Sig_diff_test", 
                                              label = "Test significant differences with:",
                                              choices = c("ANOVA (parametric)", "Kruskal-Wallis (non-parametric)")),
                               
                               column(12,verbatimTextOutput("ANOVAtest")),
                               uiOutput("ANOVA_graph_download_ui"),
                               plotlyOutput("Boxes"),
                               checkboxInput("show_ANOVA1_legend", "Show the figure legend"),
                               uiOutput("legend_ANOVA1_show"),
                               verbatimTextOutput("Tukeylisting")),
                      tabPanel("two-way ANOVA", icon=icon("snowflake-o"),
                              column(4, uiOutput("TWANOVA_IV1")),
                              column(4, uiOutput("TWANOVA_IV2")),
                              column(4, checkboxInput("plot_sub_TWANOVA", label = "Subset the data?"),
                                        uiOutput("plot_sub_TWANOVA_trait"),
                                        uiOutput("plot_sub_TWANOVA_choiceVar")),
                              downloadButton("TW_ANOVA_graph_download", label="Download plot"),
                              plotOutput("TW_ANOVA_interaction_plot"),
                              checkboxInput("show_TWANOVA_legend", "Show the figure legend"),
                              uiOutput("legend_TWANOVA_show"),
                              verbatimTextOutput("two_ANOVA_report"),
                              downloadButton("TW_ANOVA_res_graph_download", label="Download plot"),
                              plotOutput("TW_ANOVA_QQ_plot"),
                              checkboxInput("show_res_TWANOVA_legend", "Show the figure legend"),
                              uiOutput("legend_res_TWANOVA_show"))
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
                            "This tab is currently under construction - main contributor is Mitchell Morton with satelite help from Magdalena Julkowska", br(),
                            "Please be patient and send us an e mail in case you have some analysis you would like to include in the App", br(), br(),
                            "Chers & glittes data-analyst!"),
                   tabPanel("Two-way ANOVA?",
                            "can we do it? Really???",
                            br(), br(),
                            "This tab is currently under construction - main contributor is Mitchell Morton with satelite help from Magdalena Julkowska", br(),
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
                        wellPanel(
                          uiOutput("cor_Pheno_data"),
                          # which data set to use (summarized / na / original) selectize, multiple = F
                          uiOutput("cor_phenos",lable = "Choose nummeric variables from your data for correlation analysis"),
                          # which data set to use (summarized / na / original) selectize, multiple = F
                          checkboxInput("cor_data_subset", label = "Subset the data?"),
                          uiOutput("cor_subset"),
                          uiOutput("CorSpecIV_val")),
                        wellPanel(
                          selectInput("corMethod", "Correlation Method:", choices = c("pearson","spearman")),
                          selectInput("corrplotMethod", "Plot Method:", choices = c("circle", "square", "ellipse", "number", "shade", "pie")),
                          selectInput("corType", "Plot Type:", choices = c("full", "lower", "upper")),
                          selectInput("corOrder", "Order the lables by:", choices = list("Original order" = "original","Angular order of eigenvectors" = "AOE", "First Principal Component order"  = "FPC", "Hierarchical clustering order"  = "hclust", "Alphabetical order" = "alphabet")),
                          numericInput("Cor_Big_steps", "Number of levels:", value = 10, min = 3, max = 10),
                          selectizeInput("Cor_color_palette", "Palette:", choices = list("Spectral" = "Spectral", "Red-Yellow-Green" = "RdYlGn", "Red-Yellow-Blue" = "RdYlBu", "Red-Grey" = "RdGy", "Red-Blue" = "RdBu", "Purple-Orange" = "PuOr", "Purple-Green" = "PRGn", "Pink-Green" = "PiYG", "Brown-Blue-Green" = "BrBG")),
                          checkboxInput("cor_sig_show", label = "Indicate non-significant correlations"),
                          uiOutput("cor_sig_level_output")
                        )),
                      mainPanel(
                        verbatimTextOutput("tricky_table"),
                        uiOutput("corrplot_button"),
                        plotOutput("corrplot"),
                        br(),
                        
                        checkboxInput("show_cor_legend", "Show figure legend"),
                        uiOutput("legend_cor_show"),
                        #     checkboxInput(inputId = "show_table",label = "Show correlation table",value = F),
                        #     verbatimTextOutput(outputId = "description"),        
                        br(),
                        uiOutput("cortable_button"),
                        br(),
                        verbatimTextOutput("cor_table_text"),
                        dataTableOutput("cor_table"))
                    ),
                    tabPanel(
                      "Scatterplots",
                      sidebarPanel(
                        uiOutput("Pheno1"), 
                        uiOutput("Pheno2"), 
                        uiOutput("colorby"),
                        checkboxInput("scatter_shape", "Add shape?"),
                        uiOutput("scatter_shapeby"),
                        checkboxInput("corr_scatter_sub", "Subset the data?"),
                        uiOutput("Corr_scatter_trait_select"),
                        uiOutput("Corr_scatter_trait_specify")),
                      mainPanel(
                        uiOutput("downl_scatter_corr_ui"),
                        plotlyOutput("scatterplot"),
                        column(12,checkboxInput("show_scatter_legend", "Show the figure legend"),
                               uiOutput("scatter_legend_show"))
                      )))
         # end of Tab#6
),

# Tab 7 = = = = = = = = = = = = = = >> PCA ANALYSIS << = = = = = = = = = = = = = = = = = = 

tabPanel("Dimensionality reduction", icon = icon("object-group"),
         navbarPage("",
                    tabPanel("Principle Component Analysis",
                             sidebarPanel(
                               fluidRow(
                                 uiOutput("PCA_Pheno_data"),
                                 uiOutput("PCA_Select_pheno"),
                                 checkboxInput("PCA_Scale_Q", "Scale the data?"),
                                 checkboxInput("PCA_data_subset", label = "Subset the data?"),
                                 uiOutput("PCA_subset_trait"),
                                 uiOutput("PCA_subset_specific"),
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
                                                   downloadButton("Eigen_plot_download", "Download plot"),
                                                   plotOutput("PCA_eigen_plot"),
                                                   br(),
                                                   column(12,checkboxInput("show_Eigen_legend", "Show figure legend"),
                                                          uiOutput("Eigen_legend_show")),
                                                   hr(),
                                                   uiOutput("Eigen_download_button"),
                                                   dataTableOutput("Eigen_data_table")),
                                          tabPanel("Contribution of variables",
                                                   downloadButton("PCA_contribution_plot_download", "Download plot"),
                                                   plotOutput("PCA_contribution_plot"),
                                                   br(),
                                                   column(12,checkboxInput("show_PCA_contribution_legend", "Show figure legend"),
                                                          uiOutput("PCA_contribution_legend_show")),
                                                   hr(),
                                                   column(4,uiOutput("PCA1_select")),
                                                   column(4,uiOutput("PCA2_select")),
                                                   column(4, uiOutput("PCA_colorby")),
                                                   downloadButton("PCAscatter_plot_download", "Download plot"),
                                                   plotlyOutput("PCA_scatterplot"),
                                                   column(12,checkboxInput("show_PCAscatter_legend", "Show figure legend"),
                                                          uiOutput("PCAscatter_legend_show")),
                                                   hr(),                        
                                                   uiOutput("Coord_download_ind"),
                                                   dataTableOutput("PCA_coordinates_ind")),
                                          tabPanel("Contribution per PC",
                                                   uiOutput("PCA_contrib_select"),
                                                   downloadButton("Contrib_trait_plot_download", "Download plot"),
                                                   plotOutput("Contrib_trait_plot"),
                                                   br(),
                                                   column(12,checkboxInput("show_PCAcontrib_legend", "Show figure legend"),
                                                          uiOutput("PCAcontrib_legend_show")),
                                                   hr(),         
                                                   uiOutput("Contrib_download_var"),
                                                   dataTableOutput("PCA_contribution_var"))
                               ))),
                    tabPanel("Multidimensional Scaling",
                             sidebarPanel(
                               
                               fluidRow(
                                 uiOutput("MDS_Pheno_data"),
                                 uiOutput("MDS_Select_pheno"), 
                                 checkboxInput("MDS_Scale_Q", "Scale the data?"),
                                 checkboxInput("MDS_subset_Q", label = "Subset the data?"),
                                 uiOutput("MDS_subset_trait"),
                                 uiOutput("MDS_subset_specific"),
                                 br(),
                                 checkboxInput("MDS_KMC_Q", label = "Cluster samples using k-means?"),
                                 uiOutput("MDS_KMC_number"),
                                 actionButton("Go_MDS", label = "Unleash the power of MDS"))),
                             
                             mainPanel(
                               navbarPage("MDS",
                                          tabPanel("Selected dataset",
                                                   dataTableOutput("MDS_raw_table")),
                                          tabPanel("Final dataset for MDS",
                                                   dataTableOutput("MDS_final_table")),
                                          tabPanel("MDS of the samples",
                                                   downloadButton("MDS_plot_download", "Download plot"),
                                                   plotlyOutput("MDS_sample_graph"),
                                                   br(),
                                                   column(12,checkboxInput("show_MDSsample_legend", "Show figure legend"),
                                                          uiOutput("MDSsample_legend_show")),
                                                   hr(),
                                                   uiOutput("MDS_download_samples"),
                                                   dataTableOutput("MDS_table_samples")),
                                          tabPanel("Scaling of traits",
                                                   downloadButton("MDS_plot_download_transposed", "Download plot"),
                                                   plotOutput("MDS_sample_graph_transposed"),
                                                   br(),
                                                   column(12,checkboxInput("show_MDSsampleT_legend", "Show figure legend"),
                                                          uiOutput("MDSsampleT_legend_show")),
                                                   hr()
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
                                 checkboxInput(inputId = "Hcluster_scale_Q", label = "Scale the data prior to clustering"),
                                 checkboxInput("Cluster_pre_calc", label = "Perform cluster analysis on mean values?"),
                                 checkboxInput("Cluster_subset_Q", label = "Subset the data?"),
                                 uiOutput("Cluster_subset_trait"),
                                 uiOutput("Cluster_subset_specific"),
                                 uiOutput("Select_clustering_method"),
                                 actionButton("Go_cluster", "Unleash cluster analysis"),
                                 hr(),
                                 numericInput("Split_cluster", "Separate clusters at:", value = 4)),
         mainPanel(
           navbarPage("Happy clustering",
                      tabPanel("Selected dataset",
                               dataTableOutput("data_HClust_table")),
                      tabPanel("Final dataset used for HClust",
                               dataTableOutput("Final_cluster_table")),
                      tabPanel("Clustering your HOT HOT Data",
                               #dataTableOutput("Data_cluster_table"),
                               
                               uiOutput("HotHeatMap_ui"),
                               plotOutput("HotHeatMap"),
                               br(),
                               column(12,checkboxInput("show_HHMCC_legend", "Show the figure legend"),
                                      uiOutput("HHCC_legend_show")),
                               hr(),
                               verbatimTextOutput("Dendro_sentence"),
                               uiOutput("ClusterTree_ui"),
                               plotOutput("ClusterTree"),
                               br(),
                               column(12,checkboxInput("show_Dendro_legend", "Show the figure legend"),
                                      uiOutput("Dendro_legend_show")),
                               br(),
                               uiOutput("Cluster_download_button"),
                               br(),
                               dataTableOutput("Cluster_table")),
                               
                      tabPanel("Cluster validation",
                               verbatimTextOutput("HotAnovaNews"),
                               column(4,uiOutput("HotANOVA_ui")),
                               column(4, uiOutput("Select_data_cluster_validation")),
                               plotOutput("HotANOVA"),
                               hr(),
                              column(12,checkboxInput("show_HCANOVA_legend", "Show the figure legend"),
                             uiOutput("HCANOVA_legend_show")))
                               
           ))),
      tabPanel("K-means Clustering", icon = icon("barcode"),
               sidebarPanel(
                 uiOutput("Select_data_K_mean_cluster"),
                 uiOutput("Select_DV_KMC"),
                 checkboxInput(inputId = "KMCluster_scale_Q", label = "Scale the data prior to clustering"),
                 checkboxInput("KMC_use_means", label = "Perform K-means cluster analysis on mean values?"),
                 checkboxInput("KMC_subset_Q", label = "Subset the data?"),
                 uiOutput("KMC_subset_trait"),
                 uiOutput("KMC_subset_specific"),
                 #actionButton(inputId= "Select_data_KMC", label = "Set the dataset"),
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
                            tabPanel("Selected dataset",
                                     dataTableOutput("KMC_data_table")
                            ),
                            tabPanel("Final data for K-means",
                                     dataTableOutput("KMCluster_test")
                            ),
                            
                            tabPanel("Optimal number of clusters",
                                     verbatimTextOutput("Yve_message"),
                                     uiOutput("downl_elbow_graph_KMC_ui"),
                                            plotOutput("elbow_graph_KMC"),
                                     hr(),
                                     checkboxInput("show_elbow_legend", "Show the figure legend"),
                                            uiOutput("elbow_legend_show"),
                                     uiOutput("downl_silhouette_graph_KMC_ui"),
                                            plotOutput("silhouette_graph_KMC"),
                                     hr(),
                                     column(12,checkboxInput("show_silhouette_legend", "Show the figure legend"),
                                            uiOutput("silhouette_legend_show")),
                                     #plotOutput("gapstat_graph_KMC"),
                                     # This worked with Yveline but isnt working now - dont know why - I ll hash it for now 
                                     #h5("Hubert index ***", align = "center"),
                                     #column(12,uiOutput("downl_indices_plots_KMC_1_ui"),
                                     #       plotOutput("indices_plots_KMC_1")),
                                     #h5("D index ***", align = "center"),
                                     # column(12, uiOutput("downl_indices_plots_KMC_2_ui"),
                                     #  plotOutput("indices_plots_KMC_2")),
                                     verbatimTextOutput("indices_majority_KMC"),
                                     #dataTableOutput("KMCluster_test"),
                                     column(12,  uiOutput("downl_indices_plots_KMC_3_ui"),
                                            plotOutput("indices_plots_KMC_3")),
                                     hr(),
                                     column(12,checkboxInput("show_optimalK_legend", "Show the figure legend"),
                                            uiOutput("optimalK_legend_show"))
                            ),
                            tabPanel("K means clustering plots",
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
                                     column(12,checkboxInput("show_barplotKMC_legend", "Show the figure legend"),
                                            uiOutput("barplotKMC_legend_show"))
                            ),
                            tabPanel("K-means clustering scatter plots",
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
                                     hr(),
                                     column(12,checkboxInput("show_scatterplotKMC_legend", "Show the figure legend"),
                                            uiOutput("scatterplotKMC_legend_show"))
                            ),
                            tabPanel("K-means clustering data table",
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
                        checkboxInput("herit_facet", "Split the data?"),
                        uiOutput("Heritfacets"),
                        checkboxInput("herit_split", "Subset the data?"),
                        uiOutput("Herit_splito"),
                        uiOutput("Herit_split_spec")
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
             checkboxInput("Quantile_sub_Q", "Subset the data?"),
             uiOutput("Quantile_sub_trait"),
             uiOutput("Quantile_sub_spec"),
             checkboxInput("Scale_QA", label = "Scale the data for quantile analysis?"),
             actionButton("Go_data", label = "Unleash the power of Quantile Regression")
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
                                 column(4,uiOutput("model_plot_type")),
                                 column(4,uiOutput("Select_model_variable"),
                                          uiOutput("QA_plot_slider_input"))
                               ),
                               br(),
                               
                              
                               
                               fluidRow(
                                column(4,downloadButton("downl_plot_QA", "Download plot")),
                                column(4,uiOutput("subset_plot"))),
                               plotOutput("QA_plot", height = 425),
                               column(12,checkboxInput("show_QA_legend", "Show the figure legend"),
                                      uiOutput("QA_legend_show"))
                               #          br(),
                               #           uiOutput("QA_plot_download")
                               
                      )
           )  
         ) # end of main panel
         
) # end of quantile tab

# end of App - final brackets
  )
)
