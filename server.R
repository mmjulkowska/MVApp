function(input, output) {
  #  - - - - - - - - - - >> GADGETS FOR 2nd TAB <<- - - - - - - - - - - - - - - - -
  
  ItemList = reactive(if (is.null(input$your_data)) {
    return()
  } else {
    d2 = read.csv(input$your_data$datapath)
    return(colnames(d2))
  })
  
  # - - - - - - - - - - - - - - - - - >> Reactive widgets << - - - - - - - - - - - -
  # Select SampleID
  output$CustomID <- renderUI({
    if ((is.null(ItemList())) | (input$IdCheck == FALSE)) {
      return ()
    } else
      tagList(
        selectizeInput(
          inputId = "SelectID",
          label = "Select here your sample ID",
          choices = ItemList(),
          multiple = F
        )
      )
  })
  
  # Select Independent Variable (genotype, treatment et al.)
  output$CustomGeno <- renderUI({
    if (is.null(ItemList())) {
      return ()
    } else
      tagList(
        selectizeInput(
          inputId = "SelectGeno",
          label = "Select here your Genotype column",
          choices = ItemList(),
          multiple = F
        )
      )
  })
  
  output$CustomIndepVar <- renderUI({
    if (is.null(ItemList())) {
      return ()
    } else
      tagList(
        selectizeInput(
          inputId = "SelectIV",
          label = "Select here your other independent variables (such as treatment, position et al.)",
          choices = ItemList(),
          multiple = T
        )
      )
  })
  
  
  # Select time column
  output$CustomTimepoint <- renderUI({
    if ((is.null(ItemList())) | (input$TimeCheck == F)) {
      return ()
    } else
      tagList(
        selectizeInput(
          inputId = "SelectTime",
          label = "Select here column with time",
          choices = ItemList(),
          multiple = F
        )
      )
  })
  
  # Select Dependent Variables (phenotypes)
  output$CustomPheno <- renderUI({
    if (is.null(ItemList())) {
      return ()
    } else
      tagList(
        selectizeInput(
          inputId = "SelectDV",
          label = "Select here your dependent variables",
          choices = ItemList(),
          multiple = T
        )
      )
  })
  
  # - - - - - - - - - - - - - - - - - >> Tables output in Tab2 << - - - - - - - - - - - -
  
  # Table in the Tab2 - main window - uploaded file overview
  output$Data_tabl <- renderDataTable({
    if (is.null(input$your_data)) {
      return(NULL)
    } else{
      read.csv(input$your_data$datapath)
    }
  })
  
  # Table in the Tab2 - main window - selected variables by the user
  my_data <- eventReactive(input$Go_Data, {
    d2 = read.csv(input$your_data$datapath)
    my_data <-
      subset(
        d2,
        select = c(
          input$SelectGeno,
          input$SelectIV,
          input$SelectID,
          input$SelectTime,
          input$SelectDV
        )
      )
    if(input$TimeCheck == T){
    my_data[, input$SelectTime] <- as.factor(my_data[,input$SelectTime])
    return(my_data)}
    else{
    return(my_data)  
    }
  })
  
  output$my_data <- renderDataTable({
    my_data()
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # - - - - - - - - - - - - - - >> MODELING IN 3rd TAB << - - - - - - - - - - - - -
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  output$Pheno_to_model <- renderUI({
    if ((is.null(input$SelectDV)) |
        (input$TimeCheck == FALSE)) {
      return ()
    } else
      tagList(
        selectizeInput(
          inputId = "ModelPheno",
          label = "Select a phenotype for which you would like estimate kinetics",
          choices = c(input$SelectDV),
          multiple = F
        )
      )
  })
  
  output$IV_to_model <- renderUI({
    if ((is.null(input$SelectIV)) |
        (input$TimeCheck == FALSE)) {
      return ()
    } else
      tagList(
        selectizeInput(
          inputId = "ModelIV",
          label = "Select an independent variable for which you would like estimate kinetics",
          choices = c(input$SelectIV, input$SelectGeno),
          multiple = F
        )
      )
  })
  
  output$IV_subset_model <- renderUI({
    if ((is.null(input$SelectIV)) |
        (input$TimeCheck == FALSE)) {
      return ()
    } else
      tagList(
        selectizeInput(
          inputId = "ModelSubIV",
          label = "Select an independent variable for which you would to subset",
          choices = c(input$SelectIV, input$SelectGeno),
          multiple = F
        )
      )
  })
  
  # Calculations for the model - temporary
  Model_temp_data <- eventReactive(input$Go_Model, {
    temp <-
      subset(
        my_data(),
        select = c(
          input$ModelIV,
          input$ModelSubIV,
          input$SelectID,
          input$SelectTime,
          input$ModelPheno
        )
      )
    temp[,input$SelectTime] <- as.numeric(temp[,input$SelectTime])
    sub_set <- c(input$ModelIV, input$ModelSubIV, input$SelectID)
    things_to_model <- unique(temp[sub_set])
    
    for (i in 1:nrow(things_to_model)) {
      super_temp <- subset(temp, temp[, 1] == things_to_model[i, 1])
      super_temp2 <-
        subset(super_temp, super_temp[, 2] == things_to_model[i, 2])
      super_temp3 <-
        subset(super_temp2, super_temp2[, 3] == things_to_model[i, 3])
      
    # modeling SelectTime to ModelPheno  
      if (input$model == "lin") {
        fit <- lm(super_temp3[, 4] ~ super_temp3[, 5])
        things_to_model[i, 4] <- coefficients(fit)[2]
        things_to_model[i, 5] <- coefficients(fit)[1]
        things_to_model[i, 6] <- summary(fit)$r.squared
      }
      
      if (input$model == "quad") {
        super_temp3$transformed <- sqrt(super_temp3[, 5])
        fit <- lm(super_temp3[, 4] ~ super_temp3$transformed)
        things_to_model[i, 4] <- (coefficients(fit)[2]) ^ 2
        things_to_model[i, 5] <- (coefficients(fit)[1]) ^ 2
        things_to_model[i, 6] <- summary(fit)$r.squared
      }
      
      if (input$model == "exp") {
        super_temp3$transformed <- log(super_temp3[, 5])
        fit <- lm(super_temp3[, 4] ~ super_temp3$transformed)
        things_to_model[i, 4] <- log(coefficients(fit)[2])
        things_to_model[i, 5] <- coefficients(fit)[1]
        things_to_model[i, 6] <- summary(fit)$r.squared
      }
      
      if (input$model == "sqr") {
        super_temp3$transformed <- (super_temp3[, 5]) ^ 2
        fit <- lm(super_temp3[, 4] ~ super_temp3$transformed)
        things_to_model[i, 4] <- sqrt(coefficients(fit)[2])
        things_to_model[i, 5] <- sqrt(coefficients(fit)[1])
        things_to_model[i, 6] <- summary(fit)$r.squared
      }
      
    }
    colnames(things_to_model)[4] <- "RGR"
    colnames(things_to_model)[5] <- "START"
    colnames(things_to_model)[6] <- "r_sqared"
    things_to_model
  })
  
  output$Model_data <- renderDataTable({
    Model_temp_data()
  })
  
  output$Select_modelPlot <- renderUI({
    if ((is.null(input$ModelIV)) |
        (input$TimeCheck == FALSE)) {
      return ()
    } else
      
      temp <- subset(my_data(), select = c(
        input$ModelIV,
        input$ModelSubIV,
        input$SelectID,
        input$SelectTime,
        input$ModelPheno))
      
      temp$selection <- paste(temp[,input$ModelIV], temp[,input$ModelSubIV], temp[,input$SelectID], sep="_")
      
      sample_list <- unique(temp$selection)
      
      tagList(
        selectizeInput(
          inputId = "Model_graph_fit_select",
          label = "Select a specific sample to display in Fit-Plot",
          choices = sample_list,
          multiple = F
        )
      )
  })
  
  data_model_plot <- eventReactive(input$Go_modelPlot,{
    temp <- subset(my_data(), select = c(
      input$ModelIV,
      input$ModelSubIV,
      input$SelectID,
      input$SelectTime,
      input$ModelPheno))
    temp[,input$SelectTime] <- as.numeric(temp[,input$SelectTime])
    sub_set <- c(input$ModelIV, input$ModelSubIV, input$SelectID)
    things_to_model <- unique(temp[sub_set])
    
    # This isnt working!!! Why isnt it subsetting!
    temp$selection <- paste(temp[,input$ModelIV], temp[,input$ModelSubIV], temp[,input$SelectID], sep="_")
    docelowy <- subset(temp, temp$selection == input$Model_graph_fit_select)
    docelowy
  })
  
  output$Model_plot <- renderPlot({
     
     docelowy <- data_model_plot()

      if(input$model == "lin"){
        plot(docelowy[,input$ModelPheno] ~ docelowy[,input$SelectTime], main = input$Model_graph_fit_select)
        abline(lm(docelowy[,input$ModelPheno] ~ docelowy[,input$SelectTime]))
      }
      
      if (input$model == "quad") {
        docelowy$helper <- sqrt(docelowy[, input$ModelPheno])
        plot(docelowy$helper ~ docelowy[,input$SelectTime], main = input$Model_graph_fit_select)
        abline(lm(docelowy$helper ~ docelowy[,input$SelectTime]))
      }
      
      if (input$model == "exp") {
        docelowy$helper <- log(docelowy[, input$ModelPheno])
        plot(docelowy$helper ~ docelowy[,input$SelectTime], main = input$Model_graph_fit_select)
        abline(lm(docelowy$helper ~ docelowy[,input$SelectTime]))
      }
      
      if (input$model == "sqr") {
        docelowy$helper <- (docelowy[, input$ModelPheno])^2
        plot(docelowy$helper ~ docelowy[,input$SelectTime], main = input$Model_graph_fit_select)
        abline(lm(docelowy$helper ~ docelowy[,input$SelectTime]))
      }
    })
  
  # Adding data to the existing table
  
  
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  #  - - - - - - - - - - >> SUMMARY STATISTICS IN 4th TAB <<- - - - - - - - - - - -
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # Table in Tab4 - main window - summary of the data based on the selected calculations
  
  
  ## TESTING OMIT.NA     %% Mitch %%
  my_data_nona <- eventReactive(input$Go_omitna, {
    my_data_nona <- my_data()[complete.cases(my_data()),] #use na.omit instead maybe?
    return(my_data_nona)
  })
  
  na_row<-eventReactive(input$Go_omitna, {
    sum_na<-nrow(my_data())-nrow(my_data_nona())
    return(paste("Number of rows containing NAs that were removed: ", sum_na, ".", sep=""))
  })
  
  output$total_na <- renderText({na_row()})
  
  
  ## Added new input "$SelectSumm"  and output "$CustomSumm"  %% Mitch %%
  output$CustomSumm <- renderUI({
    if((is.null(ItemList()))){return ()
    } else tagList(
      selectizeInput(inputId = "SelectSumm", 
                     label = "Select desired summary statistics", 
                     choices=c("Mean", "Median", "StdDev", "StdErr", "Min", "Max"), multiple=T))
  })
  
  
  ## Added list of summary functions "summfuns"   %% Mitch %%
  summfuns<-list(Mean = function(x) mean(x),
                 Median = function(x) median(x),
                 StdDev = function(x) sd(x),
                 StdErr = function(x) std.error(x),
                 Min = function(x) min(x),
                 Max = function(x) max(x))
  
  sum_data <- eventReactive(input$Go_SummaryStat, {
    melted_icecream <- melt(my_data_nona(), id=c(input$SelectGeno, input$SelectIV, input$SelectID, input$SelectTime))
  #change day/time into factor becuase it is now numeric and does not show in melt.
    
    ## Added call to selected summary stats functions "FUN=summfuns[input$SelectSumm]"     %% Mitch %%
    sum_my_data<-summaryBy(value ~., data=melted_icecream, FUN=summfuns[input$SelectSumm])
    
    ## Label columns based on chosen summary stats     %% Mitch %%
    colnames(sum_my_data)<-c(input$SelectGeno, input$SelectIV, input$SelectID, input$SelectTime, "Dependent_Variable", input$SelectSumm)
    return(sum_my_data)
    
  })
  
  output$sum_data <- renderTable({
    sum_data()
  })
  
  
  output$HisIV <- renderUI({
   if ((input$Go_Data == FALSE)) {
    return ()
    } else
      tagList(
        selectizeInput(
          inputId = "HisIV",
          label = "Select the variable for which you would like to subset your data.",
          choices = c(
            input$SelectIV,
            input$SelectGeno,
            input$SelectTime,
            input$SelectID
          ),
          multiple = F
        )
      )
  })
  
  
  output$HisDV <- renderUI({
    if ((input$Go_Data == FALSE)) {
      return ()
    } else
      tagList(
        selectizeInput(
          inputId = "HisDV",
          label = "Select the trait for which you would like to plot the graphs.",
          choices = c(
            input$SelectDV
          ),
          multiple = F
        )
      )
  })
  
  
  my_hisdata<-eventReactive(input$HisIV, {
    hisdata<-my_data()[,c(input$HisDV,input$HisIV)]
  })
  
  
  
  output$HistType <- renderUI({
    if ((input$Go_Data == FALSE)) {
      return ()
    } else
      tagList(
        selectizeInput(
          inputId = "HistType",
          label = "Select the plot type.",
          choices = c("HistCount", "HistDensity"),
          selected = "HistCount",
          multiple = F
        )
      )
  })
  
  
  
  output$HistPlot <- renderPlotly({
    
    my_his_data <- my_hisdata()
    if (input$HistType == "HistCount") {
       fit <- ggplot(my_his_data, aes(x=my_his_data[,1], fill=my_his_data[,2])) + xlab(names(my_his_data[1])) + geom_histogram(size=0.6, alpha=0.3, col="black") 
    }
    if (input$HistType == "HistDensity" ) { 
      fit <- ggplot(my_his_data, aes(x=my_his_data[,1], fill=my_his_data[,2])) + xlab(names(my_his_data[1]))  + geom_density()
    }
    ggplotly(fit)
     }) 
  

     
     ##WORKEDDD but has to be 1 dependent variable and 1 independent only!!. Also problem with group"day" because there are too many...
    

    my_hisdata2<-eventReactive(input$Go_Boxplot, {
      hisdata2<-my_data()[,c(input$HisDV,input$HisIV)]
    })
    
    ##try to do subset by multiple variables
    output$Boxes <- renderPlotly({
      
      box_graph <- ggplot(my_hisdata2(), aes(x=my_hisdata2()[,2], y=my_hisdata2()[,1])) + xlab(names(my_hisdata2()[2])) + ylab(names(my_hisdata2()[1])) + geom_boxplot()
      
      ggplotly(box_graph)
      
      
    })
    
    
    Outlier_data <- eventReactive(input$Go_Outliers, {
      hisdata3<-my_data()[,c(input$SelectID, input$HisDV,input$HisIV)]
     ag1<-aggregate(hisdata3[,2], by=list(hisdata3[,3]), FUN=mean)
      ag2<- aggregate(hisdata3[,2], by=list(hisdata3[,3]), FUN=sd)
    
      #I am doing the 1st level outside the loop, then bind the output of other levels (>=2) to this  
      
      doublesd<-2*(ag2[1,2]) #2*sd
      lower<-ag1[1,2] - doublesd
      upper<- ag1[1,2] + doublesd
      outs1<-subset(hisdata3, hisdata3[,3] == levels(hisdata3[,3])[1] & (hisdata3[,2] < lower | hisdata3[,2] > upper))
      
      
      for (i in 2:length(levels(hisdata3[,3]))){
        doublesd<-2*(ag2[i,2]) #2*sd
        lower<-ag1[i,2] - doublesd
        upper<- ag1[i,2] + doublesd
       outs<-subset(hisdata3, hisdata3[,3] == levels(hisdata3[,3])[i] & (hisdata3[,2] < lower | hisdata3[,2] > upper))
       outs<-rbind(outs1, outs)
      } 
     outs<-as.data.frame(outs)
    })
    
    output$Outlier_data <- renderDataTable({Outlier_data()}) 
    
    
  
  ### Tab 6: correlation tab
  
  output$Pheno1 <- renderUI({
    if (is.null(input$SelectDV)) {
      return ()
    } else
      tagList(
        selectizeInput(
          inputId = "Pheno1",
          label = "Select here your dependent variable 1 to be plotted",
          choices = input$SelectDV,
          multiple = F
        )
      )
  })
  
  output$Pheno2 <- renderUI({
    if (is.null(input$SelectDV)) {
      return ()
    } else
      tagList(
        selectizeInput(
          inputId = "Pheno2",
          label = "Select here your dependent variable 2 to be plotted",
          choices = input$SelectDV,
          multiple = F
        )
      )
  })
  
  output$colorby <- renderUI({
    if ((is.null(input$SelectIV)) |
        (input$SelectGeno == FALSE)) {
      return ()
    } else
      tagList(
        selectizeInput(
          inputId = "Color",
          label = "Select here the color variable to be shown on the plot",
          choices = c(input$SelectIV, input$SelectGeno),
          multiple = F
        )
      )
  })
  
  ############ plot to fix ##########

  output$scatterplot <- renderPlotly({
    my_data <- data.frame(my_data())
    my_data %>% ggplot(aes_string(input$Pheno1, input$Pheno2)) + geom_point(aes_string(colour =input$Color))
    ggplotly()
  })
  
  ##################################
  
  output$corrplot <- renderPlot({
    beginCol <-
      length(c(
        input$SelectIV,
        input$SelectGeno,
        input$SelectTime,
        input$SelectID
      )) + 1
    endCol <-
      length(c(
        input$SelectIV,
        input$SelectGeno,
        input$SelectTime,
        input$SelectID
      )) + length(input$SelectDV)
    
    corrplot.mixed(
      cor(my_data()[, beginCol:endCol]),
      order = "hclust",
      tl.col  = "black"
    )
  })
  
  output$CorSpecIV <- renderUI({
    if ((input$Go_Data == FALSE)) {
      return ()
    } else
      tagList(
        selectizeInput(
          inputId = "CorIV_sub",
          label = "Which IV do you want to subset the data?",
          choices = c(
            input$SelectIV,
            input$SelectGeno,
            input$SelectTime,
            input$SelectID
          ),
          multiple = F
        )
      )
  })
  
  output$CorSpecIV_val <- renderUI({
    if ((input$Go_Data == FALSE)) {
      return()
    } else
      names <-
        subset(my_data(), select = input$CorIV_sub) %>% unique()
    tagList(
      selectizeInput(
        inputId = "CorIV_val",
        label = "which values of IV would you like to examine for correlation?",
        choices = c(names),
        multiple = F
      )
    )
  })
  
  output$corrplot2 <- renderPlot({
    beginCol <-
      length(c(
        input$SelectIV,
        input$SelectGeno,
        input$SelectTime,
        input$SelectID
      )) + 1
    endCol <-
      length(c(
        input$SelectIV,
        input$SelectGeno,
        input$SelectTime,
        input$SelectID
      )) + length(input$SelectDV)
    my_data <- data.frame(my_data())

    names(my_data) <- sub(input$CorIV_sub, "Cor_baby", names(my_data))
    my_data2 <- subset(my_data, Cor_baby == input$CorIV_val)
    my_data2 <- na.omit(my_data2)
    corrplot.mixed(
      cor(my_data2[, beginCol:endCol]),tl.col  = "black"
    )
  })

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # - - - - - - - - - - - - - - >> PCA IN 7th TAB <<- - - - - - - - - - - - - - - -
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  output$PCA_Pheno_data <- renderUI({
    if (is.null(my_data())) {
    return()
      }
    else  
      tagList(
        selectizeInput(
          inputId = "PCA_data",
          label = "Select the dataset that you would like to use for PCA",
          choices = c("original data", "data with NA removed", "summarized data"), multiple = F))
  })  
  
# we need to put all possible datasets in the same format - let's melt it all!
  
  my_data_melt <- eventReactive(input$Go_PCAdata,{
    my_melt <- melt(my_data(), id=c(input$SelectGeno, input$SelectIV, input$SelectID, input$SelectTime))
    my_melt
    })
  
 my_data_nona_melt <- eventReactive(input$Go_PCAdata,{
    my_melt <- melt(my_data_nona(), id=c(input$SelectGeno, input$SelectIV, input$SelectID, input$SelectTime))
    my_melt
  })
 
 sum_data_melt <- eventReactive(input$Go_PCAdata,{
   my_melt <- melt(sum_data(), id=c(input$SelectGeno, input$SelectIV, input$SelectID, input$SelectTime, "Dependent_Variable"))
   names(my_melt)[names(my_melt) == "variable"] <- "summary_stat"
   my_melt$variable <- paste(my_melt$Dependent_Variable,"_",my_melt$summary_stat, separate="")
   my_melt
   
 })
 
 PCA_data_type <- eventReactive(input$Go_PCAdata,{
   if(input$PCA_data == "original data"){
     PCA_data_type <- my_data_melt()
   }
   if(input$PCA_data == "data with NA removed"){
     PCA_data_type <- my_data_nona_melt()
   }
   if(input$PCA_data == "summarized data"){
     PCA_data_type <- sum_data_melt()
   }
   PCA_data_type
 })
 
 output$PCA_raw_table <- renderDataTable({
   PCA_data_type()
 })

 output$PCA_Select_pheno <- renderUI({
   if ((input$Go_PCAdata == FALSE)) {
     return()
   } else
     names <- subset(PCA_data_type(), select = variable) %>% unique()
   tagList(
     selectizeInput(
       inputId = "PCA_pheno",
       label = "Select the phenotypes would you like to use for the PCA",
       choices = c(names),
       multiple = T
     )
   )
 })
 
 PCA_final_data <- eventReactive(input$Go_PCA,{
   temp <- data.frame(PCA_data_type())
   temp <- subset(temp, temp$variable == input$PCA_pheno)
   temp <- temp[,c(input$SelectGeno, input$SelectIV, input$SelectID, input$SelectTime, "variable", "value")]
   temp2 <- dcast(temp, formula = ... ~ variable, value.var = "value")
   temp2
   })

 output$PCA_final_table <- renderDataTable({
   PCA_final_data()
 })
   
 PCA_eigen_data <- eventReactive(input$Go_PCA,{
   beginCol <-
     length(c(
       input$SelectIV,
       input$SelectGeno,
       input$SelectTime,
       input$SelectID
     )) + 1
   endCol <-ncol(PCA_final_data())
   PCA_ready <- PCA_final_data()
   PCA_ready <- PCA_ready[, beginCol : endCol]
   res.pca <- PCA(PCA_ready, graph = FALSE)
   eigenvalues <- res.pca$eig
   eigenvalues
 })
 
 output$PCA_eigen_plot <- renderPlot({
 eigenvalues <- PCA_eigen_data()
 barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues), 
         main = "Variances",
         xlab = "Principal Components",
         ylab = "Percentage of variances",
         col ="steelblue")
 })
 
 output$PCA_contribution_plot <- renderPlot({
   beginCol <-
     length(c(
       input$SelectIV,
       input$SelectGeno,
       input$SelectTime,
       input$SelectID
     )) + 1
   endCol <-ncol(PCA_final_data())
   PCA_ready <- PCA_final_data()
   PCA_ready <- PCA_ready[, beginCol : endCol]
   res.pca <- PCA(PCA_ready, graph = FALSE)
  # if possible - add the contribution labels as plotly labels - to show only when you scroll over the arrow with the mouse 
   # ALSO - make user interactive - which PC to plot
   fviz_pca_var(res.pca, axes = c(1,2), col.var="contrib", labelsize = 4, repel=T, addEllipses=F)+
     scale_color_gradient2(low="grey", mid="purple", 
                           high="red")+theme_bw()
   
 })
 
 output$PCA_scatter_plot <- renderPlot({
   beginCol <-
     length(c(
       input$SelectIV,
       input$SelectGeno,
       input$SelectTime,
       input$SelectID
     )) + 1
   endCol <-ncol(PCA_final_data())
   PCA_ready <- PCA_final_data()
   PCA_ready <- PCA_ready[, beginCol : endCol]
   res.pca <- PCA(PCA_ready, graph = FALSE)
   fviz_pca_ind(res.pca, col.ind="cos2") +
     scale_color_gradient2(low="grey", mid="purple", 
                           high="red", midpoint=0.50)+
     theme_minimal()
 })
 
  # end of the script
}
