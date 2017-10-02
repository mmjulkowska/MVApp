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
          label = "Select your sample ID",
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
          label = "Select your Genotype column",
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
          label = "Select your independent variables (e.g. treatment, position)",
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
          label = "Select your Time column",
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
          label = "Select your dependent variables (e.g. traits)",
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
          label = "Select a phenotype for which you would like estimate the kinetics",
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
          label = "Select an independent variable for which you would like estimate the kinetics (IndepVar)",
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
  
 # making advice on which model type to chose based on the r-square values
  Model_est_data <- eventReactive(input$Go_HelpModel, {
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

      # calculating r-squared for the linear model      
        fit_lin <- lm(super_temp3[, 4] ~ super_temp3[, 5])
        things_to_model[i, 4] <- summary(fit_lin)$r.squared
      # calculating r-squared for the quadratic model
        super_temp3$sqrt_transformed <- sqrt(super_temp3[, 5])
        fit_sqrt <- lm(super_temp3[, 4] ~ super_temp3$sqrt_transformed)
        things_to_model[i, 5] <- summary(fit_sqrt)$r.squared
      # calculating r-squared for the exponential model 
        super_temp3$log_transformed <- log(super_temp3[, 5])
        fit_log <- lm(super_temp3[, 4] ~ super_temp3$log_transformed)
        things_to_model[i, 6] <- summary(fit_log)$r.squared
      # calculating r-squared for the square root model
        super_temp3$quad_transformed <- (super_temp3[, 5]) ^ 2
        quad_fit <- lm(super_temp3[, 4] ~ super_temp3$quad_transformed)
        things_to_model[i, 7] <- summary(quad_fit)$r.squared
      }
      
    colnames(things_to_model)[4] <- "Linear_model"
    colnames(things_to_model)[5] <- "Quadratic_model"
    colnames(things_to_model)[6] <- "Exponential_model"
    colnames(things_to_model)[7] <- "Square_root_model"
    colnames(things_to_model)[1] <- "IndepVar"
    model_sum <- summaryBy(Linear_model + Quadratic_model + Exponential_model + Square_root_model ~ IndepVar, data = things_to_model)
    model_sum
  })
  
  output$Model_estimation <- renderDataTable({
    Model_est_data()
  })
  
  
  # Calculations for the model 
  # provides table with RGR / START and r-square
  # PROBLEM IS this is temporary datafile that can be overwritten with the next modeling attempt
  
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
  
  # Interactive user input for Fit-Plot - to select specific sample for fitness examination
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
  
  # object saving the user-interactive plot for fitness (Fit-Plot)
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
    
    temp$selection <- paste(temp[,input$ModelIV], temp[,input$ModelSubIV], temp[,input$SelectID], sep="_")
    docelowy <- subset(temp, temp$selection == input$Model_graph_fit_select)
    docelowy
  })
  
  # Fit-Plot
  output$Model_plot <- renderPlot({
     
     docelowy <- data_model_plot()

      if(input$model == "lin"){
        pheno <- docelowy[,input$ModelPheno]
        time <- docelowy[,input$SelectTime]
        title <- unique(docelowy$selection)
        plot(pheno ~ time, main = title, ylab = input$ModelPheno, xlab = input$SelectTime)
        abline(lm(pheno ~ time), col="red")
        }
      
      if (input$model == "quad") {
        docelowy$helper <- sqrt(docelowy[, input$ModelPheno])
        pheno <- docelowy$helper
        time <- docelowy[,input$SelectTime]
        title <- unique(docelowy$selection)
        plot(pheno ~ time, main = title, ylab = input$ModelPheno, xlab = input$SelectTime)
        abline(lm(pheno ~ time), col="red")
      }
      
      if (input$model == "exp") {
        docelowy$helper <- log(docelowy[, input$ModelPheno])
        pheno <- docelowy$helper
        time <- docelowy[,input$SelectTime]
        title <- unique(docelowy$selection)
        plot(pheno ~ time, main = title, ylab = input$ModelPheno, xlab = input$SelectTime)
        abline(lm(pheno ~ time), col="red")
      }
      
      if (input$model == "sqr") {
        docelowy$helper <- (docelowy[, input$ModelPheno])^2
        pheno <- docelowy$helper
        time <- docelowy[,input$SelectTime]
        title <- unique(docelowy$selection)
        plot(pheno ~ time, main = title, ylab = input$ModelPheno, xlab = input$SelectTime)
        abline(lm(pheno ~ time), col="red")
      }
    })
  
  # Adding data to the existing table
  # this is not working, as I am adding the data to something that doesnt exist. 
  Saved_model_data <- eventReactive(input$Go_SaveModelData,{
      saved <- Model_temp_data()
      saved <- subset(saved, select=c(1:4))
      saved[,input$SelectTime] <- "RGR"
      melted_model <- melt(saved, id=c(input$SelectGeno, input$SelectIV, input$SelectID, input$SelectTime))
      melted_model$variable <- input$ModelPheno
      melted_model
      })
  
  output$Complete_model_data <- renderDataTable({
    Saved_model_data()
  })
  
  # Fusing the model data to the data that can be used for Summary Stats
  Data_for_summ <- eventReactive(input$Go_SaveModelData,{
    melted_icecream <- melt(my_data_nona(), id=c(input$SelectGeno, input$SelectIV, input$SelectID, input$SelectTime))
    model <- Saved_model_data()
    brain_fusion <- rbind(melted_icecream, model)
    brain_fusion
  })
  
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
    if(input$Go_SaveModelData){
      melted_icecream <- Data_for_summ()
    }
    else
    melted_icecream <- melt(my_data_nona(), id=c(input$SelectGeno, input$SelectIV, input$SelectID, input$SelectTime))
    
    ## Added call to selected summary stats functions "FUN=summfuns[input$SelectSumm]"     %% Mitch %%
    sum_my_data<-summaryBy(value ~., data=melted_icecream, FUN=summfuns[input$SelectSumm])
    
    ## Label columns based on chosen summary stats     %% Mitch %%
    colnames(sum_my_data)<-c(input$SelectGeno, input$SelectIV, input$SelectID, input$SelectTime, "Dependent_Variable", input$SelectSumm)
    return(sum_my_data)
    
  })
  
  output$sum_data <- renderDataTable({
    sum_data()
  })
  
  output$Sum_download_button <- renderUI({
    if(is.null(sum_data())){
      return()}
    else
  downloadButton("data_sum", label="Download Summary Stats data")
    })
  
  output$data_sum <- downloadHandler(
    filename = "Summary_stats_MVApp.csv",
   content <- function(file) {
        write.csv(sum_data(), file)}
  )
  
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
          label = "Select the trait you would like to plot",
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
          label = "Select a plot type",
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
          label = "Select the first dependent variable to be plotted",
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
          label = "Select the second dependent variable to be plotted",
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
          label = "Select the color variable to be shown on the plot",
          choices = c(input$SelectIV, input$SelectGeno),
          multiple = F
        )
      )
  })
  
  ############ interactive scatter plot ##########

  output$scatterplot <- renderPlotly({
    my_data <- data.frame(my_data())
    my_data %>% ggplot(aes_string(input$Pheno1, input$Pheno2)) + geom_point(aes_string(colour =input$Color))
    ggplotly()
  })
  
  ########## showing r square and p value ###########
  output$corrsq <- renderText({
    cor_data <- my_data()[,c(input$Pheno1,input$Pheno2)]
    correl <- lm(cor_data[,1] ~ cor_data[,2])
    r2 <- summary(correl)$r.squared
    paste("The R square value of the linear regression is", signif(r2, 3))
  })
  
  output$corpval <- renderText({
    cor_data <- my_data()[,c(input$Pheno1,input$Pheno2)]
    correl <- lm(cor_data[,1] ~ cor_data[,2])
    pval <- summary(correl)$coefficients[8]
    paste("The p-value is", signif(pval, 3))
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
          label = "By which independent variable do you want to subset the data?",
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
        label = "Which values of the independent variable would you like to examine for correlation?",
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
 
# output$Select_PC_number <- renderUI({
#   if ((input$Go_PCAdata == FALSE)) {
 #    return()
#   } else
 #    names <- subset(PCA_data_type(), select = variable) %>% unique()
#   tagList(
  #   selectizeInput(
#       inputId = "PC_number",
#       label = "Select the number of PCs you would like to use for the PCA",
 #      choices = c(names),
 #      multiple = F
#     )
 #  )
# })
 
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
 
 #output$PCs<-renderUI({
 #  if (is.null(input$files)) { return(NULL) }
#   maxPCs<-ncol(input$files)
#   numericInput("PCs", "Number of Principal Components", 
#                2, min = 2, max = maxPCs)
# })
# https://gist.github.com/dgrapov/5846650
# https://github.com/kylehamilton/PACA/blob/master/server.R
 
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
 #ggplot(data=eigenvalues[, 2], aes(x="Principal Components", y="Percentage of variances")) + geom_bar(stat="identity")
 barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues), 
         main = "Variances",
         xlab = "Principal Components",
         ylab = "Percentage of variances",
         col ="steelblue")
# ggplotly()
 })
 
 #function(input, output) {
 #  output$value <- renderPrint({ input$radio })
# }
 
 output$Select_which_PC <- renderUI({
   if ((input$Go_PCAdata == FALSE)) {
     return()
   } else
     names <- subset(PCA_data_type(), select = variable) %>% unique()
   tagList(
     selectizeInput(
       inputId = "Which_PC",
       label = "Select which PCs you would like to plot",
       choices = c(names),
       multiple = F
     )
   )
 })
 
 output$PCA_contribution_plot <- renderPlotly({
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
   PCs <- data.frame(res.pca$ind$contrib)
   autoplot(prcomp(PCs))
   #https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html
 })
 
# output$PCA_contribution_plot <- renderPlot({
  # beginCol <-
 #    length(c(
   #    input$SelectIV,
    #   input$SelectGeno,
     #  input$SelectTime,
      # input$SelectID
  #   )) + 1
  # endCol <-ncol(PCA_final_data())
  # PCA_ready <- PCA_final_data()
  # PCA_ready <- PCA_ready[, beginCol : endCol]
  # res.pca <- PCA(PCA_ready, graph = FALSE)
#  mid=median(res.pca$var$contrib)
 # fviz_pca_var(res.pca, axes = c(1,2), col.var="contrib", labelsize = 4, repel=T, addEllipses=F)+
  # scale_color_gradient2(low="grey", mid="purple", 
   #                        high="red", midpoint=mid)+theme_bw()
# })
 
 #PCA_graph <- ggplot(res.pca, aes(x=res.pca$var$contrib[,2], y=res.pca$var$contrib[,1])) + xlab(names(res.pca$var$contrib[,2])) + ylab(names(res.pca$var$contrib[,2])) + geom_boxplot()
 #ggplotly(PCA_graph)
 
#data <- data.frame(obsnames=row.names(PC$x), PC$x)
#PCA_graph <- ggplot(res.pca$var$contrib, aes_string(x=x, y=y)) + geom_text(alpha=.4, size=3, aes(label=obsnames))
#PCA_graph <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)
 #ggplotly(PCA_graph)
 
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
   mid1=median(res.pca$var$cos2)
   fviz_pca_ind(res.pca, axes = c(1,2), col.ind="cos2") +
     scale_color_gradient2(low="grey", mid="purple", 
                           high="red", midpoint=mid1)+
     theme_minimal()
 })
 
  # end of the script
}
