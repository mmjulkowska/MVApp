function(input, output) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # - - - - - - - - - - - - - - >> DATA UPLOAD IN 2nd TAB<< - - - - - - - - - - - -
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  ItemList = reactive(if (is.null(input$your_data)) {
    return()
  } else {
    d2 = read.csv(input$your_data$datapath)
    return(colnames(d2))
  })
  
  # - - - - - - - - - - - - - - - - - >> Reactive widgets << - - - - - - - - - - - -
  
  # Select Genotype
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
          label = "Select your independent variables (e.g. treatment, position) that are going to be used for grouping your phenotypes",
          choices = ItemList(),
          multiple = T
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
          label = "Select your dependent variables (phenotypes) that you would like to analyze",
          choices = ItemList(),
          multiple = T
        )
      )
  })
  
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
  
  
  # - - - - - - - - - - - - - - - - - >> Tables output in Tab2 << - - - - - - - - - - - -
  
  # Table in the Tab2 - main window - uploaded file overview
  output$Data_tabl <- renderDataTable({
    if (is.null(input$your_data)) {
      return(NULL)
    } else{
      read.csv(input$your_data$datapath)
    }
  })
  
  output$uploaded_data_report <- renderText({
    if (is.null(input$your_data)) {
      return(NULL)
    }
    else{
      sraka <- read.csv(input$your_data$datapath)
      sraka_nona <- sraka[complete.cases(sraka), ]
      na_numbers <- nrow(sraka) - nrow(sraka_nona)
      sentence <-
        paste(
          "Your uploaded data contains ",
          dim(sraka)[1],
          " rows and ",
          dim(sraka)[2],
          " columns. There are ",
          na_numbers,
          " rows containins missing values."
        )
      return(sentence)
    }
  })
  
  output$selected_data_report <- renderText({
    if (is.null(my_data())) {
      return(NULL)
    }
    else{
      sraka <- my_data()
      sraka_nona <- sraka[complete.cases(sraka), ]
      na_numbers <- nrow(sraka) - nrow(sraka_nona)
      sentence <-
        paste(
          "Your selected data contains ",
          dim(sraka)[1],
          " rows and ",
          dim(sraka)[2],
          " columns. There are ",
          na_numbers,
          " rows containins missing values."
        )
      return(sentence)
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
    if (input$TimeCheck == T) {
      my_data[, input$SelectTime] <- as.factor(my_data[, input$SelectTime])
      return(my_data)
    }
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
    temp[, input$SelectTime] <-
      as.numeric(as.character(temp[, input$SelectTime]))
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
      fit_sqrt <-
        lm(super_temp3[, 4] ~ super_temp3$sqrt_transformed)
      things_to_model[i, 5] <- summary(fit_sqrt)$r.squared
      # calculating r-squared for the exponential model
      super_temp3$log_transformed <- log(super_temp3[, 5])
      fit_log <- lm(super_temp3[, 4] ~ super_temp3$log_transformed)
      things_to_model[i, 6] <- summary(fit_log)$r.squared
      # calculating r-squared for the square root model
      super_temp3$quad_transformed <- (super_temp3[, 5]) ^ 2
      quad_fit <-
        lm(super_temp3[, 4] ~ super_temp3$quad_transformed)
      things_to_model[i, 7] <- summary(quad_fit)$r.squared
    }
    
    colnames(things_to_model)[4] <- "Linear_model"
    colnames(things_to_model)[5] <- "Quadratic_model"
    colnames(things_to_model)[6] <- "Exponential_model"
    colnames(things_to_model)[7] <- "Square_root_model"
    colnames(things_to_model)[1] <- "IndepVar"
    model_sum <-
      summaryBy(
        Linear_model + Quadratic_model + Exponential_model + Square_root_model ~ IndepVar,
        data = things_to_model
      )
    model_sum
  })
  
  output$Model_estimation <- renderDataTable({
    Model_est_data()
  })
  
  output$best_model_advice <- renderText({
    fraka <- Model_est_data()
    fraka1 <- colnames(fraka)[apply(fraka, 1, which.max)]
    sentence <-
      paste("The model with the highest estimation is ", fraka1[1])
    return(sentence)
  })
  
  output$model_warning <- renderText({
    if (is.null(Model_temp_data())) {
      return()
    }
    else
      
      frajka <- Model_temp_data()
    frajka_boom <- subset(frajka, frajka$r_squared < 0.7)
    how_much <- ncol(frajka_boom)
    
    sentence <-
      paste(
        "There are ",
        how_much,
        " samples with r-square value below 0.7. You should consider checking them."
      )
    return(sentence)
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
    temp[, input$SelectTime] <-
      as.numeric(as.character(temp[, input$SelectTime]))
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
    colnames(things_to_model)[4] <- "DELTA"
    colnames(things_to_model)[5] <- "INTERCEPT"
    colnames(things_to_model)[6] <- "r_squared"
    things_to_model
  })
  
  output$Model_data <- renderDataTable({
    Model_temp_data()
  })
  
  # Let's allow users to chose beteween one and multiple plot display
  
  output$Select_model_plot_type <- renderUI({
    if (is.null(Model_temp_data()))
      return()
    else
      tagList(
        selectizeInput(
          inputId = "Select_model_type_plot",
          label = "Select how you would like to view the fit-plots",
          choices = c("single plot", "multiple plots")
        )
      )
  })
  
  
  # Interactive user input for Fit-Plot - to select specific sample for fitness examination
  output$Select_modelPlot <- renderUI({
    if (input$Select_model_type_plot == "multiple plots") {
      return ()
    } else
      
      temp <- subset(
        my_data(),
        select = c(
          input$ModelIV,
          input$ModelSubIV,
          input$SelectID,
          input$SelectTime,
          input$ModelPheno
        )
      )
    
    temp$selection <-
      paste(temp[, input$ModelIV], temp[, input$ModelSubIV], temp[, input$SelectID], sep =
              "_")
    
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
  data_model_plot <- eventReactive(input$Model_graph_fit_select, {
    temp <- subset(
      my_data(),
      select = c(
        input$ModelIV,
        input$ModelSubIV,
        input$SelectID,
        input$SelectTime,
        input$ModelPheno
      )
    )
    temp[, input$SelectTime] <-
      as.numeric(as.character(temp[, input$SelectTime]))
    sub_set <- c(input$ModelIV, input$ModelSubIV, input$SelectID)
    things_to_model <- unique(temp[sub_set])
    
    temp$selection <-
      paste(temp[, input$ModelIV], temp[, input$ModelSubIV], temp[, input$SelectID], sep =
              "_")
    docelowy <-
      subset(temp, temp$selection == input$Model_graph_fit_select)
    docelowy
  })
  
  # Fit-Plot - select specific sample to look at
  Fit_plotski_id <- eventReactive(input$Go_Model, {
    temp <- subset(
      my_data(),
      select = c(
        input$ModelIV,
        input$ModelSubIV,
        input$SelectID,
        input$SelectTime,
        input$ModelPheno
      )
    )
    temp[, input$SelectTime] <-
      as.numeric(as.character(temp[, input$SelectTime]))
    sub_set <- c(input$ModelIV, input$ModelSubIV, input$SelectID)
    things_to_model <- unique(temp[sub_set])
    
    temp$selection <-
      paste(temp[, input$ModelIV], temp[, input$ModelSubIV], temp[, input$SelectID], sep =
              "_")
    docelowy <-
      subset(temp,
             temp$selection == input$Model_graph_fit_select_multi)
  })
  
  
  
  # Fit-plots graph - single
  Model_plot_single <- eventReactive(input$Model_graph_fit_select, {
    docelowy <- data_model_plot()
    
    if (input$model == "lin") {
      pheno <- docelowy[, input$ModelPheno]
      time <- docelowy[, input$SelectTime]
      title <- unique(docelowy$selection)
      plot(
        pheno ~ time,
        main = title,
        ylab = input$ModelPheno,
        xlab = input$SelectTime
      )
      abline(lm(pheno ~ time), col = "red")
    }
    
    if (input$model == "quad") {
      docelowy$helper <- sqrt(docelowy[, input$ModelPheno])
      pheno <- docelowy$helper
      time <- docelowy[, input$SelectTime]
      title <- unique(docelowy$selection)
      plot(
        pheno ~ time,
        main = title,
        ylab = paste("sqrt(", input$ModelPheno, ")"),
        xlab = input$SelectTime
      )
      abline(lm(pheno ~ time), col = "red")
    }
    
    if (input$model == "exp") {
      docelowy$helper <- log(docelowy[, input$ModelPheno])
      pheno <- docelowy$helper
      time <- docelowy[, input$SelectTime]
      title <- unique(docelowy$selection)
      plot(
        pheno ~ time,
        main = title,
        ylab = paste("log(", input$ModelPheno, ")"),
        xlab = input$SelectTime
      )
      abline(lm(pheno ~ time), col = "red")
    }
    
    if (input$model == "sqr") {
      docelowy$helper <- (docelowy[, input$ModelPheno]) ^ 2
      pheno <- docelowy$helper
      time <- docelowy[, input$SelectTime]
      title <- unique(docelowy$selection)
      plot(
        pheno ~ time,
        main = title,
        ylab = paste(input$ModelPheno, "^2"),
        xlab = input$SelectTime
      )
      abline(lm(pheno ~ time), col = "red")
    }
  })
  
  ## == >> Having a go at the dynamic ploting thing << == ##
  
  output$Model_graph_fit_select_multi_input <- renderUI({
    if (input$Select_model_type_plot == "single plot") {
      return()
    }
    else{
      tagList(
        selectizeInput(
          inputId = "Model_graph_fit_select_multi",
          label = "Plot the fit-plots for",
          choices = c(
            "r-square values (low to high)",
            "r-square values (high to low)",
            "DELTA values (low to high)",
            "DELTA values (high to low)",
            "INTERCEPT values (low to high)",
            "INTERCEPT values (high to low)"
          )
        )
      )
    }
  })
  
  output$Go_fitplot_model <- renderUI({
    if (input$Select_model_type_plot == "single plot") {
      return()
    }
    else{
      actionButton(inputId = "Go_fitplot",
                   icon = icon("magic"),
                   label = "unleash multiple fit plots galery")
    }
  })
  
  output$Fit_plot_slider_input <- renderUI({
    if (input$Select_model_type_plot == "single plot") {
      return()
    }
    else{
      maxi <- nrow(Model_temp_data()) - 19
      sliderInput(
        inputId = "Fit_plot_slider",
        label = "Select starting slice of the samples you would like to view",
        min = 1,
        max = maxi,
        value = 1,
        step = 20
      )
    }
  })
  
  example_model <- eventReactive(input$Go_fitplot, {
    test <- as.data.frame(Model_temp_data())
    
    if (input$Model_graph_fit_select_multi == "r-square values (low to high)") {
      test2 <- test[order(test$r_squared), ]
    }
    if (input$Model_graph_fit_select_multi == "r-square values (high to low)") {
      test2 <- test[order(-test$r_squared), ]
    }
    if (input$Model_graph_fit_select_multi == "DELTA values (low to high)") {
      test2 <- test[order(test$DELTA), ]
    }
    if (input$Model_graph_fit_select_multi == "DELTA values (high to low)") {
      test2 <- test[order(-test$DELTA), ]
    }
    if (input$Model_graph_fit_select_multi == "INTERCEPT values (low to high)") {
      test2 <- test[order(test$INTERCEPT), ]
    }
    if (input$Model_graph_fit_select_multi == "INTERCEPT values (high to low)") {
      test2 <- test[order(-test$INTERCEPT), ]
    }
    test3 <-
      test2[input$Fit_plot_slider:(input$Fit_plot_slider + 19), ]
    test3
    
    test3$lista <-
      do.call(paste, c(test3[c(input$ModelIV, input$ModelSubIV, input$SelectID)], sep = "_"))
    real_list <- unique(test3$lista)
    
    temp <- subset(
      my_data(),
      select = c(
        input$ModelIV,
        input$ModelSubIV,
        input$SelectID,
        input$SelectTime,
        input$ModelPheno
      )
    )
    temp[, input$SelectTime] <-
      as.numeric(as.character(temp[, input$SelectTime]))
    temp$lista <-
      do.call(paste, c(temp[c(input$ModelIV, input$ModelSubIV, input$SelectID)], sep = "_"))
    temp
    temp2 <- temp[temp$lista %in% real_list, ]
    temp2
  })
  
  Fit_plot_multi_graphs <- eventReactive(input$Go_fitplot, {
    docelowy <- example_model()
    real_list <- unique(docelowy$lista)
    
    par(mfrow = c(4, 5))
    
    
    for (i in 1:length(real_list)) {
      super_temp <- subset(docelowy, docelowy$lista == real_list[i])
      
      # The graph instruction from single plot - so should work :P
      if (input$model == "lin") {
        pheno <- super_temp[, input$ModelPheno]
        time <- super_temp[, input$SelectTime]
        title <- real_list[i]
        plot(
          pheno ~ time,
          main = title,
          ylab = input$ModelPheno,
          xlab = input$SelectTime
        )
        abline(lm(pheno ~ time), col = "red")
      }
      
      if (input$model == "quad") {
        super_temp$helper <- sqrt(super_temp[, input$ModelPheno])
        pheno <- super_temp$helper
        time <- super_temp[, input$SelectTime]
        title <- real_list[i]
        plot(
          pheno ~ time,
          main = title,
          ylab = paste("sqrt(", input$ModelPheno, ")"),
          xlab = input$SelectTime
        )
        abline(lm(pheno ~ time), col = "red")
      }
      
      if (input$model == "exp") {
        super_temp$helper <- log(super_temp[, input$ModelPheno])
        pheno <- super_temp$helper
        time <- super_temp[, input$SelectTime]
        title <- real_list[i]
        plot(
          pheno ~ time,
          main = title,
          ylab = paste("log(", input$ModelPheno, ")"),
          xlab = input$SelectTime
        )
        abline(lm(pheno ~ time), col = "red")
      }
      
      if (input$model == "sqr") {
        super_temp$helper <- (super_temp[, input$ModelPheno]) ^ 2
        pheno <- super_temp$helper
        time <- super_temp[, input$SelectTime]
        title <- real_list[i]
        plot(
          pheno ~ time,
          main = title,
          ylab = paste(input$ModelPheno, "^2"),
          xlab = input$SelectTime
        )
        abline(lm(pheno ~ time), col = "red")
      }
    }
  })
  
  output$Fit_plot_only_graph <- renderPlot({
    if (input$Select_model_type_plot == "single plot") {
      Model_plot_single()
    }
    if (input$Select_model_type_plot == "multiple plots") {
      Fit_plot_multi_graphs()
    }
  })
  
  output$Model_download_button <- renderUI({
    if (is.null(Model_temp_data())) {
      return()
    }
    else
      downloadButton("Download_model_data", label = "Download Fitted data")
  })
  
  output$Download_model_data <- downloadHandler(
    filename = paste(
      "Modelled_",
      input$ModelPheno,
      "_with_",
      input$model ,
      "_MVApp.csv"
    ),
    content <- function(file) {
      write.csv(Model_temp_data(), file)
    }
  )
  
  # I dont think we should be using this - can only lead to problems!!!
  # Fusing the model data to the data that can be used for Summary Stats
  Full_set_from_modeling <- eventReactive(input$Go_SaveModelData, {
    melted_icecream <-
      melt(
        my_data(),
        id = c(
          input$SelectGeno,
          input$SelectIV,
          input$SelectID,
          input$SelectTime
        )
      )
    model <- Saved_model_data()
    brain_fusion <- rbind(melted_icecream, model)
    return(brain_fusion)
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  #  - - - - - - - - - - >> DATA CURATION IN 4th TAB << - - - - - - - - - - - - - -
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # - - - - - - - - - - - - - >>  Interactive inputs << - - - - - - - - - - - - - -
  
  # Select which grouping variables you would like to use for outliers
  output$IV_outliers_selection <- renderUI({
    if (is.null(ItemList())) {
      return()
    }
    else
      tagList(
        selectizeInput(
          "IV_outliers",
          label = "Select the Indepentent Variables for which you would like to group yor phenotypes for outlier selection",
          choices = c(
            input$SelectGeno,
            input$SelectIV,
            input$SelectTime,
            input$SelectID
          ),
          multiple = TRUE
        )
      )
  })
  
  output$Outliers_selection_pheno  <- renderUI({
    if (is.null(Outlier_overview())) {
      return()
    }
    if (input$Out_pheno_single_multi == "Single phenotype") {
      return()
    }
    if (input$Out_pheno_single_multi == "All phenotypes") {
      data <- Outlier_overview()
      max_out <- max(data$Add_outliers)
      min_out <- min(data$Add_outliers)
      sliderInput(
        "outlier_cutoff",
        "Remove the samples which are characterized as an outlier in at least x traits:",
        min_out,
        max_out,
        value = 2,
        step = 1
      )
    }
  })
  
  # Chose the phenotype based on which you want to select the outliers
  
  output$Pheno_outliers <- renderUI({
    if (is.null(ItemList()) |
        (input$Out_pheno_single_multi == "All phenotypes")) {
      return()
    }
    else
      tagList(
        selectizeInput(
          "DV_outliers",
          label = "Select the phenotype for which you would like to determine the outliers",
          choices = input$SelectDV,
          multiple = F
        )
      )
  })
  
  # Chose the phenotype for the graphs
  
  output$Pheno_graph_outliers <- renderUI({
    if (is.null(ItemList()))
    {
      return()
    }
    else
      tagList(
        selectizeInput(
          "DV_graph_outliers",
          label = "Select the phenotype for which you would like to examine graphically",
          choices = input$SelectDV,
          selected = input$SelectDV[1],
          multiple = F
        )
      )
  })
  
  
  output$Q_facet <- renderUI({
    if (input$outlier_facet == T) {
      tagList(selectInput(
        "Facet_choice",
        "Select variable for which to facet",
        choices = c(input$SelectGeno, input$SelectIV, input$SelectTime)
      ))
    }
    else{
      return()
    }
  })
  
  output$Q_colour <- renderUI({
    if (input$outlier_colour == T) {
      tagList(
        selectInput(
          "Colour_choice",
          "Select variable for which to colour code",
          choices = c(input$SelectGeno, input$SelectIV, input$SelectTime)
        )
      )
    }
    else{
      return()
    }
  })
  
  output$Facet_user_input_columns <- renderUI({
    if (input$outlier_facet == F) {
      return()
    }
    if (input$outlier_facet == T) {
      sliderInput("out_graph_facet_col",
                  label = "How many columns would you like to use for facetting?",
                  1, 9, 3)
    }
  })
  
  output$Facet_outlier_scale <- renderUI({
    if (input$outlier_facet == F) {
      return()
    }
    if (input$outlier_facet == T) {
      selectizeInput(
        "out_facet_scale",
        label = "The scale of the graphs is",
        choices = c("fixed", "free", "free_x", "free_y")
      )
      
    }
  })
  
  output$Outlier_error_bar <- renderUI({
    if (input$outlier_graph_type == "bar plot") {
      tagList(
        selectizeInput(
          "out_error_bar",
          label = "Error bars represent",
          choices = c("Standard Error", "Standard Deviation"),
          multiple = F
        )
      )
    }
  })
  
  # - - - - - - - - - - - - - >>  MAIN CALCULATIONS << - - - - - - - - - - - - - -
  
  ## TESTING OMIT.NA     %% Mitch %%
  my_data_nona <- eventReactive(input$Go_omitna == T, {
    my_data_nona <-
      my_data()[complete.cases(my_data()), ] #use na.omit instead maybe?
    return(my_data_nona)
  })
  
  # General outlier testing table => highlighting the plants with problems in multiple traits:
  Outlier_overview <- eventReactive(input$Go_outliers, {
    if (input$Go_omitna == T) {
      faka_boom <- my_data_nona()
    }
    if (input$Go_omitna == F) {
      faka_boom <- my_data()
    }
    faka_boom$id_test <-
      do.call(paste, c(faka_boom[c(input$IV_outliers)], sep = "_"))
    
    for (i in 1:length(input$SelectDV)) {
      if (input$outlier_method == "1.5*IQR away from the mean") {
        faka_laka <-
          subset(faka_boom, select = c("id_test", input$SelectDV[i]))
        faka_laka$pheno <- faka_laka[, (input$SelectDV[i])]
        bad_shit <- boxplot(faka_laka$pheno ~ faka_laka$id_test)$out
        # loop to add outliers:
        for (f in 1:nrow(faka_boom)) {
          if (faka_laka$pheno[f] %in% bad_shit) {
            faka_boom$outlier[f] <- TRUE
          }
          else{
            faka_boom$outlier[f] <- FALSE
          }
        }
        colnames(faka_boom)[which(names(faka_boom) == "outlier")] <-
          paste("out", input$SelectDV[i], sep = "_")
      }
      
      if (input$outlier_method == "Cook's Distance") {
        faka_laka <-
          subset(faka_boom, select = c("id_test", input$SelectDV[i]))
        faka_laka$pheno <- faka_laka[, (input$SelectDV[i])]
        mod <- lm(faka_laka$pheno ~ faka_laka$id_test)
        cooksd <- cooks.distance(mod)
        new_name <-
          do.call(paste, c(faka_laka[c(input$SelectDV[i])], "outl", sep = "_"))
        faka_boom$outlier <- cooksd > 4 * mean(cooksd)
        colnames(faka_boom)[which(names(faka_boom) == "outlier")] <-
          paste("out", input$SelectDV[i], sep = "_")
      }
      
      if (input$outlier_method == "Bonferonni outlier test") {
        faka_laka <-
          subset(faka_boom, select = c("id_test", input$SelectDV[i]))
        faka_laka$pheno <- faka_laka[, input$SelectDV[i]]
        mod <- lm(faka_laka$pheno ~ faka_laka$id_test)
        baddies <- car::outlierTest(mod)
        bad_shit <- names(baddies[[1]])
        bad_shit <- as.numeric(bad_shit)
        colnames(faka_boom)[endCol + i] <-
          paste("out", input$SelectDV[i], sep = "")
        faka_boom$outlier <- FALSE
        faka_boom[bad_shit, ]$outlier <- TRUE
        colnames(faka_boom)[which(names(faka_boom) == "outlier")] <-
          paste("out", input$SelectDV[i], sep = "_")
      }
      
      if (input$outlier_method == "1xStDev from the median") {
        faka_laka <-
          subset(faka_boom, select = c("id_test", input$SelectDV[i]))
        faka_laka$pheno <- faka_laka[, input$SelectDV[i]]
        faka_sum <-
          summaryBy(
            pheno ~ id_test,
            data = faka_laka,
            FUN = function(x) {
              c(median = median(x), sd = sd(x))
            }
          )
        faka_sum$min <-
          (faka_sum$pheno.median - (1 * faka_sum$pheno.sd))
        faka_sum$max <-
          (faka_sum$pheno.median + (1 * faka_sum$pheno.sd))
        faka_sum <-
          subset(faka_sum, select = c("id_test", "min", "max"))
        faka_laka <- merge(faka_laka, faka_sum, by = "id_test")
        
        
        for (e in 1:nrow(faka_laka)) {
          if (faka_laka$pheno[e] > faka_laka$max[e]) {
            faka_boom$outlier[e] <- TRUE
          }
          if (faka_laka$pheno[e] < faka_laka$min[e]) {
            faka_boom$outlier[e] <- TRUE
          }
          else{
            faka_boom$outlier[e] <- FALSE
          }
        }
        drops <- c("min", "max")
        faka_boom <- faka_boom[,!(names(faka_boom) %in% drops)]
        colnames(faka_boom)[which(names(faka_boom) == "outlier")] <-
          paste("out", input$SelectDV[i], sep = "_")
      }
      
      if (input$outlier_method == "2xStDev from the median") {
        faka_laka <-
          subset(faka_boom, select = c("id_test", input$SelectDV[i]))
        faka_laka$pheno <- faka_laka[, input$SelectDV[i]]
        faka_sum <-
          summaryBy(
            pheno ~ id_test,
            data = faka_laka,
            FUN = function(x) {
              c(median = median(x), sd = sd(x))
            }
          )
        faka_sum$min <-
          (faka_sum$pheno.median - (2 * faka_sum$pheno.sd))
        faka_sum$max <-
          (faka_sum$pheno.median + (2 * faka_sum$pheno.sd))
        faka_sum <-
          subset(faka_sum, select = c("id_test", "min", "max"))
        faka_laka <- merge(faka_laka, faka_sum, by = "id_test")
        
        
        for (e in 1:nrow(faka_laka)) {
          if (faka_laka$pheno[e] > faka_laka$max[e]) {
            faka_boom$outlier[e] <- TRUE
          }
          if (faka_laka$pheno[e] < faka_laka$min[e]) {
            faka_boom$outlier[e] <- TRUE
          }
          else{
            faka_boom$outlier[e] <- FALSE
          }
        }
        drops <- c("min", "max")
        faka_boom <- faka_boom[,!(names(faka_boom) %in% drops)]
        colnames(faka_boom)[which(names(faka_boom) == "outlier")] <-
          paste("out", input$SelectDV[i], sep = "_")
      }
      
      if (input$outlier_method == "2.5xStDev from the median") {
        faka_laka <-
          subset(faka_boom, select = c("id_test", input$SelectDV[i]))
        faka_laka$pheno <- faka_laka[, input$SelectDV[i]]
        faka_sum <-
          summaryBy(
            pheno ~ id_test,
            data = faka_laka,
            FUN = function(x) {
              c(median = median(x), sd = sd(x))
            }
          )
        faka_sum$min <-
          (faka_sum$pheno.median - (2.5 * faka_sum$pheno.sd))
        faka_sum$max <-
          (faka_sum$pheno.median + (2.5 * faka_sum$pheno.sd))
        faka_sum <-
          subset(faka_sum, select = c("id_test", "min", "max"))
        faka_laka <- merge(faka_laka, faka_sum, by = "id_test")
        
        
        for (e in 1:nrow(faka_laka)) {
          if (faka_laka$pheno[e] > faka_laka$max[e]) {
            faka_boom$outlier[e] <- TRUE
          }
          if (faka_laka$pheno[e] < faka_laka$min[e]) {
            faka_boom$outlier[e] <- TRUE
          }
          else{
            faka_boom$outlier[e] <- FALSE
          }
        }
        drops <- c("min", "max")
        faka_boom <- faka_boom[,!(names(faka_boom) %in% drops)]
        colnames(faka_boom)[which(names(faka_boom) == "outlier")] <-
          paste("out", input$SelectDV[i], sep = "_")
      }
      
      if (input$outlier_method == "3xStDev from the median") {
        faka_laka <-
          subset(faka_boom, select = c("id_test", input$SelectDV[i]))
        faka_laka$pheno <- faka_laka[, input$SelectDV[i]]
        faka_sum <-
          summaryBy(
            pheno ~ id_test,
            data = faka_laka,
            FUN = function(x) {
              c(median = median(x), sd = sd(x))
            }
          )
        faka_sum$min <-
          (faka_sum$pheno.median - (3 * faka_sum$pheno.sd))
        faka_sum$max <-
          (faka_sum$pheno.median + (3 * faka_sum$pheno.sd))
        faka_sum <-
          subset(faka_sum, select = c("id_test", "min", "max"))
        faka_laka <- merge(faka_laka, faka_sum, by = "id_test")
        
        
        for (e in 1:nrow(faka_laka)) {
          if (faka_laka$pheno[e] > faka_laka$max[e]) {
            faka_boom$outlier[e] <- TRUE
          }
          if (faka_laka$pheno[e] < faka_laka$min[e]) {
            faka_boom$outlier[e] <- TRUE
          }
          else{
            faka_boom$outlier[e] <- FALSE
          }
        }
        drops <- c("min", "max")
        faka_boom <- faka_boom[,!(names(faka_boom) %in% drops)]
        colnames(faka_boom)[which(names(faka_boom) == "outlier")] <-
          paste("out", input$SelectDV[i], sep = "_")
      }
    }
    
    drops <- ("id_test")
    faka_boom <- faka_boom[,!(names(faka_boom) %in% drops)]
    
    dropski <- c(input$IV_outliers, input$SelectDV)
    faka_kaboom <- faka_boom[,!(names(faka_boom) %in% dropski)]
    
    for (x in 1:nrow(faka_boom)) {
      z <- faka_kaboom[x, ]
      faka_boom$Add_outliers[x] <- length(z[z == TRUE])
    }
    
    return(faka_boom)
  })
  
  
  # Testing the outliers based on single phenotype
  
  Outlier_data <- eventReactive(input$Go_outliers, {
    if (input$Go_omitna == T) {
      data_outl <- my_data_nona()
    }
    if (input$Go_omitna == F) {
      data_outl <- my_data()
    }
    
    outl <-
      subset(
        data_outl,
        select = c(
          input$SelectGeno,
          input$SelectIV,
          input$SelectTime,
          input$SelectID,
          input$DV_outliers
        )
      )
    outl$id_test <-
      do.call(paste, c(outl[c(input$IV_outliers)], sep = "_"))
    
    outl$pheno <- outl[, input$DV_outliers]
    
    # outliers based on 1.5IQR
    if (input$outlier_method == "1.5*IQR away from the mean") {
      bad_shit <- boxplot(outl$pheno ~ outl$id_test)$out
      # Adding all the outliers as a column "outlier" with 1/0 values
      for (e in 1:nrow(outl)) {
        if (outl[, input$DV_outliers][e] %in% bad_shit) {
          outl$outlier[e] <- TRUE
        }
        else{
          outl$outlier[e] <- FALSE
        }
      }
      drops <- c("pheno", "id_test")
      outl <- outl[,!(names(outl) %in% drops)]
    }
    
    # outliers based on Cooks distance
    if (input$outlier_method == "Cook's Distance") {
      mod <- lm(outl$pheno ~ outl$id_test)
      cooksd <- cooks.distance(mod)
      outl$outlier <- cooksd > 4 * mean(cooksd)
      drops <- c("pheno", "id_test")
      outl <- outl[,!(names(outl) %in% drops)]
    }
    
    # outliers based on car::outlierTest
    if (input$outlier_method == "Bonferonni outlier test") {
      mod <- lm(outl$pheno ~ outl$id_test)
      baddies <- car::outlierTest(mod)
      bad_shit <- names(baddies[[1]])
      bad_shit <- as.numeric(bad_shit)
      outl$outlier <- FALSE
      outl[bad_shit, ]$outlier <- TRUE
      drops <- c("pheno", "id_test")
      outl <- outl[,!(names(outl) %in% drops)]
    }
    
    # outliers based on mean + SD
    
    if (input$outlier_method == "1xStDev from the median") {
      out_sum <-
        summaryBy(
          pheno ~ id_test,
          data = outl,
          FUN = function(x) {
            c(median = median(x), sd = sd(x))
          }
        )
      out_sum$min <- (out_sum$pheno.median - (1 * out_sum$pheno.sd))
      out_sum$max <- (out_sum$pheno.median + (1 * out_sum$pheno.sd))
      out_sum <- subset(out_sum, select = c("id_test", "min", "max"))
      outl <- merge(outl, out_sum, by = "id_test")
      for (i in 1:nrow(outl)) {
        if (outl$pheno[i] > outl$max[i]) {
          outl$outlier[i] <- TRUE
        }
        if (outl$pheno[i] < outl$min[i]) {
          outl$outlier[i] <- TRUE
        }
        else{
          outl$outlier[i] <- FALSE
        }
      }
      drops <- c("min", "max", "pheno", "id_test")
      outl <- outl[,!(names(outl) %in% drops)]
    }
    
    # outliers based on mean + 2*SD
    
    if (input$outlier_method == "2xStDev from the median") {
      out_sum <-
        summaryBy(
          pheno ~ id_test,
          data = outl,
          FUN = function(x) {
            c(median = median(x), sd = sd(x))
          }
        )
      out_sum$min <- (out_sum$pheno.median - (2 * out_sum$pheno.sd))
      out_sum$max <- (out_sum$pheno.median + (2 * out_sum$pheno.sd))
      out_sum <- subset(out_sum, select = c("id_test", "min", "max"))
      outl <- merge(outl, out_sum, by = "id_test")
      for (i in 1:nrow(outl)) {
        if (outl$pheno[i] > outl$max[i]) {
          outl$outlier[i] <- TRUE
        }
        if (outl$pheno[i] < outl$min[i]) {
          outl$outlier[i] <- TRUE
        }
        else{
          outl$outlier[i] <- FALSE
        }
      }
      drops <- c("min", "max", "pheno", "id_test")
      outl <- outl[,!(names(outl) %in% drops)]
    }
    
    # outliers based on mean + 2.5*SD
    
    if (input$outlier_method == "2.5xStDev from the median") {
      out_sum <-
        summaryBy(
          pheno ~ id_test,
          data = outl,
          FUN = function(x) {
            c(median = median(x), sd = sd(x))
          }
        )
      out_sum$min <- (out_sum$pheno.median - (2.5 * out_sum$pheno.sd))
      out_sum$max <- (out_sum$pheno.median + (2.5 * out_sum$pheno.sd))
      out_sum <- subset(out_sum, select = c("id_test", "min", "max"))
      outl <- merge(outl, out_sum, by = "id_test")
      for (i in 1:nrow(outl)) {
        if (outl$pheno[i] > outl$max[i]) {
          outl$outlier[i] <- TRUE
        }
        if (outl$pheno[i] < outl$min[i]) {
          outl$outlier[i] <- TRUE
        }
        else{
          outl$outlier[i] <- FALSE
        }
      }
      drops <- c("min", "max", "pheno", "id_test")
      outl <- outl[,!(names(outl) %in% drops)]
    }
    
    # outliers based on mean + 3*SD
    
    if (input$outlier_method == "3xStDev from the median") {
      out_sum <-
        summaryBy(
          pheno ~ id_test,
          data = outl,
          FUN = function(x) {
            c(median = median(x), sd = sd(x))
          }
        )
      out_sum$min <- (out_sum$pheno.median - (3 * out_sum$pheno.sd))
      out_sum$max <- (out_sum$pheno.median + (3 * out_sum$pheno.sd))
      out_sum <- subset(out_sum, select = c("id_test", "min", "max"))
      outl <- merge(outl, out_sum, by = "id_test")
      for (i in 1:nrow(outl)) {
        if (outl$pheno[i] > outl$max[i]) {
          outl$outlier[i] <- TRUE
        }
        if (outl$pheno[i] < outl$min[i]) {
          outl$outlier[i] <- TRUE
        }
        else{
          outl$outlier[i] <- FALSE
        }
      }
      drops <- c("min", "max", "pheno", "id_test")
      outl <- outl[,!(names(outl) %in% drops)]
    }
    
    return(outl)
    
  })
  
  
  Outliers_final_data <- eventReactive(input$Go_outliers, {
    if (input$Out_pheno_single_multi == "All phenotypes") {
      data_blob <- Outlier_overview()
    }
    if (input$Out_pheno_single_multi == "Single phenotype") {
      data_blob <- Outlier_data()
    }
    return(data_blob)
  })
  
  
  # - - - - - - - - - - - - - >>  OUTPUT TABLES / GRAPHS / DOWNLOAD BUTTONS << - - - - - - - - - - - - - -
  
  # Table with outliers marked out
  output$Outlier_overview_table <- DT::renderDataTable({
    test <- Outliers_final_data()
    # WE NEED TO MAKE CONDITIONAL FORMATING OF THIS!!!
    # right now the table doesnt show when you do SINGLE PHENOTYPE because the formating is for ALL PHENOTYPES
    # I tried to include if() statements in this position, but then it doesnt work (formating - but the table still shows up)
    # not sure how to proceed.... aiaiai :(((
    
    # datatable(test) %>% formatStyle( "Add_outliers",
    #    target = 'row',
    #    backgroundColor = styleInterval((input$outlier_cutoff-1), c("white", "pink")))
  })
  
  # Outlier report
  output$Outlier_report <- renderText({
    tescior <- Outliers_final_data()
    if (input$Out_pheno_single_multi == "All phenotypes") {
      number0 <-
        subset(tescior, tescior$Add_outliers >= input$outlier_cutoff)
      number <- nrow(number0)
      pheno <- paste(input$Out_pheno_single_multi)
      method <-
        paste(
          input$outlier_method,
          ". The sample is characterized as an outlier when it is classified as such in at least ",
          input$outlier_cutoff,
          " traits"
        )
    }
    
    if (input$Out_pheno_single_multi == "Single phenotype") {
      number0 <- subset(tescior, tescior$outlier == "TRUE")
      number <- nrow(number0)
      pheno <- paste(input$DV_outliers)
      method <- paste(input$outlier_method)
    }
    sentence <-
      paste("There are ",
            number,
            " outliers identified based on",
            pheno,
            "using",
            method)
    return(sentence)
  })
  
  # Outlier free data (and table)
  Outlier_free_data <- eventReactive(input$Go_outliers, {
    if (input$Out_pheno_single_multi == "All phenotypes") {
      good_shit <- Outliers_final_data()
      good_shit2 <-
        subset(good_shit,
               good_shit$Add_outliers < input$outlier_cutoff)
      to_trash <- paste("out", input$SelectDV[1], sep = "_")
      for (i in 2:length(input$SelectDV)) {
        new_trash <- paste("out", input$SelectDV[i], sep = "_")
        to_trash <- c(to_trash, new_trash)
      }
      to_trash <- c(to_trash, "Add_outliers")
    }
    if (input$Out_pheno_single_multi == "Single phenotype") {
      good_shit <-  Outliers_final_data()
      good_shit2 <- subset(good_shit, good_shit$outlier == FALSE)
      to_trash <- "outlier"
    }
    good_shit2 <- good_shit2[,!(names(good_shit2)) %in% to_trash]
    return(good_shit2)
  })
  
  # Outlier only data (and table)
  Outlier_only_data <- eventReactive(input$Go_outliers, {
    if (input$Out_pheno_single_multi == "All phenotypes") {
      bad_shit <- Outliers_final_data()
      bad_shit2 <-
        subset(bad_shit, bad_shit$Add_outliers >= input$outlier_cutoff)
    }
    if (input$Out_pheno_single_multi == "Single phenotype") {
      bad_shit <- Outliers_final_data()
      bad_shit2 <- subset(bad_shit, bad_shit$outlier == TRUE)
    }
    
    return(bad_shit2)
  })
  
  # Table without the outliers
  output$Outlier_free_table <- renderDataTable({
    Outlier_free_data()
  })
  
  # Table containig only the outliers
  output$Outlier_only_table <- renderDataTable({
    Outlier_only_data()
  })
  
  
  # Download table with and without the outliers:
  
  output$Full_outlier_download <- renderUI({
    if (is.null(Outliers_final_data())) {
      return()
    }
    else{
      downloadButton("full_data_outliers", label = "Download table with indicated outliers")
    }
  })
  
  output$full_data_outliers <- downloadHandler(
    filename = paste(
      "Marked_outliers_based_on_",
      input$Out_pheno_single_multi,
      "_",
      input$DV_outliers ,
      "_identified_with_",
      input$outlier_method,
      "_MVApp.csv"
    ),
    content <- function(file) {
      write.csv(Outliers_final_data(), file)
    }
  )
  
  
  # Download table with the outliers:
  
  output$Pheno_outlier_download <- renderUI({
    if (is.null(Outliers_final_data())) {
      return()
    }
    else{
      downloadButton("data_outliers", label = "Download table containing the outlier values")
    }
  })
  
  output$data_outliers <- downloadHandler(
    filename = paste(
      "Outliers_based_on_",
      input$Out_pheno_single_multi,
      "_",
      input$DV_outliers ,
      "_identified_with_",
      input$outlier_method,
      "_MVApp.csv"
    ),
    content <- function(file) {
      write.csv(Outlier_only_data(), file)
    }
  )
  
  # Download table free from the outliers:
  
  output$Pheno_outlier_free_download <- renderUI({
    if (is.null(Outliers_final_data())) {
      return()
    }
    else{
      downloadButton("data_clean_final", label = "Download table clean of the outlier values")
    }
  })
  
  output$data_clean_final <- downloadHandler(
    filename = paste(
      "Data_free_from_outliers_based_on",
      input$Out_pheno_single_multi,
      "_",
      input$DV_outliers ,
      "_identified_with_",
      input$outlier_method,
      "_MVApp.csv"
    ),
    content <- function(file) {
      write.csv(Outlier_free_data(), file)
    }
  )
  
  # Lock the no-outlier data for further analysis:
  output$Outliers_save <- renderUI({
    if (is.null(Outliers_final_data())) {
      return()
    }
    else{
      actionButton("lock_outliers",
                   icon = icon("thumbs-o-up"),
                   label = "Lock this outlier-free data for further analysis")
    }
  })
  
  no_outliers <- eventReactive(input$lock_outliers, {
    tratata <- Outlier_free_data()
    return(tratata)
  })
  
  # = = = >> GRAPH CONTAINING ALL THE DATA << = = =
  output$outlier_graph <- renderPlotly({
    if (input$Go_omitna == T) {
      data_outl <- my_data_nona()
    }
    if (input$Go_omitna == F) {
      data_outl <- my_data()
    }
    
    outl <-
      subset(
        data_outl,
        select = c(
          input$SelectGeno,
          input$SelectIV,
          input$SelectTime,
          input$SelectID,
          input$DV_graph_outliers
        )
      )
    lista <- input$IV_outliers
    
    if (input$outlier_facet == T) {
      listb <- input$Facet_choice
      outl$listb <- outl[, input$Facet_choice]
      lista <- setdiff(lista, listb)
    }
    
    if (input$outlier_colour == T) {
      listx <- input$Colour_choice
      outl$listx <- outl[, input$Colour_choice]
      listax <- setdiff(lista, listx)
      outl$listax <- do.call(paste, c(outl[listax], sep = "_"))
    }
    
    phenotype <- input$DV_graph_outliers
    outl$pheno <- outl[, input$DV_graph_outliers]
    
    
    outl$id_test <- do.call(paste, c(outl[lista], sep = "_"))
    
    if (input$outlier_graph_type == "bar plot") {
      outl$pheno <- as.numeric(outl$pheno)
      if (input$outlier_colour == T) {
        if (input$outlier_facet == F) {
          out_sum <-
            summaryBy(
              pheno ~ listax + listx,
              data = outl,
              FUN = function(x) {
                c(m = mean(x),
                  s = sd(x),
                  se = std.error(x))
              }
            )
          taka <-
            ggplot(out_sum, aes(
              x = listax,
              y = pheno.m,
              fill = listx
            ))
          #taka <- taka + guides(fill=guide_legend(title=input$outlier_colour))
        }
        if (input$outlier_facet == T) {
          out_sum <-
            summaryBy(
              pheno ~ listax + listx + listb,
              data = outl,
              FUN = function(x) {
                c(m = mean(x),
                  s = sd(x),
                  se = std.error(x))
              }
            )
          taka <-
            ggplot(out_sum, aes(
              x = listax,
              y = pheno.m,
              fill = listx
            ))
        }
      }
      
      if (input$outlier_colour == F) {
        if (input$outlier_facet == T) {
          out_sum <-
            summaryBy(
              pheno ~ id_test + listb,
              data = outl,
              FUN = function(x) {
                c(m = mean(x),
                  s = sd(x),
                  se = std.error(x))
              }
            )
          taka <- ggplot(out_sum, aes(x = id_test, y = pheno.m))
        }
        if (input$outlier_facet == F) {
          out_sum <-
            summaryBy(
              pheno ~ id_test,
              data = outl,
              FUN = function(x) {
                c(m = mean(x),
                  s = sd(x),
                  se = std.error(x))
              }
            )
          taka <- ggplot(out_sum, aes(x = id_test, y = pheno.m))
        }
      }
      
      taka <-
        taka + geom_bar(stat = "identity", position = position_dodge(1))
      
      if (input$out_error_bar == "Standard Deviation") {
        taka <-
          taka + geom_errorbar(aes(ymin = pheno.m - pheno.s, ymax = pheno.m + pheno.s),
                               position = position_dodge(1))
      }
      if (input$out_error_bar == "Standard Error") {
        taka <-
          taka + geom_errorbar(aes(ymin = pheno.m - pheno.se, ymax = pheno.m + pheno.se),
                               position = position_dodge(1))
      }
    }
    
    
    if (input$outlier_graph_type == "box plot") {
      if (input$outlier_colour == T) {
        taka <- ggplot(outl, aes(
          x = id_test,
          y = pheno,
          color = listx
        ))
        taka <-
          taka + guides(fill = guide_legend(title = input$outlier_colour))
      }
      else{
        taka <- ggplot(outl, aes(x = id_test, y = pheno))
      }
      
      taka <- taka + geom_boxplot(position = "dodge")
    }
    
    if (input$outlier_graph_type == "scatter plot") {
      if (input$outlier_colour == T) {
        taka <- ggplot(outl, aes(
          x = id_test,
          y = pheno,
          color = listx
        ))
        taka <-
          taka + guides(fill = guide_legend(title = input$outlier_colour))
      }
      else{
        taka <- ggplot(outl, aes(x = id_test, y = pheno))
      }
      
      taka <- taka + geom_point(position = position_dodge(1))
    }
    
    
    
    if (input$outlier_facet == T) {
      taka <-
        taka + facet_wrap(
          ~ listb,
          ncol = input$out_graph_facet_col,
          scale = input$out_facet_scale
        )
    }
    
    taka <- taka + theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1),
      axis.title.y = element_text(input$DV_graph_outliers)
    )
    
    taka
  })
  
  # = = = >> GRAPH WITH NO OUTLIERS << = = =
  
  output$no_outliers_graph <- renderPlotly({
    data <- Outlier_free_data()
    clean_data <-
      subset(
        data,
        select = c(
          input$SelectGeno,
          input$SelectIV,
          input$SelectTime,
          input$SelectID,
          input$DV_graph_outliers
        )
      )
    lista <- input$IV_outliers
    
    if (input$outlier_facet == T) {
      listb <- input$Facet_choice
      clean_data$listb <- clean_data[, input$Facet_choice]
      lista <- setdiff(lista, listb)
    }
    
    if (input$outlier_colour == T) {
      listx <- input$Colour_choice
      clean_data$listx <- clean_data[, input$Colour_choice]
      listax <- setdiff(lista, listx)
      clean_data$listax <-
        do.call(paste, c(clean_data[listax], sep = "_"))
    }
    
    phenotype <- input$DV_graph_outliers
    clean_data$pheno <- clean_data[, input$DV_graph_outliers]
    
    
    clean_data$id_test <-
      do.call(paste, c(clean_data[lista], sep = "_"))
    
    if (input$outlier_graph_type == "bar plot") {
      clean_data$pheno <- as.numeric(clean_data$pheno)
      if (input$outlier_colour == T) {
        if (input$outlier_facet == F) {
          clean_sum <-
            summaryBy(
              pheno ~ listax + listx,
              data = clean_data,
              FUN = function(x) {
                c(m = mean(x),
                  s = sd(x),
                  se = std.error(x))
              }
            )
          jaka <-
            ggplot(clean_sum, aes(
              x = listax,
              y = pheno.m,
              fill = listx
            ))
          #taka <- taka + guides(fill=guide_legend(title=input$outlier_colour))
        }
        if (input$outlier_facet == T) {
          clean_sum <-
            summaryBy(
              pheno ~ listax + listx + listb,
              data = clean_data,
              FUN = function(x) {
                c(m = mean(x),
                  s = sd(x),
                  se = std.error(x))
              }
            )
          jaka <-
            ggplot(clean_sum, aes(
              x = listax,
              y = pheno.m,
              fill = listx
            ))
        }
      }
      
      if (input$outlier_colour == F) {
        if (input$outlier_facet == T) {
          clean_sum <-
            summaryBy(
              pheno ~ id_test + listb,
              data = clean_data,
              FUN = function(x) {
                c(m = mean(x),
                  s = sd(x),
                  se = std.error(x))
              }
            )
          jaka <- ggplot(clean_sum, aes(x = id_test, y = pheno.m))
        }
        if (input$outlier_facet == F) {
          clean_sum <-
            summaryBy(
              pheno ~ id_test,
              data = clean_data,
              FUN = function(x) {
                c(m = mean(x),
                  s = sd(x),
                  se = std.error(x))
              }
            )
          jaka <- ggplot(clean_sum, aes(x = id_test, y = pheno.m))
        }
      }
      
      jaka <-
        jaka + geom_bar(stat = "identity", position = position_dodge(1))
      
      if (input$out_error_bar == "Standard Deviation") {
        jaka <-
          jaka + geom_errorbar(aes(ymin = pheno.m - pheno.s, ymax = pheno.m + pheno.s),
                               position = position_dodge(1))
      }
      if (input$out_error_bar == "Standard Error") {
        jaka <-
          jaka + geom_errorbar(aes(ymin = pheno.m - pheno.se, ymax = pheno.m + pheno.se),
                               position = position_dodge(1))
      }
    }
    
    if (input$outlier_graph_type == "box plot") {
      if (input$outlier_colour == T) {
        jaka <-
          ggplot(clean_data, aes(
            x = id_test,
            y = pheno,
            color = listx
          ))
        jaka <-
          jaka + guides(fill = guide_legend(title = input$outlier_colour))
      }
      else{
        jaka <- ggplot(clean_data, aes(x = id_test, y = pheno))
      }
      
      jaka <- jaka + geom_boxplot(position = "dodge")
    }
    
    if (input$outlier_graph_type == "scatter plot") {
      if (input$outlier_colour == T) {
        jaka <-
          ggplot(clean_data, aes(
            x = id_test,
            y = pheno,
            color = listx
          ))
        jaka <-
          jaka + guides(fill = guide_legend(title = input$outlier_colour))
      }
      else{
        jaka <- ggplot(clean_data, aes(x = id_test, y = pheno))
      }
      jaka <- jaka + geom_point(position = position_dodge(1))
    }
    
    if (input$outlier_facet == T) {
      jaka <-
        jaka + facet_wrap( ~ listb,
                           ncol = 3,
                           scale = input$out_facet_scale)
    }
    
    jaka <-
      jaka + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    jaka <- jaka + xlab(lista)
    jaka <- jaka + ylab(input$DV_graph_outliers)
    
    jaka
  })
  
  
  output$na_report <- renderText({
    if (input$Go_omitna == F) {
      return()
    }
    else{
      sum_na <- nrow(my_data()) - nrow(my_data_nona())
      return(paste(sum_na, " rows containing NAs that were removed."))
    }
  })
  
  # = = = = = = >> SUMMARY STATS - MOVED FROM 5th TAB << = = = = = = = = =
  
  ## Added new input "$SelectSumm"  and output "$CustomSumm"  %% Mitch %%
  
  output$Data_for_SummaryStats <- renderUI({
    if (is.null(ItemList())) {
      return ()
    } else
      tagList(
        selectizeInput(
          inputId = "SelectDataSumm",
          label = "Select the dataset to be used for the summary stats",
          choices = c("raw data", "NA removed", "outliers removed"),
          selected = "raw data",
          multiple = F
        )
      )
  })
  
  output$CustomSumm <- renderUI({
    if ((is.null(ItemList()))) {
      return ()
    } else
      tagList(
        selectizeInput(
          inputId = "SelectSumm",
          label = "Select desired summary statistics calculations to be performed",
          choices = c("Mean", "Median", "StdDev", "StdErr", "Min", "Max", "Sum"),
          multiple = T
        )
      )
  })
  
  
  ## Added list of summary functions "summfuns"   %% Mitch %%
  summfuns <- list(
    Mean = function(x)
      mean(x),
    Median = function(x)
      median(x),
    StdDev = function(x)
      sd(x),
    StdErr = function(x)
      std.error(x),
    Min = function(x)
      min(x),
    Max = function(x)
      max(x),
    Sum = function(x)
      sum(x)
  )
  
  sum_data <- eventReactive(input$Go_SummaryStat, {
    if (input$SelectDataSumm == "raw data") {
      melted_icecream <- my_data()
      drops <- input$SelectID
      melted_icecream <-
        melted_icecream[,!(names(melted_icecream) %in% drops)]
      melted_icecream <-
        melt(melted_icecream,
             id = c(input$SelectGeno, input$SelectIV, input$SelectTime))
    }
    if (input$SelectDataSumm == "Na_removed") {
      melted_icecream <- my_data_nona()
      drops <- input$SelectID
      melted_icecream <-
        melted_icecream[,!(names(melted_icecream) %in% drops)]
      melted_icecream <-
        melt(melted_icecream,
             id = c(input$SelectGeno, input$SelectIV, input$SelectTime))
    }
    if (input$SelectDataSumm == "outliers removed") {
      melted_icecream <- Outlier_free_data()
      drops <- c(input$SelectID, "outlier")
      melted_icecream <-
        melted_icecream[,!(names(melted_icecream) %in% drops)]
      melted_icecream <-
        melt(melted_icecream,
             id = c(input$SelectGeno, input$SelectIV, input$SelectTime))
    }
    
    # TO DO:
    # we need to get rid of the SelectID column before doing any Summary Stat on the data <3<3<3 MMJ <3<3<3
    
    ## Added call to selected summary stats functions "FUN=summfuns[input$SelectSumm]"     %% Mitch %%
    sum_my_data <-
      summaryBy(value ~ ., data = melted_icecream, FUN = summfuns[input$SelectSumm])
    
    ## Label columns based on chosen summary stats     %% Mitch %%
    colnames(sum_my_data) <-
      c(
        input$SelectGeno,
        input$SelectIV,
        input$SelectTime,
        "Dependent_Variable",
        input$SelectSumm
      )
    
    return(sum_my_data)
  })
  
  output$sum_data <- renderDataTable({
    sum_data()
  })
  
  output$Sum_download_button <- renderUI({
    if (is.null(sum_data())) {
      return()
    }
    else
      downloadButton("data_sum", label = "Download Summary Stats data")
  })
  
  output$data_sum <- downloadHandler(filename = "Summary_stats_MVApp.csv",
                                     content <- function(file) {
                                       write.csv(sum_data(), file)
                                     })
  
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # - - - - - - - - - - - - >> DATA EXPLORATION IN 5th TAB << - - - - - - - - - - -
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  #### HISTOGRAMS
  
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
          choices = c(input$SelectDV),
          multiple = F
        )
      )
  })
  
  output$Plotfacets <- renderUI({
    if (input$plot_facet == T) {
      tagList(
        selectInput(
          "Plotfacet_choice",
          "Select variable for which to facet",
          choices = c(setdiff(
            list(input$SelectGeno, input$SelectIV, input$SelectTime),
            input$HisIV
          ))
        )
      )
    }
    else{
      return()
    }
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
    my_his_data <-
      my_data()[, c(input$HisDV, input$HisIV, input$Plotfacet_choice)]
    #groupIV<-input$HisIV
    
    if (input$plot_facet == T) {
      facetIV <- input$Plotfacet_choice
      my_his_data$facetIV <- my_his_data[, input$Plotfacet_choice]
      #groupIV<-setdiff(groupIV, facetIV)}
      
      #plotDV<-input$HisDV
      #my_his_data$plotDV<-my_his_data[,input$HisDV]
      
      #my_his_data$groupID<-do.call(paste, c(my_his_data[groupIV], sep="_"))
      
      ### These hashed out lines are trying to work in groupID (equivalent to Magda's id_test) which allows grouping by multiple IVs
      ### Should eventually replace lines 1219 - 1230
      
      #      if (input$HistType == "HistCount") {
      #         fit <- ggplot(my_his_data, aes(x=groupID, y=plotDV)) + xlab(names(my_his_data$groupID)) + geom_histogram(size=0.6, alpha=0.3, col="black")
      #      }
      #      if (input$HistType == "HistDensity" ) {
      #        fit <- ggplot(my_his_data, aes(x=groupID, y=plotDV)) + xlab(names(my_his_data$groupID)) + geom_density(alpha = 0.3)
      #      }
      
      if (input$HistType == "HistCount") {
        fit <-
          ggplot(my_his_data, aes(x = my_his_data[, 1], fill = my_his_data[, 2])) + xlab(names(my_his_data[1])) + geom_histogram(size =
                                                                                                                                   0.6,
                                                                                                                                 alpha = 0.3,
                                                                                                                                 col = "black") + labs(fill = names(my_his_data[2]))
        fit <- fit + facet_wrap( ~ facetIV)
      }
      if (input$HistType == "HistDensity") {
        fit <-
          ggplot(my_his_data, aes(x = my_his_data[, 1], fill = my_his_data[, 2])) + xlab(names(my_his_data[1]))  + geom_density(alpha = 0.3) + labs(fill =
                                                                                                                                                      names(my_his_data[2]))
        fit <- fit + facet_wrap( ~ facetIV)
      }
    }
    
    
    if (input$plot_facet == F) {
      if (input$HistType == "HistCount") {
        fit <-
          ggplot(my_his_data, aes(x = my_his_data[, 1], fill = my_his_data[, 2])) + xlab(names(my_his_data[1])) + geom_histogram(size =
                                                                                                                                   0.6,
                                                                                                                                 alpha = 0.3,
                                                                                                                                 col = "black") + labs(fill = names(my_his_data[2]))
      }
      if (input$HistType == "HistDensity") {
        fit <-
          ggplot(my_his_data, aes(x = my_his_data[, 1], fill = my_his_data[, 2])) + xlab(names(my_his_data[1]))  + geom_density(alpha = 0.3) + labs(fill =
                                                                                                                                                      names(my_his_data[2]))
      }
    }
    ggplotly(fit)
    
  })
  
  ##STILL TO DO:
  #       try to do subset by multiple variables
  output$Boxes <- renderPlotly({
    my_his_data <-
      my_data()[, c(input$HisDV, input$HisIV, input$Plotfacet_choice)]
    #groupIV<-input$HisIV
    
    if (input$plot_facet == T) {
      facetIV <- input$Plotfacet_choice
      my_his_data$facetIV <- my_his_data[, input$Plotfacet_choice]
      
      box_graph <-
        ggplot(my_his_data, aes(x = my_his_data[, 2], y = my_his_data[, 1])) + xlab(names(my_his_data[2])) + ylab(names(my_his_data[1])) + geom_boxplot()
      box_graph <- box_graph + facet_wrap( ~ facetIV)
    }
    else{
      box_graph <-
        ggplot(my_his_data, aes(x = my_his_data[, 2], y = my_his_data[, 1])) + xlab(names(my_his_data[2])) + ylab(names(my_his_data[1])) + geom_boxplot()
      
    }
    ggplotly(box_graph)
  })
  
  ###ANOVA summary table output
  output$ANOVAtest <- renderPrint({
    my_his_data <-
      my_data()[, c(input$HisDV, input$HisIV, input$Plotfacet_choice)]
    my_his_data[, 2] <- as.factor(my_his_data[, 2])
    if (input$plot_facet == T) {
      n_rows <- length(levels(my_his_data[, 3]))
      facetting <- rep(NA, n_rows)
      p_values <- rep(NA, n_rows)
      for (i in unique(my_his_data[, 3])) {
        subsetted_data <- subset(my_his_data, my_his_data[, 3] == i)
        facetting[i] <- i
        fit_anova <-
          aov(subsetted_data[, 1] ~ subsetted_data[, 2], data = subsetted_data)
        #print(fit_anova)
        #print(summary(fit_anova))
        p_values[i] <-
          summary(fit_anova)[[1]][[1, "Pr(>F)"]] #summary of anova is a list, so we need to access the 1st element which is the results and then in 1st row column Pr>F you have the p-value
        #print(paste("The p-value of the ANOVA test is", pvalue))
        temp_anova <- as.data.frame(cbind(facetting, p_values))
      }
      temp_anova <- na.omit(temp_anova)
      colnames(temp_anova) <- c("Facetting variable", "p_value")
      print(temp_anova, row.names = FALSE)
    }
    if (input$plot_facet == F) {
      fit_anova <-
        aov(my_his_data[, 1] ~ as.factor(my_his_data[, 2]), data = my_his_data)
      #print(fit_anova)
      #br()
      #print(summary(fit_anova))
      pvalue_ANOVA <- summary(fit_anova)[[1]][[1, "Pr(>F)"]]
      print(paste("The p-value of the ANOVA test is", pvalue_ANOVA))
    }
  })
  
  ##Bartlett test
  
  output$Bartlett <- renderPrint({
    my_his_data <-
      my_data()[, c(input$HisDV, input$HisIV, input$Plotfacet_choice)]
    my_his_data[, 2] <- as.factor(my_his_data[, 2])
    if (input$plot_facet == T) {
      n_rows <- length(levels(my_his_data[, 3]))
      facetting <- rep(NA, n_rows)
      pvalue_bartlett <- rep(NA, n_rows)
      for (i in unique(my_his_data[, 3])) {
        facetting[i] <- i
        subsetted_data <- subset(my_his_data, my_his_data[, 3] == i)
        fit_bartlett <-
          bartlett.test(subsetted_data[, 1] ~ subsetted_data[, 2], data = subsetted_data)
        #print(fit_bartlett)
        #model_bartlett<-fit_bartlett[[4]] #result of bartlett is a list with 4th element the description of model
        pvalue_bartlett[i] <-
          fit_bartlett[[3]] #result of bartlett is a list with 3rd element the p-value
        temp_bartlett <-
          as.data.frame(cbind(facetting, pvalue_bartlett))
      }
      temp_bartlett <- na.omit(temp_bartlett)
      colnames(temp_bartlett) <- c("Facetting variable", "p_value")
      print(temp_bartlett, row.names = FALSE)
    }
    
    if (input$plot_facet == F) {
      fit_bartlett <-
        bartlett.test(my_his_data[, 1] ~ my_his_data[, 2], data = my_his_data)
      #print(fit_bartlett)
      #model_bartlett<-fit_bartlett[[4]] #result of bartlett is a list with 4th element the description of model
      pvalue_bartlett <-
        fit_bartlett[[3]] #result of bartlett is a list with 3rd element the p-value
      print(
        paste(
          "The p-value of the Bartlett test of homogeneity of variances is",
          pvalue_bartlett
        )
      )
    }
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # - - - - - - - - - - - - >> DATA CORRELATION IN 6th TAB << - - - - - - - - - - -
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  ######################### correlation plot#############################
  
  # select data sets to be used
  output$cor_Pheno_data <- renderUI({
    if (is.null(ItemList())) {
      return()
    }
    else
      tagList(
        selectizeInput(
          inputId = "cor_data",
          label = "Select the dataset that you would like to use for correlation",
          choices = c("raw data", "NA removed", "outliers removed"),
          multiple = F
        )
      )
  })
  
  cor_data_type <- eventReactive(input$cor_data, {
    if (input$cor_data == "raw data") {
      cor_data_type <- my_data()
    }
    if (input$cor_data == "NA removed") {
      cor_data_type <- my_data_nona()
    }
    if (input$cor_data == "outliers removed") {
      cor_data_type <- Outlier_free_data()
    }
    cor_data_type
  })
  
  
  # whether to use subsetted data or not
  output$cor_subset <- renderUI({
    if (input$cor_data_subset == F) {
      return()
    }
    else{
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
    }
  })
  
  output$CorSpecIV_val <- renderUI({
    if ((input$cor_data_subset == F)) {
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
  
  output$corrplot <- renderPlot({
    df <- cor_data_type()
    
    if (input$cor_data_subset) {
      df <- df[df[input$CorIV_sub] == input$CorIV_val, ]
    }
    
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
    
    
    corrplot(
      cor(df[, beginCol:endCol], method = input$corMethod),
      method = input$corrplotMethod,
      type = input$corType,
      order = input$corOrder,
      tl.col = 'black'
    )
  })
  
  ################ cor_table output###################
  
  my_table <- eventReactive(input$Go_table, {
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
    
    df <- cor_data_type()
    if (input$cor_data_subset) {
      df <- df[df[input$CorIV_sub] == input$CorIV_val, ]
    }
    
    res <-
      rcorr(as.matrix(df[, beginCol:endCol]), type = input$corMethod)
    
    flattenCorrMatrix <- function(cormat, pmat) {
      ut <- upper.tri(cormat)
      data.frame(
        row = rownames(cormat)[row(cormat)[ut]],
        column = rownames(cormat)[col(cormat)[ut]],
        cor  = (cormat)[ut],
        p = pmat[ut]
      )
    }
    
    result <- flattenCorrMatrix(res$r, res$P)
    return(result)
  })
  
  output$cor_table <- renderDataTable({
    my_table()
  })
  
  ############ interactive scatter plot ##########
  
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
  
  output$scatterplot <- renderPlotly({
    my_data <- data.frame(my_data())
    my_data %>% ggplot(aes_string(input$Pheno1, input$Pheno2)) + geom_point(aes_string(colour =
                                                                                         input$Color))
    ggplotly()
  })
  
  # r2 and p-value ----------------------------------------------------------
  
  output$corrsq <- renderText({
    cor_data <- my_data()[, c(input$Pheno1, input$Pheno2)]
    correl <- lm(cor_data[, 1] ~ cor_data[, 2])
    r2 <- summary(correl)$r.squared
    paste("The R square value of the linear regression is", signif(r2, 3))
  })
  
  
  output$corpval <- renderText({
    cor_data <- my_data()[, c(input$Pheno1, input$Pheno2)]
    correl <- lm(cor_data[, 1] ~ cor_data[, 2])
    pval <- summary(correl)$coefficients[8]
    paste("The p-value is", signif(pval, 3))
  })
  
  # make downloadButton for corplot -----------------------------------------
  
  
  output$downloadCorrplot <- downloadHandler(
    filename = function() {
      paste0('corrplot-', Sys.Date(), '.png')
    },
    content = function(con) {
      df <- cor_data_type()
      
      if (input$cor_data_subset) {
        df <- df[df[input$CorIV_sub] == input$CorIV_val, ]
      }
      
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
      
      png(con, width = 800, height = 800)
      
      corrplot(
        cor(df[, beginCol:endCol], method = input$corMethod),
        method = input$corrplotMethod,
        type = input$corType,
        order = input$corOrder,
        tl.col = 'black'
      )
      
      dev.off()
    }
  )
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # - - - - - - - - - - - - - - >> PCA IN 7th TAB <<- - - - - - - - - - - - - - - -
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  output$PCA_Pheno_data <- renderUI({
    if (is.null(ItemList())) {
      return()
    }
    else
      tagList(
        selectizeInput(
          inputId = "PCA_data",
          label = "Select the dataset that you would like to use for PCA",
          choices = c("raw data", "NA removed", "outliers removed"),
          multiple = F
        )
      )
  })
  
  PCA_data_type <- eventReactive(input$Go_PCAdata, {
    if (input$PCA_data == "raw data") {
      PCA_data_type <- my_data()
    }
    if (input$PCA_data == "NA removed") {
      PCA_data_type <- my_data_nona()
    }
    if (input$PCA_data == "outliers removed") {
      PCA_data_type <- Outlier_free_data()
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
      tagList(
        selectizeInput(
          inputId = "PCA_pheno",
          label = "Select the phenotypes would you like to use for the PCA",
          choices = c(input$SelectDV),
          multiple = T
        )
      )
  })
  output$PCA_subset_trait <- renderUI({
    if (input$PCA_data_subset == F) {
      return()
    }
    else{
      tagList(
        selectizeInput(
          inputId = "PCA_subset_T",
          label = "Select Indepdentend Variables for which you would like to subset",
          choices = c(input$SelectGeno, input$SelectIV, input$SelectTime),
          multiple = T
        )
      )
    }
  })
  
  lista_PCA <- eventReactive(input$PCA_subset_T, {
    subset_lista <- input$PCA_subset_T
    id_lista <-
      c(input$SelectGeno, input$SelectIV, input$SelectTime)
    id_lista2 <- setdiff(id_lista, subset_lista)
    temp <- PCA_data_type()
    temp$subset_id <-
      do.call(paste, c(temp[c(subset_lista)], sep = "_"))
    the_list <- unique(temp$subset_id)
    the_list
  })
  
  output$PCA_subset_specific <- renderUI({
    if (is.null(input$PCA_subset_T)) {
      return()
    }
    else{
      subset_lista <- input$PCA_subset_T
      id_lista <-
        c(input$SelectGeno, input$SelectIV, input$SelectTime)
      id_lista2 <- setdiff(id_lista, subset_lista)
      temp <- PCA_data_type()
      temp$subset_id <-
        do.call(paste, c(temp[c(subset_lista)], sep = "_"))
      the_list <- unique(temp$subset_id)
      
      tagList(
        selectizeInput(
          inputId = "PCA_subset_S",
          label = "Select specific subset for which you would like to subset",
          choices = c(the_list),
          multiple = F
        )
      )
    }
  })
  
  PCA_final_data <- eventReactive(input$Go_PCA, {
    temp <- data.frame(PCA_data_type())
    temp <-
      subset(
        temp,
        select = c(
          input$SelectGeno,
          input$SelectIV,
          input$SelectTime,
          input$SelectID,
          input$PCA_pheno
        )
      )
    
    if (input$PCA_data_subset == T) {
      subset_lista <- input$PCA_subset_T
      if (input$PCA_data_avg == F) {
        id_lista <-
          c(input$SelectGeno,
            input$SelectIV,
            input$SelectID,
            input$SelectTime)
        id_lista2 <- setdiff(id_lista, subset_lista)
        temp$id <- do.call(paste, c(temp[c(id_lista2)], sep = "_"))
        temp$sub_id <-
          do.call(paste, c(temp[c(subset_lista)], sep = "_"))
        temp2 <- subset(temp, temp$sub_id == input$PCA_subset_S)
        temp2 <- subset(temp2, select = c("id", input$PCA_pheno))
      }
      if (input$PCA_data_avg == T) {
        id_lista <- c(input$SelectGeno, input$SelectIV, input$SelectTime)
        id_lista2 <- setdiff(id_lista, subset_lista)
        temp$id <- do.call(paste, c(temp[c(id_lista2)], sep = "_"))
        temp$sub_id <-
          do.call(paste, c(temp[c(subset_lista)], sep = "_"))
        temp <- subset(temp, temp$sub_id == input$PCA_subset_S)
        temp <- subset(temp, select = c("id", input$PCA_pheno))
        temp2 <- summaryBy(. ~ id, data = temp)
        # Add remove .mean from column names
      }
    }
    if (input$PCA_data_subset == F) {
      if (input$PCA_data_avg == F) {
        temp$id <-
          do.call(paste, c(temp[c(input$SelectGeno,
                                  input$SelectIV,
                                  input$SelectTime,
                                  input$SelectID)], sep = "_"))
        temp2 <- subset(temp, select = c("id", input$PCA_pheno))
      }
      if (input$PCA_data_avg == T) {
        temp <-
          subset(
            temp,
            select = c(
              input$SelectGeno,
              input$SelectIV,
              input$SelectTime,
              input$SelectID,
              input$PCA_pheno
            )
          )
        temp$id <-
          do.call(paste, c(temp[c(input$SelectGeno, input$SelectIV, input$SelectTime)], sep =
                             "_"))
        temp <- subset(temp, select = c("id", input$PCA_pheno))
        temp2 <- summaryBy(. ~ id, data = temp)
      }
    }
    
    return(temp2)
  })
  
  output$PCA_final_table <- renderDataTable({
    PCA_final_data()
  })
  
  PCA_eigen_data <- eventReactive(input$Go_PCA, {
    beginCol <- 2
    endCol <- ncol(PCA_final_data())
    PCA_ready <- PCA_final_data()
    PCA_ready <- PCA_ready[, beginCol:endCol]
    res.pca <- PCA(PCA_ready, graph = FALSE)
    eigenvalues <- res.pca$eig
    eigenvalues
  })
  
  output$PCA_eigen_plot <- renderPlotly({
    eigenvalues <- PCA_eigen_data()
    Y = (eigenvalues[, 2])
    X = (names.arg = 1:nrow(eigenvalues))
    bar <-
      ggplot(eigenvalues, aes(x = X, y = Y)) + geom_bar(stat = "identity") +
      xlab("Principal Components") + ylab("Percentage of variances") +
      ggtitle("Variances")
    ggplotly(bar)
  })
  
  output$PCA1_select <- renderUI({
    if ((input$Go_PCAdata == FALSE)) {
      return()
    } else
      eigenvalues <- PCA_eigen_data()
    list_avail_PCs <- unique(1:(nrow(eigenvalues) - 2))
    tagList(
      selectizeInput(
        inputId = "Which_PC1",
        label = "Select which PCs you would like to plot on x-axis",
        choices = list_avail_PCs,
        multiple = F
      )
    )
  })
  
  output$PCA2_select <- renderUI({
    if ((input$Go_PCAdata == FALSE)) {
      return()
    } else
      eigenvalues <- PCA_eigen_data()
    list_avail_PCs <- unique(1:(nrow(eigenvalues) - 2))
    tagList(
      selectizeInput(
        inputId = "Which_PC2",
        label = "Select which PCs you would like to plot on y-axis",
        choices = list_avail_PCs,
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
    endCol <- ncol(PCA_final_data())
    PCA_ready <- PCA_final_data()
    PCA_ready <- PCA_ready[, beginCol:endCol]
    res.pca <- PCA(PCA_ready, graph = FALSE)
    mid = median(res.pca$var$contrib)
    fviz_pca_var(
      res.pca,
      axes = c(
        as.numeric(input$Which_PC1),
        as.numeric(input$Which_PC2)
      ),
      col.var = "contrib",
      geom = "auto",
      labelsize = 4,
      repel = T,
      label = "var",
      addlabels = T,
      invisible = "none"
    ) +
      scale_color_gradient2(
        low = "grey",
        mid = "purple",
        high = "red",
        midpoint = mid
      ) + theme_bw()
  })
  
  output$PCA_scatter_plot <- renderPlotly({
    beginCol <-
      length(c(
        input$SelectIV,
        input$SelectGeno,
        input$SelectTime,
        input$SelectID
      )) + 1
    endCol <- ncol(PCA_final_data())
    PCA_ready <- PCA_final_data()
    PCA_ready <- PCA_ready[, beginCol:endCol]
    res.pca <- PCA(PCA_ready, graph = FALSE)
    mid1 = median(res.pca$var$cos2)
    fviz_pca_ind(
      res.pca,
      axes = c(
        as.numeric(input$Which_PC1),
        as.numeric(input$Which_PC2)
      ),
      col.ind = "cos2",
      repel = T
    ) +
      scale_color_gradient2(
        low = "grey",
        mid = "purple",
        high = "red",
        midpoint = mid1
      ) +
      theme_minimal()
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # - - - - - - - - - - - - >> CLUSTER ANALYSIS IN 8th TAB << - - - - - - - - - - -
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # =  = = = = = = = = >> INPUT GADGETS << = = = = = = = = = = = = =
  output$Select_data_cluster <- renderUI({
    if (is.null(ItemList())) {
      return()
    }
    else
      tagList(
        selectizeInput(
          inputId = "Cluster_data",
          label = "Select the dataset that you would like to use for clustering analysis",
          choices = c(
            "raw data",
            "NA removed",
            "outliers removed",
            "Summary Stats data"
          ),
          multiple = F
        )
      )
  })
  
  output$Select_cluster_method <- renderUI({
    if (is.null(ItemList())) {
      return()
    }
    else
      tagList(
        selectizeInput(
          inputId = "Cluster_cor_method",
          label = "Select the correlation method for clustering",
          choices = c("pearson", "kendall", "spearman"),
          multiple = F
        )
      )
  })
  
  
  output$Select_clustering_method <- renderUI({
    if (is.null(ItemList())) {
      return()
    }
    else
      tagList(
        selectizeInput(
          inputId = "Cluster_method",
          label = "Select the method for calculating clusters",
          choices = c(
            "ward.D",
            "ward.D2",
            "single",
            "complete",
            "average",
            "mcquitty",
            "median",
            "centroid"
          ),
          multiple = F
        )
      )
  })
  
  output$Select_data_cluster_validation <- renderUI({
    if (is.null(input$Split_cluster)) {
      return()
    }
    else{
      tagList(
        selectizeInput(
          "Clust_test",
          label = "Which phenotypes would you like to test for significant differences between clusters",
          choices = input$SelectDV
        )
      )
    }
  })
  
  Data_for_cluster <- eventReactive(exists(input$Cluster_data), {
    if (input$Cluster_data == "raw data") {
      cluster_data <- my_data()
    }
    if (input$Cluster_data == "NA removed") {
      cluster_data <- my_data_nona()
    }
    if (input$Cluster_data == "outliers removed") {
      cluster_data <- Outlier_free_data()
    }
    return(cluster_data)
  })
  
  output$Data_cluster_table <- renderDataTable({
    Data_for_cluster()
  })
  
  output$Select_phenotypes_cluster <- renderUI({
    if (is.null(Data_for_cluster())) {
      return()
    }
    else
    {
      tagList(
        selectizeInput(
          inputId = "Cluster_pheno",
          label = "Select the phenotypes you would like to use for clustering analysis",
          choices = c(input$SelectDV),
          multiple = T
        )
      )
    }
  })
  
  output$Cluster_subset_trait <- renderUI({
    if (input$Cluster_subset_Q == F) {
      return()
    }
    else{
      tagList(
        selectizeInput(
          inputId = "Cluster_subset_T",
          label = "Select Indepdentend Variables for which you would like to subset",
          choices = c(input$SelectGeno, input$SelectIV, input$SelectTime),
          multiple = T
        )
      )
    }
  })
  
  lista_cluster <- eventReactive(input$Cluster_subset_t, {
    subset_lista <- input$Cluster_subset_T
    id_lista <-
      c(input$SelectGeno, input$SelectIV, input$SelectTime)
    id_lista2 <- setdiff(id_lista, subset_lista)
    temp <- Data_for_cluster()
    temp$subset_id <-
      do.call(paste, c(temp[c(subset_lista)], sep = "_"))
    the_list <- unique(temp$subset_id)
    the_list
  })
  
  output$Cluster_subset_specific <- renderUI({
    if (is.null(input$Cluster_subset_T)) {
      return()
    }
    else{
      subset_lista <- input$Cluster_subset_T
      id_lista <-
        c(input$SelectGeno, input$SelectIV, input$SelectTime)
      id_lista2 <- setdiff(id_lista, subset_lista)
      temp <- Data_for_cluster()
      temp$subset_id <-
        do.call(paste, c(temp[c(subset_lista)], sep = "_"))
      the_list <- unique(temp$subset_id)
      
      tagList(
        selectizeInput(
          inputId = "Cluster_subset_S",
          label = "Select specific subset for which you would like to subset",
          choices = c(the_list),
          multiple = F
        )
      )
    }
  })
  
  # = = = = = = = = = = = >> MAIN CALCULATIONS AND TABLES << = = = = = = = = = = = = = = =
  
  Final_data_cluster <- eventReactive(input$Go_cluster, {
    temp <- Data_for_cluster()
    if (input$Cluster_subset_Q == T) {
      subset_lista <- input$Cluster_subset_T
      if (input$Cluster_pre_calc == F) {
        id_lista <-
          c(input$SelectGeno,
            input$SelectIV,
            input$SelectTime,
            input$SelectID)
        id_lista2 <- setdiff(id_lista, subset_lista)
        temp$id <- do.call(paste, c(temp[c(id_lista2)], sep = "_"))
        temp$sub_id <-
          do.call(paste, c(temp[c(subset_lista)], sep = "_"))
        temp2 <- subset(temp, temp$sub_id == input$Cluster_subset_S)
        temp2 <-
          subset(temp2, select = c("id", input$Cluster_pheno))
      }
      if (input$Cluster_pre_calc == T) {
        id_lista <- c(input$SelectGeno, input$SelectIV, input$SelectTime)
        id_lista2 <- setdiff(id_lista, subset_lista)
        temp$id <- do.call(paste, c(temp[c(id_lista2)], sep = "_"))
        temp$sub_id <-
          do.call(paste, c(temp[c(subset_lista)], sep = "_"))
        temp <- subset(temp, temp$sub_id == input$Cluster_subset_S)
        temp2 <- subset(temp, select = c("id", input$Cluster_pheno))
        temp2 <- summaryBy(. ~ id, data = temp2)
      }
    }
    if (input$Cluster_subset_Q == F) {
      if (input$Cluster_pre_calc == F) {
        temp$id <-
          do.call(paste, c(temp[c(input$SelectGeno,
                                  input$SelectIV,
                                  input$SelectTime,
                                  input$SelectID)], sep = "_"))
        temp2 <- subset(temp, select = c("id", input$Cluster_pheno))
      }
      if (input$Cluster_pre_calc == T) {
        temp$id <-
          do.call(paste, c(temp[c(input$SelectGeno, input$SelectIV, input$SelectTime)], sep =
                             "_"))
        temp2 <- subset(temp, select = c("id", input$Cluster_pheno))
        temp2 <- summaryBy(. ~ id, data = temp2)
      }
    }
    return(temp2)
  })
  
  output$Final_cluster_table <- renderDataTable({
    Final_data_cluster()
  })
  
  # = = = = = = = = = >> OUTPUT PLOTS AND SENTENCES << = = = = = = = = = = = = = =
  
  output$ClusterTree <- renderPlot({
    clust_temp <- Final_data_cluster()
    clust_temp <- na.omit(clust_temp)
    clust_matrix <- clust_temp[, 2:ncol(clust_temp)]
    row.names(clust_matrix) <- clust_temp$id
    clust_matrix = as.matrix(clust_matrix)
    clust_t_matrix = t(clust_matrix)
    clust_t_cor = cor(clust_t_matrix, method = input$Cluster_cor_method)
    clust_t_dist = dist(clust_t_cor)
    clust_t_clust = hclust(clust_t_dist, method = input$Cluster_method)
    
    plot(as.dendrogram(clust_t_clust), horiz = T)
    
  })
  
  output$HotHeatMap <- renderPlot({
    clust_temp <- Final_data_cluster()
    clust_temp <- na.omit(clust_temp)
    clust_matrix <- clust_temp[, 2:ncol(clust_temp)]
    row.names(clust_matrix) <- clust_temp$id
    clust_matrix = as.matrix(clust_matrix)
    clust_t_matrix = t(clust_matrix)
    clust_t_cor = cor(clust_t_matrix, method = input$Cluster_cor_method)
    clust_t_dist = dist(clust_t_cor)
    clust_t_clust = hclust(clust_t_dist, method = input$Cluster_method)
    heatmap.2(
      clust_t_matrix,
      Colv = as.dendrogram(clust_t_clust),
      col = blue2red(100),
      scale = c("row"),
      density.info = "none",
      trace = "none",
      cexRow = 0.7
    )
  })
  
  output$Dendro_sentence <- renderText({
    if (is.null(input$Split_cluster)) {
      return()
    }
    else{
      clust_temp <- Final_data_cluster()
      clust_temp <- na.omit(clust_temp)
      clust_matrix <- clust_temp[, 2:ncol(clust_temp)]
      row.names(clust_matrix) <- clust_temp$id
      clust_matrix = as.matrix(clust_matrix)
      clust_t_matrix = t(clust_matrix)
      clust_t_cor = cor(clust_t_matrix, method = input$Cluster_cor_method)
      clust_t_dist = dist(clust_t_cor)
      clust_t_clust = hclust(clust_t_dist, method = input$Cluster_method)
      cluster <-
        as.data.frame(cutree(clust_t_clust, h = as.numeric(input$Split_cluster)))
      names(cluster)[1] <- "cluster"
      clust_number <- length(unique(cluster$cluster))
      
      sentence <-
        paste(
          "Cutting the dengrodram at ",
          input$Split_cluster,
          " will result in ",
          clust_number,
          " clusters. Please be aware that clustering your data into too many clusters might not be informative."
        )
      return(sentence)
    }
  })
  
  output$HotAnovaNews <- renderText({
    clust_temp <- Final_data_cluster()
    clust_temp <- na.omit(clust_temp)
    clust_matrix <- clust_temp[, 2:ncol(clust_temp)]
    row.names(clust_matrix) <- clust_temp$id
    clust_matrix = as.matrix(clust_matrix)
    clust_t_matrix = t(clust_matrix)
    clust_t_cor = cor(clust_t_matrix, method = input$Cluster_cor_method)
    clust_t_dist = dist(clust_t_cor)
    clust_t_clust = hclust(clust_t_dist, method = input$Cluster_method)
    
    # cut_tree at $tree_cut value (but first make it numeric)
    
    cluster <-
      as.data.frame(cutree(clust_t_clust, h = as.numeric(input$Split_cluster)))
    names(cluster)[1] <- "cluster"
    clust_number <- length(unique(cluster$cluster))
    
    temp <- Data_for_cluster()
    if (input$Cluster_pre_calc == F) {
      temp$id <-
        do.call(paste, c(temp[c(input$SelectGeno,
                                input$SelectIV,
                                input$SelectTime,
                                input$SelectID)], sep = "_"))
      temp2 <- subset(temp, select = c("id", input$SelectDV))
    }
    if (input$Cluster_pre_calc == T) {
      temp$id <-
        do.call(paste, c(temp[c(input$SelectGeno, input$SelectIV, input$SelectTime)], sep =
                           "_"))
      temp2 <- subset(temp, select = c("id", input$SelectDV))
      temp2 <- summaryBy(. ~ id, data = temp2)
      colnames(temp2) = gsub(pattern = ".mean",
                             replacement = "",
                             x = colnames(temp2))
    }
    
    row.names(temp2) <- temp2$id
    
    new_shait <- merge(cluster, temp2, by = "row.names")
    
    to_test <- new_shait[, c("id", "cluster", input$SelectDV[1])]
    names(to_test)[3] <- "phenotype"
    to_test$cluster <- as.factor(to_test$cluster)
    amod <- aov(phenotype ~ cluster, data = to_test)
    
    if (summary(amod)[[1]][["Pr(>F)"]] < 0.05) {
      sig_listxxx <- input$SelectDV[1]
    }
    
    
    for (i in 2:length(input$SelectDV)) {
      to_test <- new_shait[, c("id", "cluster", input$SelectDV[i])]
      names(to_test)[3] <- "phenotype"
      to_test$cluster <- as.factor(to_test$cluster)
      amod <- aov(phenotype ~ cluster, data = to_test)
      
      if (summary(amod)[[1]][["Pr(>F)"]] < 0.05) {
        significantna_lista <- input$SelectDV[i]
      }
      sig_listxxx <- c(sig_listxxx, significantna_lista)
    }
    
    lista_cudow <- unique(sig_listxxx)
    #sentence <- paste("significant effect of clustering was observed for ", lista_cudow, <font color=\"#FF0000\"><b>)
    paste("<font color=\"#008080\"><b>", lista_cudow, "</b></font>")
    
    
  })
  
  
  output$HotANOVA <- renderPlot({
    clust_temp <- Final_data_cluster()
    clust_temp <- na.omit(clust_temp)
    clust_matrix <- clust_temp[, 2:ncol(clust_temp)]
    row.names(clust_matrix) <- clust_temp$id
    clust_matrix = as.matrix(clust_matrix)
    clust_t_matrix = t(clust_matrix)
    clust_t_cor = cor(clust_t_matrix, method = input$Cluster_cor_method)
    clust_t_dist = dist(clust_t_cor)
    clust_t_clust = hclust(clust_t_dist, method = input$Cluster_method)
    
    # cut_tree at $tree_cut value (but first make it numeric)
    
    cluster <-
      as.data.frame(cutree(clust_t_clust, h = as.numeric(input$Split_cluster)))
    names(cluster)[1] <- "cluster"
    
    temp <- Data_for_cluster()
    if (input$Cluster_pre_calc == F) {
      temp$id <-
        do.call(paste, c(temp[c(input$SelectGeno,
                                input$SelectIV,
                                input$SelectTime,
                                input$SelectID)], sep = "_"))
      temp2 <- subset(temp, select = c("id", input$SelectDV))
    }
    if (input$Cluster_pre_calc == T) {
      temp$id <-
        do.call(paste, c(temp[c(input$SelectGeno, input$SelectIV, input$SelectTime)], sep =
                           "_"))
      temp2 <- subset(temp, select = c("id", input$SelectDV))
      temp2 <- summaryBy(. ~ id, data = temp2)
      colnames(temp2) = gsub(pattern = ".mean",
                             replacement = "",
                             x = colnames(temp2))
    }
    
    row.names(temp2) <- temp2$id
    new_shait <- merge(cluster, temp2, by = "row.names")
    to_test <- new_shait[, c("id", "cluster", input$Clust_test)]
    names(to_test)[3] <- "phenotype"
    to_test$cluster <- as.factor(to_test$cluster)
    amod <- aov(phenotype ~ cluster, data = to_test)
    tuk <- glht(amod, linfct = mcp(cluster = "Tukey"))
    tuk.cld <- cld(tuk)
    old.par <- par(mai = c(1, 1, 1.25, 1))
    shaka_laka <-
      plot(tuk.cld,
           las = 1,
           col = "#dd1c77",
           ylab = input$Clust_test)
    shaka_laka
  })
  
  
  
  output$Cluster_table <- renderDataTable({
    clust_temp <- Final_data_cluster()
    clust_temp <- na.omit(clust_temp)
    clust_matrix <- clust_temp[, 2:ncol(clust_temp)]
    row.names(clust_matrix) <- clust_temp$id
    clust_matrix = as.matrix(clust_matrix)
    clust_t_matrix = t(clust_matrix)
    clust_t_cor = cor(clust_t_matrix, method = input$Cluster_cor_method)
    clust_t_dist = dist(clust_t_cor)
    clust_t_clust = hclust(clust_t_dist, method = input$Cluster_method)
    
    # cut_tree at $tree_cut value (but first make it numeric)
    
    cluster <-
      as.data.frame(cutree(clust_t_clust, h = as.numeric(input$Split_cluster)))
    names(cluster)[1] <- "cluster"
    clust_number <- length(unique(cluster$cluster))
    
    temp <- Data_for_cluster()
    if (input$Cluster_pre_calc == F) {
      temp$id <-
        do.call(paste, c(temp[c(input$SelectGeno,
                                input$SelectIV,
                                input$SelectTime,
                                input$SelectID)], sep = "_"))
      temp2 <- subset(temp, select = c("id", input$SelectDV))
    }
    if (input$Cluster_pre_calc == T) {
      temp$id <-
        do.call(paste, c(temp[c(input$SelectGeno, input$SelectIV, input$SelectTime)], sep =
                           "_"))
      temp2 <- subset(temp, select = c("id", input$SelectDV))
      temp2 <- summaryBy(. ~ id, data = temp2)
      colnames(temp2) = gsub(pattern = ".mean",
                             replacement = "",
                             x = colnames(temp2))
    }
    
    row.names(temp2) <- temp2$id
    
    new_shait <- merge(cluster, temp2, by = "row.names")
    return(new_shait)
  })
  
  output$Cluster_download_button <- renderUI({
    if (is.null(input$Split_cluster)) {
      return()
    }
    else
      downloadButton("data_clustered", label = "Download Cluster data")
  })
  
  output$data_clustered <- downloadHandler(
    filename = paste(
      "Cluster analysis_based_on_",
      input$Cluster_pheno,
      "_with_split_at_",
      input$Split_cluster,
      "_MVApp.csv"
    ),
    content <- function(file) {
      clust_temp <- Final_data_cluster()
      clust_temp <- na.omit(clust_temp)
      clust_matrix <- clust_temp[, 2:ncol(clust_temp)]
      row.names(clust_matrix) <- clust_temp$id
      clust_matrix = as.matrix(clust_matrix)
      clust_t_matrix = t(clust_matrix)
      clust_t_cor = cor(clust_t_matrix, method = input$Cluster_cor_method)
      clust_t_dist = dist(clust_t_cor)
      clust_t_clust = hclust(clust_t_dist, method = input$Cluster_method)
      
      # cut_tree at $tree_cut value (but first make it numeric)
      
      cluster <-
        as.data.frame(cutree(clust_t_clust, h = as.numeric(input$Split_cluster)))
      names(cluster)[1] <- "cluster"
      clust_number <- length(unique(cluster$cluster))
      
      temp <- Data_for_cluster()
      if (input$Cluster_pre_calc == F) {
        temp$id <-
          do.call(paste, c(temp[c(input$SelectGeno,
                                  input$SelectIV,
                                  input$SelectTime,
                                  input$SelectID)], sep = "_"))
        temp2 <- subset(temp, select = c("id", input$SelectDV))
      }
      if (input$Cluster_pre_calc == T) {
        temp$id <-
          do.call(paste, c(temp[c(input$SelectGeno, input$SelectIV, input$SelectTime)], sep =
                             "_"))
        temp2 <- subset(temp, select = c("id", input$SelectDV))
        temp2 <- summaryBy(. ~ id, data = temp2)
        colnames(temp2) = gsub(pattern = ".mean",
                               replacement = "",
                               x = colnames(temp2))
      }
      
      row.names(temp2) <- temp2$id
      
      new_shait <- merge(cluster, temp2, by = "row.names")
      
      write.csv(new_shait, file)
    }
  )
  
  # end of the script
}