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
          label = "Select column containing Genotype information",
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
          label = "Select column(s) containing Independent Variables (such as Treatment)",
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
          label = "Select columns containing Dependent Variables (Phenotypes)",
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
          label = "Select column containing sample ID",
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
          label = "Select column containing Time / gradient information (MUST be numeric)",
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
    if(is.null(input$your_data)) {
      return(NULL)}
    else{
     sraka <- read.csv(input$your_data$datapath)
     sraka_nona <- sraka[complete.cases(sraka),]
     na_numbers <- nrow(sraka) - nrow(sraka_nona)
     sentence <- paste("Your uploaded data contains ", dim(sraka)[1], " rows and ", dim(sraka)[2], " columns. There are ", na_numbers, " rows containins missing values.")
     return(sentence)
    }
  })
  
  output$selected_data_report <- renderText({
    if(is.null(my_data())) {
      return(NULL)}
    else{
      sraka <- my_data()
      sraka_nona <- sraka[complete.cases(sraka),]
      na_numbers <- nrow(sraka) - nrow(sraka_nona)
      sentence <- paste("Your selected data contains ", dim(sraka)[1], " rows and ", dim(sraka)[2], " columns. There are ", na_numbers, " rows containins missing values.")
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
          label = "Phenotype used for modeling",
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
          label = "Independent Variable to split the samples (default - Treatment)",
          choices = c(input$SelectIV, input$SelectGeno),
          multiple = F,
          selected = input$SelectIV
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
          label = "Independent Variable to subset the samples (default - Genotype)",
          choices = c(input$SelectIV, input$SelectGeno),
          multiple = F, 
          selected = input$SelectGeno
        )
      )
  })
  
  output$if_cubic_knots <- renderUI({
    if(input$model == "cubic"){
      textInput(
        inputId = "cubic_knots",
        label = "Time-point to split the cubic spline."
      )
    }
    else{
      return()
    }
  })
  
  output$Spline_df_select <- renderUI({
    if(input$model == "smooth"){
      selectizeInput(
        inputId = "spline_df",
        label = "The degrees of freedom are",
        choices = c("automatic", "user defined")
      )
    }
    else{
      return()
    }
  })
  
  output$Spline_user_df <- renderUI({
    if(input$model == "smooth"){
    if(input$spline_df == "user defined"){
      textInput(
        inputId = "model_smooth_df",
        label = "Number of degrees of freedom:"
      )
    }
    else{
      return()
    }}
    else{
      return()
    }
  })
  
  # - - - - - - - - -  >> PRE-calculations << - - - - - - - - - - 
  
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
    temp[,input$SelectTime] <- as.numeric(as.character(temp[,input$SelectTime]))
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
      # calculating r-squared for the cubic spline with one knot at the middle
      # calculating r-squared for the smoothed
    }
    
    colnames(things_to_model)[4] <- "Linear_model"
    colnames(things_to_model)[5] <- "Quadratic_model"
    colnames(things_to_model)[6] <- "Exponential_model"
    colnames(things_to_model)[7] <- "Square_root_model"
    colnames(things_to_model)[1] <- "IndepVar"
    model_sum <- summaryBy(Linear_model + Quadratic_model + Exponential_model + Square_root_model ~ IndepVar, data = things_to_model)
    
    colnames(model_sum)[1] <- "IndepVar"
    colnames(model_sum)[2] <- "Linear_model"
    colnames(model_sum)[3] <- "Quadratic_model"
    colnames(model_sum)[4] <- "Exponential_model"
    colnames(model_sum)[5] <- "Square_root_model"
    model_sum
  })
  
  output$Model_estimation <- renderDataTable({
    Model_est_data()
  })
  
  output$best_model_advice <- renderPrint({
    fraka <- Model_est_data()
    fraka1 <- colnames(fraka)[apply(fraka,1,which.max)]
    cat(paste("The model with the best fit is", fraka1[1]))
    cat("\n")
    cat("The model estimation is based on the r2 values for Linear, Quadratic, Exponential and Square root functions, presented in the table below.")
    cat("\n")
    cat("If your data contains many timepoints, you can consider fitting a polynomial curves, using smoothed / cubic splines available in the model menu.")
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
    
    temp[,input$SelectTime] <- as.numeric(as.character(temp[,input$SelectTime]))
    sub_set <- c(input$ModelIV, input$ModelSubIV, input$SelectID)
    things_to_model <- unique(temp[sub_set])
    
    for (i in 1:nrow(things_to_model)) {
      super_temp <- subset(temp, temp[, 1] == things_to_model[i, 1])
      super_temp2 <-
        subset(super_temp, super_temp[, 2] == things_to_model[i, 2])
      super_temp3 <-
        subset(super_temp2, super_temp2[, 3] == things_to_model[i, 3])
      
      if (input$model == "lin") {
        fit_lin <- lm(super_temp3[, 4] ~ super_temp3[, 5])
        things_to_model[i, 4] <- coefficients(fit_lin)[2]
        things_to_model[i, 5] <- coefficients(fit_lin)[1]
        things_to_model[i, 6] <- summary(fit_lin)$r.squared
        colnames(things_to_model)[4] <- "DELTA"
        colnames(things_to_model)[5] <- "INTERCEPT"
        colnames(things_to_model)[6] <- "r_squared"
      }
      
      if (input$model == "quad") {
        super_temp3$transformed <- sqrt(super_temp3[, 5])
        fit_quad <- lm(super_temp3[, 4] ~ super_temp3$transformed)
        things_to_model[i, 4] <- (coefficients(fit_quad)[2]) ^ 2
        things_to_model[i, 5] <- (coefficients(fit_quad)[1]) ^ 2
        things_to_model[i, 6] <- summary(fit_quad)$r.squared
        colnames(things_to_model)[4] <- "DELTA"
        colnames(things_to_model)[5] <- "INTERCEPT"
        colnames(things_to_model)[6] <- "r_squared"
      }
      
      if (input$model == "exp") {
        super_temp3$transformed <- log(super_temp3[, 5])
        fit_exp <- lm(super_temp3[, 4] ~ super_temp3$transformed)
        things_to_model[i, 4] <- log(coefficients(fit_exp)[2])
        things_to_model[i, 5] <- coefficients(fit_exp)[1]
        things_to_model[i, 6] <- summary(fit_exp)$r.squared
        colnames(things_to_model)[4] <- "DELTA"
        colnames(things_to_model)[5] <- "INTERCEPT"
        colnames(things_to_model)[6] <- "r_squared"
      }
      
      if (input$model == "sqr") {
        super_temp3$transformed <- (super_temp3[, 5]) ^ 2
        fit_sq <- lm(super_temp3[, 4] ~ super_temp3$transformed)
        things_to_model[i, 4] <- sqrt(coefficients(fit_sq)[2])
        things_to_model[i, 5] <- sqrt(coefficients(fit_sq)[1])
        things_to_model[i, 6] <- summary(fit_sq)$r.squared
        colnames(things_to_model)[4] <- "DELTA"
        colnames(things_to_model)[5] <- "INTERCEPT"
        colnames(things_to_model)[6] <- "r_squared"
      }
      if (input$model == "cubic") {
        knoty <- input$cubic_knots
        colnames(super_temp3)[4] <- "time"
        colnames(super_temp3)[5] <- "phenotype"
        fit_cub <- lm(phenotype ~ bs(time, knots=knoty), data=super_temp3)
      
        for(e in 1:length(fit_cub$coef)){
          things_to_model[i,(3+e)] <- fit_cub$coef[[e]]
        }
        things_to_model[i,(4+length(fit_cub$coef))] <- summary(fit_cub)$r.squared
        
        colnames(things_to_model)[4] <- "INTERCEPT"
        for(f in 2:length(fit_cub$coef)){
          colnames(things_to_model)[3+f] <- paste("Coef",f,sep="_")
        }
        colnames(things_to_model)[(4+length(fit_cub$coef))] <- "r_squared"
        }
      
      if(input$model == "smooth"){
        if(input$spline_df == "user determined"){
          fit_smooth <- smooth.spline(x = super_temp3[, 4], y = super_temp3[, 5], df= input$model_smooth_df)}
        if(input$spline_df == "automatic"){
        fit_smooth <- smooth.spline(x = super_temp3[, 4], y = super_temp3[, 5], cv=T)}
        
        for(e in 1:length(fit_smooth$fit$coef)){
          things_to_model[i,(3+e)] <- fit_smooth$fit$coef[e]
        }
        
        for(g in 1:length(unique(super_temp3[,4]))){
          super_temp3[g,6] <- predict(fit_smooth, unique(super_temp3[,4])[g])$y
        }
        
        x <- super_temp3[,6]
        y <- super_temp3[,5]
        test <- data.frame(x,y)
        rsq <- cor(test,method="pearson")[1,2]^2
        things_to_model[i,(4 + e)] <- rsq
        colnames(things_to_model)[4 + e] <- "r_squared"
        things_to_model[i,(5+e)] <- fit_smooth$df
        colnames(things_to_model)[5 + e] <- "df"
        
        super_temp$predict  
        for(f in 1:length(fit_smooth$fit$coef)){
          colnames(things_to_model)[3+f] <- paste("Coef",f,sep="_")
      }}
    }
    things_to_model
  })
  
  output$model_warning <- renderPrint({
    if(is.null(Model_temp_data())){
      return()}
    else {
    frajka <- Model_temp_data()
    frajka_boom <- subset(frajka, select=c("r_squared"))
    frajka_boom <- subset(frajka_boom, frajka_boom$r_squared < 0.7)                      
    how_much <- nrow(frajka_boom)
    
    cat(paste("There are ", how_much, " samples with r-square value below 0.7."))
    cat("\n")
    cat("You should consider checking those samples using fit-plots and even going back to your original data.")
    cat("\n")
    
    
    
    }
  })
  
  
  output$Model_data <- renderDataTable({
    Model_temp_data()
  })
  
  # Let's allow users to chose beteween one and multiple plot display
  
  output$Select_model_plot_type <- renderUI({
    if(is.null(Model_temp_data())){
      return()}
    else
      tagList(
        selectizeInput(
          inputId = "Select_model_type_plot",
          label = "View fit-plots as:",
          choices = c("single plot", "multiple plots")
        )
      )
  })
  
  
  
  # Interactive user input for Fit-Plot - to select specific sample for fitness examination
  output$Select_modelPlot <- renderUI({
    if (input$Select_model_type_plot == "multiple plots") {
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
        label = "Select a specific sample to view",
        choices = sample_list,
        multiple = F
      )
    )
  })
  
  # object saving the user-interactive plot for fitness (Fit-Plot)
  data_model_plot <- eventReactive(input$Model_graph_fit_select,{
    temp <- subset(my_data(), select = c(
      input$ModelIV,
      input$ModelSubIV,
      input$SelectID,
      input$SelectTime,
      input$ModelPheno))
    temp[,input$SelectTime] <- as.numeric(as.character(temp[,input$SelectTime]))
    
    temp$selection <- paste(temp[,input$ModelIV], temp[,input$ModelSubIV], temp[,input$SelectID], sep="_")
    docelowy <- subset(temp, temp$selection == input$Model_graph_fit_select)
    docelowy
  })
  
  # Fit-Plot - select specific sample to look at
  Fit_plotski_id <- eventReactive( input$Go_Model,{
    temp <- subset(my_data(), select = c(
      input$ModelIV,
      input$ModelSubIV,
      input$SelectID,
      input$SelectTime,
      input$ModelPheno))
    temp[,input$SelectTime] <- as.numeric(as.character(temp[,input$SelectTime]))
    sub_set <- c(input$ModelIV, input$ModelSubIV, input$SelectID)
    things_to_model <- unique(temp[sub_set])
    
    temp$selection <- paste(temp[,input$ModelIV], temp[,input$ModelSubIV], temp[,input$SelectID], sep="_")
    docelowy <- subset(temp, temp$selection == input$Model_graph_fit_select_multi)
  })
  
  
  
  # Fit-plots graph - single 
  Model_plot_single <- eventReactive(input$Model_graph_fit_select,{
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
      plot(pheno ~ time, main = title, ylab = paste("sqrt(",input$ModelPheno,")"), xlab = input$SelectTime)
      abline(lm(pheno ~ time), col="red")
    }
    
    if (input$model == "exp") {
      docelowy$helper <- log(docelowy[, input$ModelPheno])
      pheno <- docelowy$helper
      time <- docelowy[,input$SelectTime]
      title <- unique(docelowy$selection)
      plot(pheno ~ time, main = title, ylab = paste("log(",input$ModelPheno,")"), xlab = input$SelectTime)
      abline(lm(pheno ~ time), col="red")
    }
    
    if (input$model == "sqr") {
      docelowy$helper <- (docelowy[, input$ModelPheno])^2
      pheno <- docelowy$helper
      time <- docelowy[,input$SelectTime]
      title <- unique(docelowy$selection)
      plot(pheno ~ time, main = title, ylab = paste(input$ModelPheno, "^2"), xlab = input$SelectTime)
      abline(lm(pheno ~ time), col="red")
    }
    
    if(input$model == "cubic"){
      pheno <- docelowy[,input$ModelPheno]
      time <- docelowy[,input$SelectTime]
      fit_cub <- lm(pheno ~ bs(time, knots = input$cubic_knots))
      title <- unique(docelowy$selection)
      timelims <- range(time)
      time.grid <- seq(from = timelims[1], to = timelims[2])
      plot(pheno ~ time, main = title, ylab = input$ModelPheno, xlab = input$SelectTime)
      points(time.grid, predict(fit_cub, newdata=list(time = time.grid)), col = "darkgreen")
      abline(v=c(input$cubic_knots), lty=2, col="darkgreen")
    }
    
    if(input$model == "smooth"){
      fit_smooth <- smooth.spline(x = docelowy[,4], y = docelowy[,5], cv=T)
      pheno <- docelowy[,input$ModelPheno]
      time <- docelowy[,input$SelectTime]
      title <- unique(docelowy$selection)
      plot(docelowy[,5] ~ docelowy[,4], main = title, ylab = input$ModelPheno, xlab = input$SelectTime)
      lines(fit_smooth, col="purple", lwd=2)
    }
  })
  
  ## == >> Having a go at the dynamic ploting thing << == ##
  
  output$Model_graph_fit_select_multi_input <- renderUI({
    if(input$Select_model_type_plot == "single plot"){
      return()}
    else{
      tagList(
        selectizeInput(
          inputId = "Model_graph_fit_select_multi",
          label = "Fit-plots arranged by",
          choices = c("r-square values (low to high)", 
                      "r-square values (high to low)")
        ))}
  })
  
  output$Go_fitplot_model <- renderUI({
    if(input$Select_model_type_plot == "single plot"){
      return()
    }
    else{
      actionButton(inputId = "Go_fitplot", icon=icon("magic"), label="update fit-plot galery")
    }
  })
  
  output$Fit_plot_slider_input <- renderUI({
    if(input$Select_model_type_plot == "single plot"){
      return()
    }
    else{
      maxi <- nrow(Model_temp_data())-19
      sliderInput(inputId = "Fit_plot_slider", label = "Plot portion of the data starting from element number...", min=1, max=maxi, value=1, step=20)
    }
  })
  
  example_model <- eventReactive(input$Go_fitplot,{
    test <- as.data.frame(Model_temp_data())
    
    if(input$Model_graph_fit_select_multi == "r-square values (low to high)"){
      test2 <- test[order(test$r_squared),]}
    if(input$Model_graph_fit_select_multi == "r-square values (high to low)"){
      test2 <- test[order(-test$r_squared),]}
    test3 <- test2[input$Fit_plot_slider:(input$Fit_plot_slider+19),]
    test3
    
    test3$lista <- do.call(paste,c(test3[c(input$ModelIV, input$ModelSubIV, input$SelectID)], sep = "_"))
    real_list <- unique(test3$lista)
    
    temp <- subset(my_data(), select = c(
      input$ModelIV,
      input$ModelSubIV,
      input$SelectID,
      input$SelectTime,
      input$ModelPheno))
    temp[,input$SelectTime] <- as.numeric(as.character(temp[,input$SelectTime]))
    temp$lista <- do.call(paste,c(temp[c(input$ModelIV, input$ModelSubIV, input$SelectID)], sep = "_"))
    temp
    temp2 <- temp[temp$lista %in% real_list,]
    temp2
  })
  
  Fit_plot_multi_graphs <- eventReactive(input$Go_fitplot,{
    
    docelowy <- example_model()
    real_list <- unique(docelowy$lista)
    
    par(mfrow=c(4,5))
    
    
      for(i in 1:length(real_list)){
      super_temp <- subset(docelowy, docelowy$lista == real_list[i])  
      
      # The graph instruction from single plot - so should work :P
      if(input$model == "lin"){
        pheno <- super_temp[,input$ModelPheno]
        time <- super_temp[,input$SelectTime]
        title <- real_list[i]
        plot(pheno ~ time, main = title, ylab = input$ModelPheno, xlab = input$SelectTime)
        abline(lm(pheno ~ time), col="red")
      }
      
      if (input$model == "quad") {
        super_temp$helper <- sqrt(super_temp[, input$ModelPheno])
        pheno <- super_temp$helper
        time <- super_temp[,input$SelectTime]
        title <- real_list[i]
        plot(pheno ~ time, main = title, ylab = paste("sqrt(",input$ModelPheno,")"), xlab = input$SelectTime)
        abline(lm(pheno ~ time), col="red")
      }
      
      if (input$model == "exp") {
        super_temp$helper <- log(super_temp[, input$ModelPheno])
        pheno <- super_temp$helper
        time <- super_temp[,input$SelectTime]
        title <- real_list[i]
        plot(pheno ~ time, main = title, ylab = paste("log(",input$ModelPheno,")"), xlab = input$SelectTime)
        abline(lm(pheno ~ time), col="red")
      }
      
      if (input$model == "sqr") {
        super_temp$helper <- (super_temp[, input$ModelPheno])^2
        pheno <- super_temp$helper
        time <- super_temp[,input$SelectTime]
        title <- real_list[i]
        plot(pheno ~ time, main = title, ylab = paste(input$ModelPheno, "^2"), xlab = input$SelectTime)
        abline(lm(pheno ~ time), col="red")
      }
      
      if(input$model == "cubic"){
        pheno <- super_temp[,input$ModelPheno]
        time <- super_temp[,input$SelectTime]
        fit_cub <- lm(pheno ~ bs(time, knots = input$cubic_knots))
        title <- real_list[i]
        timelims <- range(time)
        time.grid <- seq(from = timelims[1], to = timelims[2])
        plot(pheno ~ time, main = title, ylab = input$ModelPheno, xlab = input$SelectTime)
        points(time.grid, predict(fit_cub, newdata=list(time = time.grid)), col = "darkgreen")
        abline(v=c(input$cubic_knots), lty=2, col="darkgreen")
      }
      
      if(input$model == "smooth"){
        fit_smooth <- smooth.spline(x = super_temp[,4], y = super_temp[,5], cv=T)
        pheno <- super_temp[,input$ModelPheno]
        time <- super_temp[,input$SelectTime]
        title <- real_list[i]
        plot(super_temp[,5] ~ super_temp[,4], main = title, ylab = input$ModelPheno, xlab = input$SelectTime)
        lines(fit_smooth, col="purple", lwd=2)
      }
    }
  })
  
  output$Fit_plot_only_graph <- renderPlot({
    if(input$Select_model_type_plot == "single plot"){
      Model_plot_single()
    }
    if(input$Select_model_type_plot == "multiple plots"){
      Fit_plot_multi_graphs()
    }
  })
  
  output$Model_download_button <- renderUI({
    if(is.null(Model_temp_data())){
      return()}
    else
      downloadButton("Download_model_data", label="Download modelled data")
  })  
  
  output$Download_model_data <- downloadHandler(
    filename = paste("Modelled ",input$ModelPheno, " with ", input$model ," MVApp.csv"),
    content <- function(file) {
      write.csv(Model_temp_data(), file)}
  )
  
  
  # - - - - - - - - - - - - >> SUMMARY STATS ON MODELING DATA << - - - - - - - - - - - - - - - 
  
  output$model_comparison_report <- renderPrint({
    temp <- Model_temp_data()  
    temp$facet <- temp[,input$model_facet_plot]
    temp$color <- temp[,input$model_color_plot]
    temp$phenotype <- temp[,input$model_trait_plot]
    thres <- as.numeric(as.character(input$Model_threshold))
    
    amod <- aov(phenotype ~ facet + color + facet*color, data = temp)
    cat("ANOVA report")
    cat("\n")
    if(summary(amod)[[1]][[5]][1] < thres){
    cat("The effect of ", input$model_facet_plot, "is SIGNIFICANT on ", input$model_trait_plot, "with a p-value of ", summary(amod)[[1]][[5]][1], ".")
    }
    if(summary(amod)[[1]][[5]][1] > thres){
    cat("The effect of ", input$model_facet_plot, "is NOT significant on ", input$model_trait_plot, "with a p-value of ", summary(amod)[[1]][[5]][1], ".")
    }
    
    if(summary(amod)[[1]][[5]][2] < thres){
    cat("\n")
    cat("The effect of ", input$model_color_plot, "is SIGNIFICANT on ", input$model_trait_plot, "with a p-value of ", summary(amod)[[1]][[5]][2], ".")
    }
    if(summary(amod)[[1]][[5]][2] > thres){
      cat("\n")
      cat("The effect of ", input$model_color_plot, "is NOT significant on ", input$model_trait_plot, "with a p-value of ", summary(amod)[[1]][[5]][2], ".")
    }
    
    if(summary(amod)[[1]][[5]][3] < thres){
    cat("\n")
    cat("The interaction between ", input$model_color_plot, "and ",  input$model_facet_plot, "is SIGNIFICANT on ", input$model_trait_plot, "with a p-value of ", summary(amod)[[1]][[5]][3], ".")
    }  
    if(summary(amod)[[1]][[5]][3] > thres){
      cat("\n")
      cat("The interaction between ", input$model_color_plot, "and ",  input$model_facet_plot, "is NOT significant on ", input$model_trait_plot, "with a p-value of ", summary(amod)[[1]][[5]][3], ".")
    }
  })
  
  # - - - - - - - - - - - - >> input gizmos << - -  - - - - - - - - - - - - - - - 
  output$Select_model_trait_to_plot <- renderUI({
    if(is.null(Model_temp_data())){
    return()}
    else{
      taka <- Model_temp_data()  
      list <- colnames(taka)
      listek <- c(input$ModelIV,input$ModelSubIV,input$SelectID)
      lista <- setdiff(list, listek)
      
      tagList(
        selectizeInput(
          inputId = "model_trait_plot",
          label = "Phenotype to plot",
          choices = lista,
          multiple = F
        ))}
  })
  
  output$Select_model_graph_to_plot <- renderUI({
    if(is.null(Model_temp_data())){
      return()
    }
    else{
      tagList(
        selectizeInput(
          inputId = "model_graph_plot",
          label = "Graph type",
          choices = c("box plot", "scatter plot", "bar graph"),
          multiple=F
          ))}
  })
    
  output$Select_model_error_bar_to_plot <- renderUI({
    if(is.null(Model_temp_data())){
      return()
    }
     if(input$model_graph_plot == "bar graph"){
      tagList(
        selectizeInput(
          inputId = "model_error_plot",
          label = "The error bars represent",
          choices = c("Standard Error", "Standard Deviation"),
          multiple = F
        ))}
    else{
      return()
    }
  })
  
  output$Select_model_color_to_plot <- renderUI({
    if(is.null(Model_temp_data())){
      return()
    }
    else{
      tagList(
        selectizeInput(
          inputId = "model_color_plot",
          label = "Color the graph by",
          choices = c(input$ModelIV,input$ModelSubIV,input$SelectID),
          multiple = F,
          selected= input$ModelSubIV))}
  })
  
  output$Select_model_facet_to_plot <- renderUI({
    if(is.null(Model_temp_data())){
      return()
    }
    else{
      tagList(
        selectizeInput(
          inputId = "model_facet_plot",
          label = "Split the graph by",
          choices = c(input$ModelIV,input$ModelSubIV,input$SelectID),
          multiple = F,
          selected= input$ModelIV))}
  })
  
  output$Select_model_facet_scale <- renderUI({
    if(is.null(Model_temp_data())){
      return()}
    else
      tagList(
        selectizeInput(
          inputId = "Select_model_facet_sc",
          label = "Scale of the split plot",
          choices = c("fixed", "free"),
          selected = "fixed"
        ))
  })
  

  
  output$Select_model_color_scale_to_plot <- renderUI({
    if(is.null(Model_temp_data())){
      return()}
    else
      tagList(
        selectizeInput(
          inputId = "Select_model_color_sc",
          label = "Select color palet",
          choices = c("Set1", "Set2", "Set3", "Pastel1", "Pastel2", "Paired", "Dark2", "Accent", "Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")
        ))
  })
  
  output$Select_model_background_color_to_plot <- renderUI({
    if(is.null(Model_temp_data())){
      return()}
    else
      tagList(
        checkboxInput(
          inputId = "Select_model_background",
          label = "Remove background"))
  })
  
  output$Select_model_maj_grid_to_plot <- renderUI({
    if(is.null(Model_temp_data())){
      return()}
    else
      tagList(
        checkboxInput(
          inputId = "Select_model_maj_grid",
          label = "Remove major grid lines"))
  })
  
  
  output$Model_Selection_of_colors <- renderUI({
    if(is.null(Model_temp_data())){
      return()
    }
    else
      selectizeInput(
        inputId = "Model_col_select_order",
        label = "Show samples based on:",
        choices = c("Order of the trait (increasing)", "Order of the trait (decreasing)", "Chose samples to plot")
      )
  }) 
  
  output$Select_number_of_colors <- renderUI({
    if(is.null(Model_temp_data())){
      return()
    }
    if(input$Model_col_select_order == "Chose samples to plot"){
      return()
    }
    else{
      sliderInput(
        inputId = "Model_col_number",
        label = "Show ... number of samples",
        min = 2,
        max = 12,
        value = 9)
    }
  })
  
  output$Select_portion_of_color <- renderUI({
    if(is.null(Model_temp_data())){
      return()
    }
    if(input$Model_col_select_order == "Chose samples to plot"){
      flop <- Model_temp_data()
      list <- unique(flop[,input$model_color_plot])
      selectizeInput(
        inputId = "Model_spec_color",
        label = "Plot specific samples:",
        choices = list,
        multiple = T
      )
    }
    else{
      flop <- Model_temp_data()
      max0 <- (length(unique(flop[,input$model_color_plot])) - (input$Model_col_number-1))
      sliderInput(
        inputId = "Model_col_portion",
        label = "Plot portion of the data starting from element number ...",
        min = 1,
        max = max0,
        value = 1,
        step = input$Model_col_number)}
  }) 
  
  
  # - - - - - - - >> CALCULATIONS <<- - - - - - - - - - - - 
  
  output$model_comparison_summary <- renderDataTable({
    temp <- Model_temp_data()
    temp[,input$SelectID] <- NULL
    temp_melt <- melt(temp, id=c(input$ModelIV, input$ModelSubIV))
    temp_sum <- summaryBy(value ~  ., data = temp_melt, FUN=function(x) {c(median = mean(x), sd = sd(x), se = std.error(x))})
    temp_sum
  })
  
  # Add download button here
  
  output$Model_summ_download_button <- renderUI({
    if(is.null(Model_temp_data())){
      return()}
    else
      downloadButton("Download_summ_model_data", label="Download summary statistics of modelled data")
  })  
  
  output$Download_summ_model_data <- downloadHandler(
    filename = paste("Summary Statistics of data modelled for ",input$ModelPheno, " with ", input$model ," MVApp.csv"),
    content <- function(file) {
      temp <- Model_temp_data()
      temp[,input$SelectID] <- NULL
      temp_melt <- melt(temp, id=c(input$ModelIV, input$ModelSubIV))
      temp_sum <- summaryBy(value ~  ., data = temp_melt, FUN=function(x) {c(median = median(x), sd = sd(x), se = std.error(x))})
      write.csv(temp_sum, file)}
  )
  
  # - - - - - - - >> GRAPHS <<- - - - - - - - - - - - 
  
  output$model_comparison_plotski <- renderPlotly({
    temp <- Model_temp_data()
    temp[,input$SelectID] <- NULL
    temp$colorek <- temp[,input$model_color_plot]
    temp_sub <- subset(temp, select = c("colorek", input$model_trait_plot))
    names(temp_sub)[2] <- "pheno"
    temp_sum <- summaryBy(pheno ~  colorek, data = temp_sub)
    
    
    if(input$Model_col_select_order == "Chose samples to plot"){
      from_sub <- subset(temp, temp$colorek %in% input$Model_spec_color)}
    
    if(input$Model_col_select_order == "Order of the trait (increasing)"){
      from_sort <- temp_sum[order(-temp_sum$pheno.mean),]  
      min <- as.numeric(as.character(input$Model_col_portion))
      max <- as.numeric(as.character(input$Model_col_portion)) + (input$Model_col_number-1)
      super_lista <- as.character(from_sort$colorek[min:max])
      from_sub <- subset(temp, temp$colorek %in% super_lista)
    }
    
    if(input$Model_col_select_order == "Order of the trait (decreasing)"){
      from_sort <- temp_sum[order(temp_sum$pheno.mean),]  
      min <- as.numeric(as.character(input$Model_col_portion))
      max <- as.numeric(as.character(input$Model_col_portion)) + (input$Model_col_number-1)
      super_lista <- as.character(from_sort$colorek[min:max])
      from_sub <- subset(temp, temp$colorek %in% super_lista)
    }
    
    dropski <- c("colorek")
    from_sub <- from_sub[, !(names(from_sub) %in% dropski)]
  
  if(input$model_graph_plot == "bar graph"){
    
    temp_melt <- melt(from_sub, id=c(input$ModelIV, input$ModelSubIV))
    temp_melt <- subset(temp_melt, temp_melt$variable == input$model_trait_plot)
    temp_sum <- summaryBy(value ~  ., data = temp_melt, FUN=function(x) {c(median = median(x), sd = sd(x), se = std.error(x))})
    temp_sum$color <- temp_sum[,input$model_color_plot]
    temp_sum$facet <- temp_sum[,input$model_facet_plot]
    benc <- ggplot(data = temp_sum, aes(x = color, y = value.median, fill = color))
    benc <- benc + geom_bar(stat = "identity", position=position_dodge(1))
    if(input$model_error_plot == "Standard Error"){
      benc <- benc + geom_errorbar(aes(ymin = value.median - value.se, ymax =value.median + value.se), position=position_dodge(1))
    }
    if(input$model_error_plot == "Standard Deviation"){
      benc <- benc + geom_errorbar(aes(ymin = value.median - value.sd, ymax =value.median + value.sd), position=position_dodge(1))
    }
    benc <- benc + facet_wrap(~facet, scale = input$Select_model_facet_sc) 
    #benc <- benc + scale_fill_manual(values = colorRampPalette(brewer.pal(input$Select_model_color_sc)))
  }
  
  temp_melt <- melt(from_sub, id=c(input$ModelIV, input$ModelSubIV))
  melt_sub <- subset(temp_melt, temp_melt$variable == input$model_trait_plot)
  melt_sub$id <- paste(melt_sub[,input$ModelIV], melt_sub[,input$ModelSubIV], sep="_")
  melt_sub$color <- melt_sub[,input$model_color_plot]
  melt_sub$facet <- melt_sub[,input$model_facet_plot]
  no <- c(input$ModelIV, input$ModelSubIV)
  no_fac <- setdiff(no, input$model_facet_plot)
  melt_sub$no_facet <- paste(melt_sub[,no_fac], sep="_")
  
  if(input$model_graph_plot == "box plot"){
        benc <- ggplot(data = melt_sub, aes(x= color, y = value, fill = color))
        benc <- benc + geom_boxplot()
        benc <- benc + facet_wrap(~facet, scale = input$Select_model_facet_sc) 
       # benc <- benc + scale_fill_brewer(palette = input$Select_model_color_sc)
      }
  
  if(input$model_graph_plot == "scatter plot"){
    benc <- ggplot(data = melt_sub, aes(x= color, y = value, fill = color))
    benc <- benc + geom_point()
    
    
      benc <- benc + facet_wrap(~ facet, scale = input$Select_model_facet_sc)
      #benc <- benc + scale_color_brewer(palette = input$Select_model_color_sc)
  }
  
  if(input$Select_model_background == T){
    benc <- benc + theme_minimal()}
  if(input$Select_model_maj_grid == T){
  benc <- benc + theme(panel.grid.major = element_blank())}

  benc <- benc + ylab(input$model_trait_plot)
  benc <- benc + xlab(input$model_color_plot)
  
  benc
  })
  
  # - - - - - - - - - - >> TUKEY MESSAGE << - - - - - - - - 
  output$model_comparison_Tukey <- renderPrint({
    
    Chosenpvalue<-as.numeric(as.character(input$Model_threshold))
    temp <- Model_temp_data()
    temp[,input$SelectID] <- NULL
    
    
    temp_melt <- melt(temp, id=c(input$ModelIV, input$ModelSubIV))
    melt_sub <- subset(temp_melt, temp_melt$variable == input$model_trait_plot)
    melt_sub$color <- melt_sub[,input$model_color_plot]
    melt_sub$facet <- melt_sub[,input$model_facet_plot]
    
    final <- subset(melt_sub, select = c("facet", "color", "value"))
    
    for (h in unique(final$facet)){
      subset_melt <- subset(final, final$facet == h )
      fit_tukey <- aov(subset_melt$value ~ subset_melt$color)
      out <- HSD.test(fit_tukey, "subset_melt$color", group = T, alpha = Chosenpvalue)
      
      out_tukey<-as.data.frame(out$groups)
      out_tukey$x<-row.names(out_tukey)
      n_name<-rep(h, length(levels(subset_melt$color)))
      out_tukey_n<-as.data.frame(cbind(out_tukey, n_name))
      colnames(out_tukey_n)[4] <- input$model_facet_plot
      colnames(out_tukey_n)[3] <- input$model_color_plot
      colnames(out_tukey_n)[1] <- input$model_trait_plot
      #colnames(out_tukey_n)<-c(input$model_facet_plot, input$model_trait_color, "Tukey's letters", input$model_trait_plot)
      out_tukey_f<-out_tukey_n[c(4,3,2,1)]
      print(as.data.frame(out_tukey_f), row.names=FALSE)
    }
  })
  
  
  # - - - - - - - >> DOWNLOAD BUTTONS <<- - - - - - - - - - - - 
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  #  - - - - - - - - - - >> DATA CURATION IN 4th TAB << - - - - - - - - - - - - - -
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # - - - - - - - - - - - - - >>  Interactive inputs << - - - - - - - - - - - - - -
  
  # Select which grouping variables you would like to use for outliers
  output$IV_outliers_selection <- renderUI({
    if(is.null(ItemList())){return()}
    else
      tagList(
        selectizeInput("IV_outliers",
                       label = "Indepentent Variables for grouping the samples to identify outlier samples",
                       choices=c(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID),
                       multiple=TRUE,
                       selected = c(input$SelectGeno, input$SelectIV, input$SelectTime))
      )})
  
  output$Outliers_selection_pheno  <- renderUI({
    if(is.null(Outlier_overview())){
      return()
    }
    if(input$Out_pheno_single_multi == "Single phenotype"){
      return()
    }
    if(input$Out_pheno_single_multi == "All phenotypes"){
      data <- Outlier_overview()
      max_out <- max(data$Add_outliers)
      min_out <- min(data$Add_outliers)
      sliderInput("outlier_cutoff", "Remove the samples which are characterized as an outlier in at least ... traits:", min_out, max_out, value = 2, step = 1)  
    }
  })
  
  # Chose the phenotype based on which you want to select the outliers
  
  output$Pheno_outliers <- renderUI({
    if(is.null(ItemList()) | (input$Out_pheno_single_multi == "All phenotypes")){return()}
    else
      tagList(
        selectizeInput("DV_outliers",
                       label = "Selected phenotype for the outlier selection",
                       choices= input$SelectDV,
                       multiple=F)
      )})
  
  # Chose the phenotype for the graphs
  
  output$Pheno_graph_outliers <- renderUI({
    if(is.null(ItemList()))
    {return()}
    else
      tagList(
        selectizeInput("DV_graph_outliers",
                       label = "Phenotype to plot",
                       choices= input$SelectDV,
                       multiple=F, 
                       selected = input$SelectDV[1])
      )})
  
  
  output$Q_facet <- renderUI({
    if(input$outlier_facet == T){
      tagList(
        selectInput("Facet_choice", "Variable to split the plot",
                    choices = c(input$SelectGeno, input$SelectIV, input$SelectTime))
      )
    }
    else{
      return()
    }
  })
  
  output$Q_colour <- renderUI({
    if(input$outlier_colour == T){
      tagList(
        selectInput("Colour_choice", "Variable to colour code the plot",
                    choices = c(input$SelectGeno, input$SelectIV, input$SelectTime))
      )
    }
    else{
      return()
    }
  })
  
  output$Facet_outlier_scale <- renderUI({
    if(input$outlier_facet == F){
      return()
    }  
    if(input$outlier_facet == T){
      selectizeInput(
        "out_facet_scale",
        label = "The scale of the split plot",
        choices=c("fixed", "free")
      )
      
    }
  })
  
  output$Outlier_error_bar <- renderUI({
    if(input$outlier_graph_type == "bar plot"){
      tagList(
        selectizeInput("out_error_bar",
                       label = "Error bars represent",
                       choices = c("Standard Error", "Standard Deviation"), multiple = F))}
  })
  
  output$Select_outlier_color_scale_to_plot <- renderUI({
    if(is.null(Model_temp_data())){
      return()}
    else
      tagList(
        selectizeInput(
          inputId = "Select_outl_color_sc",
          label = "Select color palet",
          choices = c("Set1", "Set2", "Set3", "Pastel1", "Pastel2", "Paired", "Dark2", "Accent", "Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")
        ))
  })
  
  output$Select_outlier_background_color_to_plot <- renderUI({
    if(is.null(Model_temp_data())){
      return()}
    else
      tagList(
        checkboxInput(
          inputId = "Select_outl_background",
          label = "Remove background"))
  })
  
  output$Select_outlier_maj_grid_to_plot <- renderUI({
    if(is.null(Model_temp_data())){
      return()}
    else
      tagList(
        checkboxInput(
          inputId = "Select_outl_maj_grid",
          label = "Remove major grid lines"))
  })
  
  
  # - - - - - - - - - - - - - >>  MAIN CALCULATIONS << - - - - - - - - - - - - - -
  
  ## TESTING OMIT.NA     %% Mitch %%
  my_data_nona <- eventReactive(input$Go_omitna == T, {
    my_data_nona <- my_data()[complete.cases(my_data()),] #use na.omit instead maybe?
    return(my_data_nona)
  })
  
  # General outlier testing table => highlighting the plants with problems in multiple traits:
  Outlier_overview <- eventReactive(input$Go_outliers,{
    if(input$Go_omitna == T){
      faka_boom <- my_data_nona()  
    }
    if(input$Go_omitna == F){
      faka_boom <- my_data()
    }
    faka_boom$id_test <- do.call(paste,c(faka_boom[c(input$IV_outliers)], sep = "_"))
    
    
    
    for(i in 1:length(input$SelectDV)){
      
      if(input$outlier_method == "1.5*IQR away from the mean (default)"){
        
        faka_laka <- subset(faka_boom, select=c("id_test", input$SelectDV[i]))
        faka_laka$pheno <- faka_laka[,(input$SelectDV[i])]
        bad_shit <- boxplot(faka_laka$pheno ~ faka_laka$id_test)$out
        # loop to add outliers:
        for(f in 1:nrow(faka_boom)){
          if(faka_laka$pheno[f] %in% bad_shit){
            faka_boom$outlier[f] <- TRUE}
          else{
            faka_boom$outlier[f] <- FALSE
          }}
        colnames(faka_boom)[which(names(faka_boom) == "outlier")] <- paste("out", input$SelectDV[i], sep = "_")
      }
      
      if(input$outlier_method == "Cook's Distance"){
        
        faka_laka <- subset(faka_boom, select=c("id_test", input$SelectDV[i]))
        faka_laka$pheno <- faka_laka[,(input$SelectDV[i])]
        mod <- lm(faka_laka$pheno ~ faka_laka$id_test)
        cooksd <- cooks.distance(mod)
        new_name <- do.call(paste,c(faka_laka[c(input$SelectDV[i])],"outl", sep="_"))
        faka_boom$outlier <- cooksd > 4*mean(cooksd)
        colnames(faka_boom)[which(names(faka_boom) == "outlier")] <- paste("out", input$SelectDV[i], sep = "_")
      }
      
      if(input$outlier_method == "Bonferonni outlier test"){
        
        faka_laka <- subset(faka_boom, select=c("id_test", input$SelectDV[i]))
        faka_laka$pheno <- faka_laka[,input$SelectDV[i]]
        mod <- lm(faka_laka$pheno ~ faka_laka$id_test)
        baddies <- car::outlierTest(mod)
        bad_shit <- names(baddies[[1]])
        bad_shit <- as.numeric(bad_shit)
        colnames(faka_boom)[endCol + i] <- paste("out", input$SelectDV[i], sep = "")
        faka_boom$outlier <- FALSE
        faka_boom[bad_shit,]$outlier <- TRUE
        colnames(faka_boom)[which(names(faka_boom) == "outlier")] <- paste("out", input$SelectDV[i], sep = "_")
      }
      
      if(input$outlier_method == "1xStDev from the median"){
        faka_laka <- subset(faka_boom, select=c("id_test", input$SelectDV[i]))
        faka_laka$pheno <- faka_laka[,input$SelectDV[i]]
        faka_sum <- summaryBy(pheno ~ id_test, data = faka_laka, FUN=function(x) {c(median = median(x), sd = sd(x))})
        faka_sum$min <- (faka_sum$pheno.median - (1*faka_sum$pheno.sd))
        faka_sum$max <- (faka_sum$pheno.median + (1*faka_sum$pheno.sd))
        faka_sum <- subset(faka_sum, select=c("id_test", "min", "max"))
        faka_laka <- merge(faka_laka, faka_sum, by="id_test")
        
        
        for(e in 1:nrow(faka_laka)){
          if(faka_laka$pheno[e] > faka_laka$max[e]){
            faka_boom$outlier[e] <- TRUE
          }
          if(faka_laka$pheno[e] < faka_laka$min[e]){
            faka_boom$outlier[e] <- TRUE
          }
          else{
            faka_boom$outlier[e] <- FALSE
          }}
        drops <- c("min", "max")
        faka_boom <- faka_boom[, !(names(faka_boom) %in% drops)]
        colnames(faka_boom)[which(names(faka_boom) == "outlier")] <- paste("out", input$SelectDV[i], sep = "_")
      }
      
      if(input$outlier_method == "2xStDev from the median"){
        faka_laka <- subset(faka_boom, select=c("id_test", input$SelectDV[i]))
        faka_laka$pheno <- faka_laka[,input$SelectDV[i]]
        faka_sum <- summaryBy(pheno ~ id_test, data = faka_laka, FUN=function(x) {c(median = median(x), sd = sd(x))})
        faka_sum$min <- (faka_sum$pheno.median - (2*faka_sum$pheno.sd))
        faka_sum$max <- (faka_sum$pheno.median + (2*faka_sum$pheno.sd))
        faka_sum <- subset(faka_sum, select=c("id_test", "min", "max"))
        faka_laka <- merge(faka_laka, faka_sum, by="id_test")
        
        
        for(e in 1:nrow(faka_laka)){
          if(faka_laka$pheno[e] > faka_laka$max[e]){
            faka_boom$outlier[e] <- TRUE
          }
          if(faka_laka$pheno[e] < faka_laka$min[e]){
            faka_boom$outlier[e] <- TRUE
          }
          else{
            faka_boom$outlier[e] <- FALSE
          }}
        drops <- c("min", "max")
        faka_boom <- faka_boom[, !(names(faka_boom) %in% drops)]
        colnames(faka_boom)[which(names(faka_boom) == "outlier")] <- paste("out", input$SelectDV[i], sep = "_")
      }
      
      if(input$outlier_method == "2.5xStDev from the median"){
        faka_laka <- subset(faka_boom, select=c("id_test", input$SelectDV[i]))
        faka_laka$pheno <- faka_laka[,input$SelectDV[i]]
        faka_sum <- summaryBy(pheno ~ id_test, data = faka_laka, FUN=function(x) {c(median = median(x), sd = sd(x))})
        faka_sum$min <- (faka_sum$pheno.median - (2.5*faka_sum$pheno.sd))
        faka_sum$max <- (faka_sum$pheno.median + (2.5*faka_sum$pheno.sd))
        faka_sum <- subset(faka_sum, select=c("id_test", "min", "max"))
        faka_laka <- merge(faka_laka, faka_sum, by="id_test")
        
        
        for(e in 1:nrow(faka_laka)){
          if(faka_laka$pheno[e] > faka_laka$max[e]){
            faka_boom$outlier[e] <- TRUE
          }
          if(faka_laka$pheno[e] < faka_laka$min[e]){
            faka_boom$outlier[e] <- TRUE
          }
          else{
            faka_boom$outlier[e] <- FALSE
          }}
        drops <- c("min", "max")
        faka_boom <- faka_boom[, !(names(faka_boom) %in% drops)]
        colnames(faka_boom)[which(names(faka_boom) == "outlier")] <- paste("out", input$SelectDV[i], sep = "_")
      }
      
      if(input$outlier_method == "3xStDev from the median"){
        faka_laka <- subset(faka_boom, select=c("id_test", input$SelectDV[i]))
        faka_laka$pheno <- faka_laka[,input$SelectDV[i]]
        faka_sum <- summaryBy(pheno ~ id_test, data = faka_laka, FUN=function(x) {c(median = median(x), sd = sd(x))})
        faka_sum$min <- (faka_sum$pheno.median - (3*faka_sum$pheno.sd))
        faka_sum$max <- (faka_sum$pheno.median + (3*faka_sum$pheno.sd))
        faka_sum <- subset(faka_sum, select=c("id_test", "min", "max"))
        faka_laka <- merge(faka_laka, faka_sum, by="id_test")
        
        
        for(e in 1:nrow(faka_laka)){
          if(faka_laka$pheno[e] > faka_laka$max[e]){
            faka_boom$outlier[e] <- TRUE
          }
          if(faka_laka$pheno[e] < faka_laka$min[e]){
            faka_boom$outlier[e] <- TRUE
          }
          else{
            faka_boom$outlier[e] <- FALSE
          }}
        drops <- c("min", "max")
        faka_boom <- faka_boom[, !(names(faka_boom) %in% drops)]
        colnames(faka_boom)[which(names(faka_boom) == "outlier")] <- paste("out", input$SelectDV[i], sep = "_")
      }
    }
    
    drops <- ("id_test")
    faka_boom <- faka_boom[ , !(names(faka_boom) %in% drops)]
    
    dropski <- c(input$IV_outliers, input$SelectDV)
    faka_kaboom <- faka_boom[, !(names(faka_boom) %in% dropski)]
    
    for(x in 1:nrow(faka_boom)){
      z <- faka_kaboom[x,]
      faka_boom$Add_outliers[x] <- length(z[z==TRUE]) 
    }
    
    return(faka_boom)
  })
  
  
  # Testing the outliers based on single phenotype
  
  Outlier_data <- eventReactive(input$Go_outliers, {
    
    if(input$Go_omitna == T){
      data_outl <- my_data_nona()  
    }
    if(input$Go_omitna == F){
      data_outl <- my_data()
    }
    
    outl <- subset(data_outl, select=c(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID, input$DV_outliers))
    outl$id_test <- do.call(paste,c(outl[c(input$IV_outliers)], sep = "_"))
    
    outl$pheno <- outl[,input$DV_outliers]
    
    # outliers based on 1.5IQR
    if(input$outlier_method == "1.5*IQR away from the mean (default)"){
      
      bad_shit <- boxplot(outl$pheno ~ outl$id_test)$out
      # Adding all the outliers as a column "outlier" with 1/0 values 
      for(e in 1:nrow(outl)){
        if(outl[,input$DV_outliers][e] %in% bad_shit){
          outl$outlier[e] <- TRUE }
        else{ 
          outl$outlier[e] <- FALSE }
      }
      drops <- c("pheno", "id_test")
      outl <- outl[ , !(names(outl) %in% drops)]
    }
    
    # outliers based on Cooks distance
    if(input$outlier_method == "Cook's Distance"){
      
      mod <- lm(outl$pheno ~ outl$id_test)
      cooksd <- cooks.distance(mod)
      outl$outlier <- cooksd > 4*mean(cooksd)
      drops <- c("pheno", "id_test")
      outl <- outl[ , !(names(outl) %in% drops)]
    }
    
    # outliers based on car::outlierTest
    if(input$outlier_method == "Bonferonni outlier test"){
      
      mod <- lm(outl$pheno ~ outl$id_test)
      baddies <- car::outlierTest(mod)
      bad_shit <- names(baddies[[1]])
      bad_shit <- as.numeric(bad_shit)
      outl$outlier <- FALSE
      outl[bad_shit,]$outlier <- TRUE
      drops <- c("pheno", "id_test")
      outl <- outl[ , !(names(outl) %in% drops)]
    }
    
    # outliers based on mean + SD  
    
    if(input$outlier_method == "1xStDev from the median"){
      
      out_sum <- summaryBy(pheno ~ id_test, data = outl, FUN=function(x) {c(median = median(x), sd = sd(x))})
      out_sum$min <- (out_sum$pheno.median - (1*out_sum$pheno.sd))
      out_sum$max <- (out_sum$pheno.median + (1*out_sum$pheno.sd))
      out_sum <- subset(out_sum, select=c("id_test", "min", "max"))
      outl <- merge(outl, out_sum, by="id_test")
      for(i in 1:nrow(outl)){
        if(outl$pheno[i] > outl$max[i]){
          outl$outlier[i] <- TRUE
        }
        if(outl$pheno[i] < outl$min[i]){
          outl$outlier[i] <- TRUE
        }
        else{
          outl$outlier[i] <- FALSE
        }}
      drops <- c("min","max", "pheno", "id_test")
      outl <- outl[ , !(names(outl) %in% drops)]
    }
    
    # outliers based on mean + 2*SD  
    
    if(input$outlier_method == "2xStDev from the median"){
      
      out_sum <- summaryBy(pheno ~ id_test, data = outl, FUN=function(x) {c(median = median(x), sd = sd(x))})
      out_sum$min <- (out_sum$pheno.median - (2*out_sum$pheno.sd))
      out_sum$max <- (out_sum$pheno.median + (2*out_sum$pheno.sd))
      out_sum <- subset(out_sum, select=c("id_test", "min", "max"))
      outl <- merge(outl, out_sum, by="id_test")
      for(i in 1:nrow(outl)){
        if(outl$pheno[i] > outl$max[i]){
          outl$outlier[i] <- TRUE
        }
        if(outl$pheno[i] < outl$min[i]){
          outl$outlier[i] <- TRUE
        }
        else{
          outl$outlier[i] <- FALSE
        }}
      drops <- c("min","max", "pheno", "id_test")
      outl <- outl[ , !(names(outl) %in% drops)]
    }
    
    # outliers based on mean + 2.5*SD  
    
    if(input$outlier_method == "2.5xStDev from the median"){
      
      out_sum <- summaryBy(pheno ~ id_test, data = outl, FUN=function(x) {c(median = median(x), sd = sd(x))})
      out_sum$min <- (out_sum$pheno.median - (2.5*out_sum$pheno.sd))
      out_sum$max <- (out_sum$pheno.median + (2.5*out_sum$pheno.sd))
      out_sum <- subset(out_sum, select=c("id_test", "min", "max"))
      outl <- merge(outl, out_sum, by="id_test")
      for(i in 1:nrow(outl)){
        if(outl$pheno[i] > outl$max[i]){
          outl$outlier[i] <- TRUE
        }
        if(outl$pheno[i] < outl$min[i]){
          outl$outlier[i] <- TRUE
        }
        else{
          outl$outlier[i] <- FALSE
        }}
      drops <- c("min","max", "pheno", "id_test")
      outl <- outl[ , !(names(outl) %in% drops)]
    }
    
    # outliers based on mean + 3*SD  
    
    if(input$outlier_method == "3xStDev from the median"){
      
      out_sum <- summaryBy(pheno ~ id_test, data = outl, FUN=function(x) {c(median = median(x), sd = sd(x))})
      out_sum$min <- (out_sum$pheno.median - (3*out_sum$pheno.sd))
      out_sum$max <- (out_sum$pheno.median + (3*out_sum$pheno.sd))
      out_sum <- subset(out_sum, select=c("id_test", "min", "max"))
      outl <- merge(outl, out_sum, by="id_test")
      for(i in 1:nrow(outl)){
        if(outl$pheno[i] > outl$max[i]){
          outl$outlier[i] <- TRUE
        }
        if(outl$pheno[i] < outl$min[i]){
          outl$outlier[i] <- TRUE
        }
        else{
          outl$outlier[i] <- FALSE
        }}
      drops <- c("min","max", "pheno", "id_test")
      outl <- outl[ , !(names(outl) %in% drops)]
    }
    
    data_outl$outlier <- outl$outlier
    return(data_outl)
    
  })
  
  
  Outliers_final_data <- eventReactive(input$Go_outliers,{
    if(input$Out_pheno_single_multi == "All phenotypes"){
      data_blob <- Outlier_overview()
    }
    if(input$Out_pheno_single_multi == "Single phenotype"){
      data_blob <- Outlier_data()  
    }
    return(data_blob)
  })
  
  
  # - - - - - - - - - - - - - >>  OUTPUT TABLES / GRAPHS / DOWNLOAD BUTTONS << - - - - - - - - - - - - - -
  
  # Table with outliers marked out
  output$Outlier_overview_table <- renderDataTable({
    test <- Outliers_final_data()
  })
  
  # Outlier report
  output$Outlier_report <- renderPrint({
    
    tescior <- Outliers_final_data()
    if(input$Out_pheno_single_multi == "All phenotypes"){
      number0 <- subset(tescior, tescior$Add_outliers >= input$outlier_cutoff)
      number <- nrow(number0)
      pheno <- paste(input$Out_pheno_single_multi)
      method <- paste(input$outlier_method, ". The sample is characterized as an outlier when it is classified as such in at least ", input$outlier_cutoff, " traits")
    }
    
    if(input$Out_pheno_single_multi == "Single phenotype"){
      number0 <- subset(tescior, tescior$outlier == "TRUE")
      number <- nrow(number0)
      pheno <-paste(input$DV_outliers)
      method <- paste(input$outlier_method)
    }
    cat(paste("There are ", number," outliers identified based on", pheno, "using", method))
    cat("\n")
    cat("DISCLAIMER:") 
    cat("\n")
    cat("Please think twice before removing a sample from your data, as it might contain valuable information.") 
    cat("\n")
    cat("We advice you to go back to your original data, look at the pictures (if you have them) and make sure that the sample is not representative for a VERY good reason.")
  })
  
  # Outlier free data (and table)
  Outlier_free_data <- eventReactive(input$Go_outliers,{
    if(input$Out_pheno_single_multi == "All phenotypes"){
      good_shit <- Outliers_final_data()
      good_shit2 <- subset(good_shit, good_shit$Add_outliers < input$outlier_cutoff)
      to_trash <- paste("out", input$SelectDV[1], sep = "_")
      for(i in 2:length(input$SelectDV)){
        new_trash <- paste("out", input$SelectDV[i], sep = "_")
        to_trash <- c(to_trash, new_trash)
      }
      to_trash <- c(to_trash, "Add_outliers")
    }
    if(input$Out_pheno_single_multi == "Single phenotype"){
      good_shit <-  Outliers_final_data()
      good_shit2 <- subset(good_shit, good_shit$outlier == FALSE)
      to_trash <- "outlier"
    }
    good_shit2 <- good_shit2[, !(names(good_shit2)) %in% to_trash]
    return(good_shit2)
  })
  
  # Outlier only data (and table)
  Outlier_only_data <- eventReactive(input$Go_outliers,{
    if(input$Out_pheno_single_multi == "All phenotypes"){
      bad_shit <- Outliers_final_data() 
      bad_shit2 <- subset(bad_shit, bad_shit$Add_outliers >= input$outlier_cutoff)
    }
    if(input$Out_pheno_single_multi == "Single phenotype"){
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
    if(is.null(Outliers_final_data())){
      return()}
    else{
      downloadButton("full_data_outliers", label="Download table with indicated outliers")}
  })  
  
  output$full_data_outliers <- downloadHandler(
    filename = paste("Marked outliers based on ",input$Out_pheno_single_multi,"_", input$DV_outliers ," identified with ", input$outlier_method, " MVApp.csv"),
    content <- function(file) {
      write.csv(Outliers_final_data(), file)}
  )
  
  
  # Download table with the outliers:
  
  output$Pheno_outlier_download <- renderUI({
    if(is.null(Outliers_final_data())){
      return()}
    else{
      downloadButton("data_outliers", label="Download table containing ONLY the outliers")}
  })  
  
  output$data_outliers <- downloadHandler(
    filename = paste("Outliers_based_on_",input$Out_pheno_single_multi,"_", input$DV_outliers ,"_identified_with_", input$outlier_method, "_MVApp.csv"),
    content <- function(file) {
      write.csv(Outlier_only_data(), file)}
  )
  
  # Download table free from the outliers:
  
  output$Pheno_outlier_free_download <- renderUI({
    if(is.null(Outliers_final_data())){
      return()}
    else{
      downloadButton("data_clean_final", label="Download table clean of the outlier values")}
  })  
  
  output$data_clean_final <- downloadHandler(
    filename = paste("Data free from outliers based on",input$Out_pheno_single_multi," ", input$DV_outliers ," identified with ", input$outlier_method, " MVApp.csv"),
    content <- function(file) {
      write.csv(Outlier_free_data(), file)}
  )
  
  # Lock the no-outlier data for further analysis:
  output$Outliers_save <- renderUI({
    if(is.null(Outliers_final_data())){
      return()
    }
    else{
      actionButton("lock_outliers", icon=icon("thumbs-down"), label = "Lock this outlier-free data for further analysis")
    }
  })
  
  no_outliers <- eventReactive(input$lock_outliers,{
    tratata <- Outlier_free_data()
    return(tratata)
  })
  
  # = = = >> GRAPH CONTAINING ALL THE DATA << = = = 
  output$outlier_graph <- renderPlotly({
    if(input$Go_omitna == T){
      data_outl <- my_data_nona()  
    }
    if(input$Go_omitna == F){
      data_outl <- my_data()
    }
    
    outl <- subset(data_outl, select=c(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID, input$DV_graph_outliers))
    lista <- input$IV_outliers
    
    if(input$outlier_facet == T){
      listb <- input$Facet_choice
      outl$listb <- outl[,input$Facet_choice]
      lista <- setdiff(lista, listb)}
    
    if(input$outlier_colour == T){
      listx <- input$Colour_choice
      outl$listx <- outl[,input$Colour_choice]
      listax <- setdiff(lista, listx)
      outl$listax <- do.call(paste,c(outl[listax], sep = "_"))
    }
    
    phenotype <- input$DV_graph_outliers
    outl$pheno <- outl[,input$DV_graph_outliers]
    
    
    outl$id_test <- do.call(paste,c(outl[lista], sep = "_"))
    
    if(input$outlier_graph_type == "bar plot"){
      outl$pheno <- as.numeric(outl$pheno)
      if(input$outlier_colour == T) {
        if(input$outlier_facet == F){
        out_sum <- summaryBy(pheno ~ listax + listx, data = outl, FUN = function(x) { c(m = mean(x), s = sd(x), se = std.error(x)) })  
        taka <- ggplot(out_sum, aes(x = listax, y= pheno.m, fill = listx))
        #taka <- taka + guides(fill=guide_legend(title=input$outlier_colour))
      }
      if(input$outlier_facet == T){
        out_sum <- summaryBy(pheno ~ listax + listx + listb, data = outl, FUN = function(x) { c(m = mean(x), s = sd(x), se = std.error(x)) })  
        taka <- ggplot(out_sum, aes(x = listax, y= pheno.m, fill = listx))
      }}
      
      if(input$outlier_colour == F){
        if(input$outlier_facet == T){
          out_sum <- summaryBy(pheno ~ id_test + listb, data = outl, FUN = function(x) { c(m = mean(x), s = sd(x), se = std.error(x)) })  
          taka <- ggplot(out_sum, aes(x = id_test, y= pheno.m))
        }
        if(input$outlier_facet == F){
          out_sum <- summaryBy(pheno ~ id_test, data = outl, FUN = function(x) { c(m = mean(x), s = sd(x), se = std.error(x)) })
          taka <- ggplot(out_sum, aes(x = id_test, y= pheno.m))
        }
      }
      
      taka <- taka + geom_bar(stat="identity", position=position_dodge(1))
      #taka <- taka + scale_fill_manual(values = colorRampPalette(brewer.pal(input$Select_model_color_sc)))
      
      if(input$out_error_bar == "Standard Deviation"){
        taka <- taka + geom_errorbar(aes(ymin=pheno.m-pheno.s, ymax=pheno.m+pheno.s), position=position_dodge(1))}
      if(input$out_error_bar == "Standard Error"){
      taka <- taka + geom_errorbar(aes(ymin=pheno.m-pheno.se, ymax=pheno.m+pheno.se), position=position_dodge(1))}
    }
    
    
    if(input$outlier_graph_type == "box plot"){
      if(input$outlier_colour == T){
        taka <- ggplot(outl, aes(x = id_test, y= pheno, color = listx))
      }
      else{
        taka <- ggplot(outl, aes(x = id_test, y= pheno))   
      }
      
      taka <- taka + geom_boxplot(position="dodge")
      #taka <- taka + scale_fill_brewer(palette = input$Select_outl_color_sc)
      }
    
    if(input$outlier_graph_type == "scatter plot"){
      
      if(input$outlier_colour == T){
        taka <- ggplot(outl, aes(x = id_test, y= pheno, color = listx))    
      }
      else{
        taka <- ggplot(outl, aes(x = id_test, y= pheno))      
      }
      
      taka <- taka + geom_point(position=position_dodge(1))
      #taka <- taka + scale_color_brewer(palette = input$Select_outl_color_sc)
      }
    
    
    
    if(input$outlier_facet == T){
    taka <- taka + facet_wrap(~listb, ncol=input$out_graph_facet_col, scale = input$out_facet_scale)}
    
    taka <- taka + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    taka <- taka + xlab("")
    taka <- taka + ylab(input$DV_graph_outliers)
    
    if(input$outlier_colour == T){
    taka <- taka + theme(legend.title=element_blank())
    }
  
  if(input$Select_outl_background == T){
    taka <- taka + theme_minimal()}
  if(input$Select_outl_maj_grid == T){
    taka <- taka + theme(panel.grid.major = element_blank())}
    taka
  })
  
  # = = = >> GRAPH WITH NO OUTLIERS << = = = 
  
  output$no_outliers_graph <- renderPlotly({
    data <- Outlier_free_data()
    clean_data <- subset(data, select=c(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID, input$DV_graph_outliers))
    lista <- input$IV_outliers
    
    if(input$outlier_facet == T){
      listb <- input$Facet_choice
      clean_data$listb <- clean_data[,input$Facet_choice]
      lista <- setdiff(lista, listb)}
    
    if(input$outlier_colour == T){
      listx <- input$Colour_choice
      clean_data$listx <- clean_data[,input$Colour_choice]
      listax <- setdiff(lista, listx)
      clean_data$listax <- do.call(paste,c(clean_data[listax], sep = "_"))
    }
    
    phenotype <- input$DV_graph_outliers
    clean_data$pheno <- clean_data[,input$DV_graph_outliers]
    
    
    clean_data$id_test <- do.call(paste,c(clean_data[lista], sep = "_"))
    
    if(input$outlier_graph_type == "bar plot"){
      clean_data$pheno <- as.numeric(clean_data$pheno)
      if(input$outlier_colour == T) {
        if(input$outlier_facet == F){
          clean_sum <- summaryBy(pheno ~ listax + listx, data = clean_data, FUN = function(x) { c(m = mean(x), s = sd(x), se = std.error(x)) })  
          jaka <- ggplot(clean_sum, aes(x = listax, y= pheno.m, fill = listx))
          #taka <- taka + guides(fill=guide_legend(title=input$outlier_colour))
        }
        if(input$outlier_facet == T){
          clean_sum <- summaryBy(pheno ~ listax + listx + listb, data = clean_data, FUN = function(x) { c(m = mean(x), s = sd(x), se = std.error(x)) })  
          jaka <- ggplot(clean_sum, aes(x = listax, y= pheno.m, fill = listx))
        }}
      
      if(input$outlier_colour == F){
        if(input$outlier_facet == T){
          clean_sum <- summaryBy(pheno ~ id_test + listb, data = clean_data, FUN = function(x) { c(m = mean(x), s = sd(x), se = std.error(x)) })  
          jaka <- ggplot(clean_sum, aes(x = id_test, y= pheno.m))
        }
        if(input$outlier_facet == F){
          clean_sum <- summaryBy(pheno ~ id_test, data = clean_data, FUN = function(x) { c(m = mean(x), s = sd(x), se = std.error(x)) })
          jaka <- ggplot(clean_sum, aes(x = id_test, y= pheno.m))
        }
      }
      
      jaka <- jaka + geom_bar(stat="identity", position=position_dodge(1))
      #jaka <- jaka + scale_fill_manual(values = colorRampPalette(brewer.pal(input$Select_model_color_sc)))
      
      if(input$out_error_bar == "Standard Deviation"){
        jaka <- jaka + geom_errorbar(aes(ymin=pheno.m-pheno.s, ymax=pheno.m+pheno.s), position=position_dodge(1))}
      if(input$out_error_bar == "Standard Error"){
        jaka <- jaka + geom_errorbar(aes(ymin=pheno.m-pheno.se, ymax=pheno.m+pheno.se), position=position_dodge(1))}
    }
    
    if(input$outlier_graph_type == "box plot"){
      if(input$outlier_colour == T){
        jaka <- ggplot(clean_data, aes(x = id_test, y= pheno, color = listx)) 
        jaka <- jaka + guides(fill=guide_legend(title=input$outlier_colour))
      }
      else{
        jaka <- ggplot(clean_data, aes(x = id_test, y= pheno))   
      }
      
      #jaka <- jaka + scale_fill_brewer(palette = input$Select_outl_color_sc)
      jaka <- jaka + geom_boxplot(position="dodge")}
    
    if(input$outlier_graph_type == "scatter plot"){
      if(input$outlier_colour == T){
        jaka <- ggplot(clean_data, aes(x = id_test, y= pheno, color = listx)) 
        jaka <- jaka + guides(fill=guide_legend(title=input$outlier_colour))
      }
      else{
        jaka <- ggplot(clean_data, aes(x = id_test, y= pheno))   
      }
      jaka <- jaka + geom_point(position=position_dodge(1))
      #jaka <- jaka + scale_color_brewer(palette = input$Select_outl_color_sc)
    }
    
    if(input$outlier_facet == T){
      jaka <- jaka + facet_wrap(~listb, ncol=3, scale = input$out_facet_scale)}
    
    jaka <- jaka + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    jaka <- jaka + xlab("")
    jaka <- jaka + ylab(input$DV_graph_outliers)
    
    if(input$outlier_colour == T){
      jaka <- jaka + theme(legend.title=element_blank())
    }
    
    if(input$Select_outl_background == T){
      jaka <- jaka + theme_minimal()}
    if(input$Select_outl_maj_grid == T){
      jaka <- jaka + theme(panel.grid.major = element_blank())}
    
    
    jaka
  })
  
  
  output$na_report <- renderText({
    if(input$Go_omitna == F){
      return()
    }
    else{
      sum_na<-nrow(my_data())-nrow(my_data_nona())
      return(paste(sum_na, " rows containing missing values that were removed.")) 
    }
  })
  
  # = = = = = = >> SUMMARY STATS << = = = = = = = = = 
  
  ## Added new input "$SelectSumm"  and output "$CustomSumm"  %% Mitch %%
  
  output$Data_for_SummaryStats <- renderUI({
    if(is.null(ItemList())){return ()
    } else tagList(
      selectizeInput(inputId = "SelectDataSumm",
                     label = "Dataset to be used for the summary stats",
                     choices= c("raw data", "missing values removed", "outliers removed"), selected="raw data", multiple = F))
  })
  
  output$CustomSumm <- renderUI({
    if((is.null(ItemList()))){return ()
    } else tagList(
      selectizeInput(inputId = "SelectSumm", 
                     label = "Calculations to perform:", 
                     choices=c("Mean", "Median", "StdDev", "StdErr", "Min", "Max", "Sum", "No.samples"), multiple=T))
  })
  
  
  ## Added list of summary functions "summfuns"   %% Mitch %%
  summfuns<-list(Mean = function(x) mean(x),
                 Median = function(x) median(x),
                 StdDev = function(x) sd(x),
                 StdErr = function(x) std.error(x),
                 Min = function(x) min(x),
                 Max = function(x) max(x),
                 Sum = function(x) sum(x),
                 No.samples = function(x) length(x))
  
  sum_data <- eventReactive(input$Go_SummaryStat, {
    if(input$SelectDataSumm == "raw data"){
      melted_icecream <- my_data()
      drops <-input$SelectID
      melted_icecream <- melted_icecream[ , !(names(melted_icecream)%in% drops)]
      melted_icecream <- melt(melted_icecream, id=c(input$SelectGeno, input$SelectIV, input$SelectTime))
    }
    if(input$SelectDataSumm == "missing values removed"){
      melted_icecream <- my_data_nona()
      drops <-input$SelectID
      melted_icecream <- melted_icecream[ , !(names(melted_icecream)%in% drops)]
      melted_icecream <- melt(melted_icecream, id=c(input$SelectGeno, input$SelectIV, input$SelectTime))
    }
    if(input$SelectDataSumm == "outliers removed"){
      melted_icecream <- Outlier_free_data()
      drops <-c(input$SelectID, "outlier")
      melted_icecream <- melted_icecream[ , !(names(melted_icecream)%in% drops)]
      melted_icecream <- melt(melted_icecream, id=c(input$SelectGeno, input$SelectIV, input$SelectTime))
    }
    
    # TO DO:
    # we need to get rid of the SelectID column before doing any Summary Stat on the data <3<3<3 MMJ <3<3<3
    
    ## Added call to selected summary stats functions "FUN=summfuns[input$SelectSumm]"     %% Mitch %%
    sum_my_data<-summaryBy(value ~., data=melted_icecream, FUN=summfuns[input$SelectSumm])
    
    ## Label columns based on chosen summary stats     %% Mitch %%
    colnames(sum_my_data)<-c(input$SelectGeno, input$SelectIV, input$SelectTime, "Dependent_Variable", input$SelectSumm)
    
    return(sum_my_data)
  })
  
  output$sum_data <- renderDataTable({
    sum_data()
  })
  
  output$Sum_download_button <- renderUI({
    if(is.null(sum_data())){
      return()}
    else
      downloadButton("data_sum", label="Download summary statistics data")
  })
  
  output$data_sum <- downloadHandler(
    filename = "Summary stats MVApp.csv",
    content <- function(file) {
      write.csv(sum_data(), file)}
  )
  
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # - - - - - - - - - - - - >> DATA EXPLORATION IN 5th TAB << - - - - - - - - - - -
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ##select dataset
  output$Histo_Pheno_data <- renderUI({
    if(is.null(ItemList())){return()}
    else
      tagList(
        selectizeInput(
          inputId = "Histo_data",
          label = "Dataset to explore:",
          choices = c("raw data", "missing values removed", "outliers removed"), multiple = F))
  })  
  
  Histo_data_type <- eventReactive(exists(input$Histo_data),{
    if(input$Histo_data == "raw data"){
      Histo_data_type <- my_data()
    }
    if(input$Histo_data == "missing values removed"){
      Histo_data_type <- my_data_nona()
    }
    if(input$Histo_data == "outliers removed"){
      Histo_data_type <- Outlier_free_data()
    }
    Histo_data_type
  })
  
  #### HISTOGRAMS  
  
  output$HisIV <- renderUI({
    if ((input$Go_Data == FALSE)) {
      return ()
    } else
      tagList(
        selectizeInput(
          inputId = "HisIV",
          label = "Independent Variable to subset the data:",
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
          label = "Phenotype to plot",
          choices = c(
            input$SelectDV
          ),
          multiple = F
        )
      )
  })
  
  output$Chosenthreshold <- renderUI({
    if ((input$Go_Data == FALSE)) {
      return ()
    } else
      tagList(
        selectizeInput(
          inputId = "Chosenthreshold",
          label = "p-value threshold:",
          choices = c(0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.05, 0.1),
          selected = 0.05,
          multiple = F
        )
      )
  })
  
  
  ###to choose the method to correct for multiple testing
  #output$Chosenmultipletest <- renderUI({
  # if ((input$Go_Data == FALSE)) {
  #  return ()
  #} else
  # tagList(
  #  selectizeInput(
  #   inputId = "Chosenmultipletesting",
  #  label = "Select the method to correct p-values for multiple testing",
  # choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
  #selected = "bonferroni",
  #multiple = F
  #)
  #)
  #})
  
  output$Plotfacets <- renderUI({
    if(input$plot_facet == T){
      tagList(
        selectInput("Plotfacet_choice", "Independent Variable to split the plots",
                    choices = c(setdiff(list(input$SelectGeno, input$SelectIV, input$SelectTime),input$HisIV)))
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
          label = "Plot type",
          choices = c("Histogram with counts on y-axis", "Histogram with density on y-axis"),
          selected = "Histogram with counts on y-axis",
          multiple = F
        )
      )
  })
  
  
  
  
  output$HistPlot <- renderPlotly({
    
    my_his_data<-Histo_data_type()[,c(input$HisDV,input$HisIV,input$Plotfacet_choice)]
    #groupIV<-input$HisIV
    
    if(input$plot_facet ==T){
      facetIV<-input$Plotfacet_choice
      my_his_data$facetIV<-my_his_data[,input$Plotfacet_choice]
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
      
      if (input$HistType == "Histogram with counts on y-axis") {
        fit <- ggplot(my_his_data, aes(x=my_his_data[,1], fill=my_his_data[,2])) + xlab(names(my_his_data[1])) + geom_histogram(size=0.6, alpha=0.3, col="black") + labs(fill=names(my_his_data[2]))
        fit <- fit + facet_wrap(~facetIV)
      }
      if (input$HistType == "Histogram with density on y-axis" ) { 
        fit <- ggplot(my_his_data, aes(x=my_his_data[,1], fill=my_his_data[,2])) + xlab(names(my_his_data[1]))  + geom_density(alpha = 0.3) + labs(fill=names(my_his_data[2]))
        fit <- fit + facet_wrap(~facetIV)
      }
    }
    
    
    if(input$plot_facet ==F){
      if (input$HistType == "Histogram with counts on y-axis") {
        fit <- ggplot(my_his_data, aes(x=my_his_data[,1], fill=my_his_data[,2])) + xlab(names(my_his_data[1])) + geom_histogram(size=0.6, alpha=0.3, col="black") + labs(fill=names(my_his_data[2]))
      }
      if (input$HistType == "Histogram with density on y-axis" ) { 
        fit <- ggplot(my_his_data, aes(x=my_his_data[,1], fill=my_his_data[,2])) + xlab(names(my_his_data[1]))  + geom_density(alpha = 0.3) + labs(fill=names(my_his_data[2]))
      }
    }
    ggplotly(fit)
    
  }) 
  
  
  output$Shapiro<- renderPrint({
    if(input$plot_facet ==T){
      my_his_data<-Histo_data_type()[,c(input$HisDV,input$HisIV,input$Plotfacet_choice)]
      groupedIV<-input$HisIV
      groupedFacet<-input$Plotfacet_choice
      my_his_data$combinedTID<-paste(my_his_data[,groupedIV], my_his_data[,groupedFacet], sep="_")
      #my_his_data$groupID<-do.call(paste, c(my_his_data[groupIV], sep="_"))
      my_his_data$combinedTID<-as.factor(my_his_data$combinedTID)
      m_Trows<-length(levels(my_his_data$combinedTID))
      facetting_shapiro<-rep(NA, m_Trows)
      interpret_shapiro<-rep(NA,m_Trows)
      shapiro_pvalue<-rep(NA,m_Trows)
      for (i in unique(my_his_data$combinedTID)){
        subsetted_shapiro<-subset(my_his_data, my_his_data$combinedTID==i)
        facetting_shapiro[i]<-i
        shapirotest<-shapiro.test(subsetted_shapiro[,1])
        shapiro_pvalue[i]<-signif(shapirotest$p.value,5)
        if (shapirotest$p.value < as.numeric(as.character(input$Chosenthreshold)) ) {
          interpret_shapiro[i]<-"Data is NOT normally distributed"
        } else {
          interpret_shapiro[i]<-"Data is NORMALLY distributed"
        }
        
        temp_shapiro<-as.data.frame(cbind(facetting_shapiro,shapiro_pvalue, interpret_shapiro))
      }
      colnames(temp_shapiro)<-c("Group", "p_value", "")
      temp_shapiro<-na.omit(temp_shapiro)
      
      sig_shapiro<-subset(temp_shapiro, as.numeric(as.character(temp_shapiro$p_value)) < as.numeric(as.character(input$Chosenthreshold)))
      list_sig_shapiro<- as.vector(sig_shapiro[,1])
      cat("The data for ",input$HisDV, "sub-grouped by", input$HisIV, "and", input$Plotfacet_choice, "does not show the normal distribution in the following samples:")
      cat("\n")
      cat(list_sig_shapiro, sep=", ")
      #cat(cat(list_sig_shapiro, sep=", "), "for", input$HisDV, "with sub-grouping by", input$HisIV, "and", input$Plotfacet_choice,  "might NOT have a normal distribution")
      
      if(input$showShapirotest==T){
        cat("\n")
        cat(paste("The p-value of the Shapiro-Wilk test of normality for ", input$HisDV, " for each selected group is:", "\n", "\n", sep=""))
        print(temp_shapiro, row.names=FALSE)
      }
    }
    
    
    if(input$plot_facet == F){
      my_his_data<-Histo_data_type()[,c(input$HisDV,input$HisIV)]
      shapiroIV<-input$HisIV
      m_Frows<-length(levels(as.factor(my_his_data[,shapiroIV])))
      interpret_shapiro<-rep(NA,m_Frows)
      facetting_shapiro<-rep(NA, m_Frows)
      shapiro_pvalue<-rep(NA,m_Frows)
      for (i in unique(my_his_data[,shapiroIV])){
        subsetted_shapiro<-subset(my_his_data, my_his_data[,shapiroIV]==i)
        facetting_shapiro[i]<-i
        shapirotest<-shapiro.test(subsetted_shapiro[,1])
        shapiro_pvalue[i]<-signif(shapirotest$p.value,5)
        if (shapirotest$p.value < as.numeric(as.character(input$Chosenthreshold)) ) {
          interpret_shapiro[i]<-"Data is NOT normally distributed"
        } else {
          interpret_shapiro[i]<-"Data is NORMALLY distributed"
        }
        temp_shapiro<-as.data.frame(cbind(facetting_shapiro,shapiro_pvalue, interpret_shapiro))
      }
      colnames(temp_shapiro)<-c("", "p_value", "")
      temp_shapiro<-na.omit(temp_shapiro)
      
      sig_shapiro<-subset(temp_shapiro, as.numeric(as.character(temp_shapiro$p_value)) < as.numeric(as.character(input$Chosenthreshold)))
      list_sig_shapiro<- as.vector(sig_shapiro[,1])
      #list_sha <- unique(list_sig_shapiro)
      #paste("<font color=\"#008080\"><b>",list_sha, "</b></font>")
      #print(colore)
      cat(cat(list_sig_shapiro, sep=", "), "for", input$HisDV, "with sub-grouping by", input$HisIV, "does NOT have a normal distribution")
      #paste(type='text/css', 'list_sig_shapiro, {color = "red"}')
      
      if(input$showShapirotest==T){
        cat("\n")
        cat(paste("The p-value of the Shapiro-Wilk test of normality for ", input$HisDV, " for each selected group is:", "\n", "\n", sep=""))
        print(temp_shapiro, row.names=FALSE)
      }
    }
    
  })
  
  output$QQplot_slider <- renderUI({
    if(input$showShapirotest == T){
      sliderInput(
        "QQplots_graph_col",
        label = "Display QQ plots in ... columns:",
        1, 9, 3
      )
    }
    else{
      return()
    }
  })
  
  # Add here a conditional slider
  
  output$QQplot_slider2 <- renderUI({
    if(input$showShapirotest == F){
      return()
    }
    else{
    my_his_data<-Histo_data_type()[,c(input$HisDV,input$HisIV,input$Plotfacet_choice)]
    groupedIV<-input$HisIV
    groupedFacet<-input$Plotfacet_choice
    my_his_data$combinedTID<-paste(my_his_data[,groupedIV], my_his_data[,groupedFacet], sep="_")
    my_his_data$combinedTID<-as.factor(my_his_data$combinedTID)
    length_of_elements <- as.numeric(length(unique(my_his_data$combinedTID)))
    see_those_graphs <- as.numeric(input$QQplots_graph_col*4)
    maximum <- as.numeric(length_of_elements)
    if(see_those_graphs >  length_of_elements){
      return()
    }
    else{
     sliderInput(
       inputId = "QQplots_portion_show",
       label = "Plot portion of the data starting from element number...",
       min = 1, max = maximum, ticks=T, step = see_those_graphs, value = 1) 
    }
    }
  })
  
  
  output$QQplot <- renderPlot({
    if(input$showShapirotest==T){
      
      if(input$plot_facet ==T){
        my_his_data<-Histo_data_type()[,c(input$HisDV,input$HisIV,input$Plotfacet_choice)]
        groupedIV<-input$HisIV
        groupedFacet<-input$Plotfacet_choice
        my_his_data$combinedTID<-paste(my_his_data[,groupedIV], my_his_data[,groupedFacet], sep="_")
        #my_his_data$groupID<-do.call(paste, c(my_his_data[groupIV], sep="_"))
        my_his_data$combinedTID<-as.factor(my_his_data$combinedTID)
        
        # par(mfrow=c(4,5))
        length_of_elements <- as.numeric(length(unique(my_his_data$combinedTID)))
        col_number <- as.numeric(input$QQplots_graph_col)
        see_those_graphs <- as.numeric(col_number*4)
        lista <- unique(my_his_data$combinedTID)
        start <- as.numeric(input$QQplots_portion_show)
        end <- (start + see_those_graphs)-1
        if(end > length_of_elements){
        end <- length_of_elements  
        }
        
        to_plot <- lista[start:end]
        
        if(length_of_elements < see_those_graphs){
          par(mfrow=c(4,col_number))
          for (i in 1:length(lista)){
            subsetted_shapiro<-subset(my_his_data, my_his_data$combinedTID==lista[i])
            #QQ<-ggplot(data=as.data.frame(qqnorm(subsetted_shapiro[,1] , plot=F)), mapping=aes(x=x, y=y)) +  geom_point() + geom_smooth(method="lm", se=FALSE)
            #QQ<-QQ + facet_wrap(~combinedTID)
            QQplot<-qqnorm(subsetted_shapiro[,1], main=paste(input$HisDV, "for ", lista[i]))
            QQline<-qqline(subsetted_shapiro[,1], col = 2)
            QQplot
            QQline
          }}
        
        if(length_of_elements > see_those_graphs){
          par(mfrow=c(4,col_number))
          for (i in 1:length(to_plot)){
            subsetted_shapiro<-subset(my_his_data, my_his_data$combinedTID==to_plot[i])
            #QQ<-ggplot(data=as.data.frame(qqnorm(subsetted_shapiro[,1] , plot=F)), mapping=aes(x=x, y=y)) +  geom_point() + geom_smooth(method="lm", se=FALSE)
            #QQ<-QQ + facet_wrap(~combinedTID)
            QQplot<-qqnorm(subsetted_shapiro[,1], main=paste(input$HisDV, "for ", to_plot[i]))
            QQline<-qqline(subsetted_shapiro[,1], col = 2)
            QQplot
            QQline
          }}}
      
      
      if(input$plot_facet ==F){
        my_his_data<-Histo_data_type()[,c(input$HisDV,input$HisIV)]
        shapiroIV<-input$HisIV
        my_his_data$shapiroIV<-my_his_data[,input$HisIV]
        par(mfrow=c(4,input$QQplots_graph_col))
        for (i in unique(my_his_data[,shapiroIV])){
          subsetted_shapiro<-subset(my_his_data, my_his_data$shapiroIV==i)
          #QQ<-ggplot(data=as.data.frame(qqnorm(subsetted_shapiro[,1] , plot=F)), mapping=aes(x=x, y=y)) + geom_point() + geom_smooth(method="lm", se=FALSE)
          #QQ<-QQ + facet_wrap(~shapiroIV)
          QQplot<-qqnorm(subsetted_shapiro[,1], main=paste(input$HisDV, "for ", i))
          QQline<-qqline(subsetted_shapiro[,1], col = 2)
          QQplot
          QQline
        }
      }}
    else{}
    #ggplotly(QQ)
  })
  
  
  output$Tukeylisting <- renderPrint({
    Chosen_tukey_threshold <- as.numeric(as.character(input$Chosenthreshold))
    
    if(input$plot_facet ==T){
      my_his_data<-Histo_data_type()[,c(input$HisDV,input$HisIV,input$Plotfacet_choice)]
      my_his_data[,2]<-as.factor(my_his_data[,2])
      for (i in unique(my_his_data[,3])){
        subsetted_data<- subset(my_his_data, my_his_data[,3]==i)
        fit_tukey<-aov(subsetted_data[,1] ~ subsetted_data[,2], data=subsetted_data)
        out<-HSD.test(fit_tukey, "subsetted_data[, 2]", group=TRUE, alpha = Chosen_tukey_threshold) ##note that there is an extra space after comma because this is how it is written in summary(fit_graph)
        out_tukey<-as.data.frame(out$groups)
        out_tukey$x<-row.names(out_tukey)
        n_name<-rep(i, length(levels(subsetted_data[,2])))
        out_tukey_n<-as.data.frame(cbind(out_tukey, n_name))
        colnames(out_tukey_n)<-c(names(subsetted_data[1]), "Tukey's letters", names(subsetted_data)[2], names(subsetted_data[3]))
        out_tukey_f<-out_tukey_n[c(4,3,2,1)]
        print(as.data.frame(out_tukey_f), row.names=FALSE)
      }
    }
    
    if(input$plot_facet ==F){
      my_his_data<-Histo_data_type()[,c(input$HisDV,input$HisIV)]
      my_his_data[,2]<-as.factor(my_his_data[,2])
      fit_tukey<-aov(my_his_data[,1] ~ my_his_data[,2], data=my_his_data)
      out<-HSD.test(fit_tukey, "my_his_data[, 2]", group=TRUE, alpha = Chosen_tukey_threshold) ##note that there is an extra space after comma because this is how it is written in summary(fit_graph)
      out_tukey<-as.data.frame(out$groups)
      out_tukey$x<-row.names(out_tukey)
      colnames(out_tukey)<-c(names(my_his_data[1]), "Significant groups based on Tukey's pairwise comparison ", names(my_his_data)[2])
      out_tukey_f<-out_tukey[c(3,1,2)]
      print(out_tukey_f, row.names=FALSE)
    }
  })
  
  
  #the margin needs to be fixed to be able to see the y-lab
  output$Boxes <- renderPlotly({
    my_his_data<-Histo_data_type()[,c(input$HisDV,input$HisIV,input$Plotfacet_choice)]
    #groupIV<-input$HisIV
    
    if(input$plot_facet ==T){
      facetIV<-input$Plotfacet_choice
      my_his_data$facetIV<-my_his_data[,input$Plotfacet_choice]
      
      box_graph <- ggplot(my_his_data, aes(x=my_his_data[,2], y=my_his_data[,1], fill=my_his_data[,2])) + xlab(names(my_his_data[2])) + ylab(names(my_his_data[1])) + geom_boxplot()
      box_graph<- box_graph + facet_wrap(~facetIV) + scale_fill_discrete(names(my_his_data[2]))
    }
    else{
      box_graph <- ggplot(my_his_data, aes(x=my_his_data[,2], y=my_his_data[,1], fill=my_his_data[,2])) + xlab(names(my_his_data[2])) + ylab(names(my_his_data[1])) + geom_boxplot()
      box_graph<- box_graph + scale_fill_discrete(names(my_his_data[2]))
    }
    ggplotly(box_graph)
  })
  
  
  
  
  ####We need to correct for multiple testing p.adjust(p, method = p.adjust.methods, n = length(p))
  # p.adjust.methods
  # c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
  #   "fdr", "none")
  #OR maybe use anova(lm(~))?
  ###ANOVA summary table output
  output$ANOVAtest <- renderPrint({
    my_his_data<-Histo_data_type()[,c(input$HisDV,input$HisIV,input$Plotfacet_choice)]
    my_his_data[,2]<-as.factor(my_his_data[,2])
    if(input$plot_facet ==T){
      n_rows<-length(levels(my_his_data[,3]))
      facetting<-rep(NA,n_rows)
      p_values_anova<-rep(NA,n_rows)
      interpret_anova<-rep(NA,n_rows)
      # p_values_anovacorr<-rep(NA,n_rows)
      for (i in unique(my_his_data[,3])){
        subsetted_data<- subset(my_his_data, my_his_data[,3]==i)
        facetting[i]<-i
        fit_anova<-aov(subsetted_data[,1] ~ subsetted_data[,2], data=subsetted_data)
        #print(fit_anova)
        #print(summary(fit_anova))
        p_values_anova[i]<-signif(summary(fit_anova)[[1]][[1,"Pr(>F)"]],5) #summary of anova is a list, so we need to access the 1st element which is the results and then in 1st row column Pr>F you have the p-value
        #p_values_anovacorr[i]<-p.adjust(p, method = Chosenmultipletesting)
        #print(paste("The p-value of the ANOVA test is", pvalue))
        #temp_anova<-as.data.frame(cbind(facetting, p_values_anova, p_values_anovacorr))
        if (summary(fit_anova)[[1]][[1,"Pr(>F)"]]  < as.numeric(as.character(input$Chosenthreshold)) ) {
          interpret_anova[i]<-"SIGNIFICANT difference in means"
        } else {
          interpret_anova[i]<-"NO significant difference in means"
        }
        temp_anova<-as.data.frame(cbind(facetting, p_values_anova,interpret_anova))
      }
      temp_anova<-na.omit(temp_anova)
      colnames(temp_anova) <- c("", "p_value","")
      #colnames(temp_anova) <- c("", "p_value", "p_value corrected")
      cat(paste("The p-value of the ANOVA test between different ", input$HisIV, "S for each ", input$Plotfacet_choice, " is:", "\n", "\n", sep=""))
      print(temp_anova, row.names=FALSE)
    }
    if(input$plot_facet ==F){ 
      fit_anova <- aov(my_his_data[,1] ~ as.factor(my_his_data[,2]), data = my_his_data)
      #print(fit_anova)
      #br()
      #print(summary(fit_anova))
      pvalue_ANOVA<-signif(summary(fit_anova)[[1]][[1,"Pr(>F)"]],5)
      cat("ANOVA", "\n")
      cat(paste("The p-value of the ANOVA test between different ", input$HisIV, "S is ", pvalue_ANOVA,"\n", "\n", sep=""))
      
      
      if (summary(fit_anova)[[1]][[1,"Pr(>F)"]]  < as.numeric(as.character(input$Chosenthreshold)) ) {
        cat("Significant difference in means")
      } else {
        cat("NO significant difference in means")
      }
    }
  })
  
  
  
  
  ####We need to correct for multiple testing p.adjust(p, method = p.adjust.methods, n = length(p))
  # p.adjust.methods
  # c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
  #   "fdr", "none")
  
  ##Bartlett test
  output$Bartlett <- renderPrint({
    my_his_data<-Histo_data_type()[,c(input$HisDV,input$HisIV,input$Plotfacet_choice)]
    my_his_data[,2]<-as.factor(my_his_data[,2])
    
    if(input$plot_facet ==T){
      n_rows<-length(levels(my_his_data[,3]))
      facetting <-rep(NA,n_rows)
      pvalue_bartlett<-rep(NA,n_rows)
      interpret_bartlett<-rep(NA,n_rows)
      for (i in unique(my_his_data[,3])){
        facetting[i]<-i
        subsetted_data<- subset(my_his_data, my_his_data[,3]==i)
        fit_bartlett<-bartlett.test(subsetted_data[,1] ~ subsetted_data[,2], data=subsetted_data)
        #print(fit_bartlett)
        #model_bartlett<-fit_bartlett[[4]] #result of bartlett is a list with 4th element the description of model
        pvalue_bartlett[i]<-signif(fit_bartlett[[3]], 5) #result of bartlett is a list with 3rd element the p-value
        
        if (fit_bartlett[[3]] < as.numeric(as.character(input$Chosenthreshold)) ) {
          interpret_bartlett[i]<-"Not equal"
        } else {
          interpret_bartlett[i]<-"Equal"
        }
        
        temp_bartlett<-as.data.frame(cbind(facetting, pvalue_bartlett, interpret_bartlett))
      }
      temp_bartlett<-na.omit(temp_bartlett)
      colnames(temp_bartlett) <- c("", "p_value", paste("The variances between ", input$HisIV, " groups are:", sep=""))
      cat(paste("The p-value of the Bartlett test of homogeneity of variances between different ", input$HisIV, " for each ", input$Plotfacet_choice, " is:", "\n", "\n", sep=""))
      print(temp_bartlett, row.names=FALSE)
    }
    
    if(input$plot_facet ==F){ 
      
      fit_bartlett<-bartlett.test(my_his_data[,1] ~ my_his_data[,2], data=my_his_data)
      #print(fit_bartlett)
      #model_bartlett<-fit_bartlett[[4]] #result of bartlett is a list with 4th element the description of model
      pvalue_bartlett<-signif(fit_bartlett[[3]],5) #result of bartlett is a list with 3rd element the p-value
      cat("HOMOGENEITY OF VARIANCE ANALYSIS", "\n")
      cat("The p-value of the Bartlett test of homogeneity of variances between different ", input$HisIV, " is ", pvalue_bartlett, ".", "\n", sep="")
      
      if (pvalue_bartlett < as.numeric(as.character(input$Chosenthreshold) )) {
        cat("Based on your chosen p-value threshold, the variances between ", input$HisIV, " groups are EQUAL", sep="")
      } else {
        cat("Based on your chosen p-value threshold, the variances between ", input$HisIV, " groups are NOT equal.", sep="")
      }
    }
  })
  
  
  
  #######We need to correct for multiple testing p.adjust(p, method = p.adjust.methods, n = length(p))
  # p.adjust.methods
  # c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
  #   "fdr", "none")
  
  ##Levene test
  output$Levene <- renderPrint({
    my_his_data<-Histo_data_type()[,c(input$HisDV,input$HisIV,input$Plotfacet_choice)]
    my_his_data[,2]<-as.factor(my_his_data[,2])
    
    if(input$plot_facet ==T){
      n_rows<-length(levels(my_his_data[,3]))
      facetting <-rep(NA,n_rows)
      pvalue_levene<-rep(NA,n_rows)
      interpret_levene<-rep(NA,n_rows)
      for (i in unique(my_his_data[,3])){
        facetting[i]<-i
        subsetted_data<- subset(my_his_data, my_his_data[,3]==i)
        fit_levene<-leveneTest(subsetted_data[,1] ~ subsetted_data[,2], data=subsetted_data)
        
        pvalue_levene[i]<-signif(fit_levene[[3]][[1]], 5) #result of levene is a list with 1st element of 3rd element the p-value
        
        if (fit_levene[[3]][[1]] < as.numeric(as.character(input$Chosenthreshold)) ) {
          interpret_levene[i]<-"Not equal"
        } else {
          interpret_levene[i]<-"Equal"
        }
        
        temp_levene<-as.data.frame(cbind(facetting, pvalue_levene, interpret_levene))
      }
      temp_levene<-na.omit(temp_levene)
      colnames(temp_levene) <- c("", "p_value", paste("The variances between ", input$HisIV, " groups are:", sep=""))
      cat(paste("The p-value of the Levene test of homogeneity of variances between different ", input$HisIV, "S for each ", input$Plotfacet_choice, " is:", "\n", "\n", sep=""))
      print(temp_levene, row.names=FALSE)
    }
    
    if(input$plot_facet ==F){ 
      
      fit_levene<-leveneTest(my_his_data[,1] ~ my_his_data[,2], data=my_his_data)
      #print(fit_levene)
      pvalue_levene<-signif(fit_levene[[3]][[1]],5) #result of levene is a list with 1st element of 3rd element the p-value
      cat("HOMOGENEITY OF VARIANCE ANALYSIS", "\n")
      cat("The p-value of the Levene test of homogeneity of variances between different ", input$HisIV, "S is ", pvalue_levene, ".", "\n", sep="")
      
      if (pvalue_levene < as.numeric(as.character(input$Chosenthreshold))) {
        cat("Based on your chosen p-value threshold, the variances between ", input$HisIV, " groups are EQUAL.", sep="")
      } else {
        cat("Based on your chosen p-value threshold, the variances between ", input$HisIV, " groups are NOT equal.", sep="")
      }
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
          label = "Dataset to correlate:",
          choices = c("raw data", "missing values removed", "outliers removed"),
          multiple = F
        )
      )
  })
  
  cor_data_type <- eventReactive(input$cor_data, {
    if (input$cor_data == "raw data") {
      cor_data_type <- my_data()
    }
    if (input$cor_data == "missing values removed") {
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
          label = "Subset the data by:",
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
        label = "Use subset:",
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
  
  output$cor_table <- renderDataTable({
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
  
  output$cor_table_text <- renderPrint({
    if(input$cor_data_subset == F){
      cat(paste("The", input$corMethod, "correlation coefficients and p values of your data are:"))
    }
    else{
      cat(paste("The", input$corMethod, "correlation coefficients and p values for you subsetted data in",input$CorIV_sub, "-", input$CorIV_val, "are:"))
    }
  })
  
  output$cortable_button <- renderUI({
    if (is.null(ItemList())) {
      return()
    }
    else{
      downloadButton("cortable_download_button", label = "Download correlation table")
    }
  })
  
  output$cortable_download_button <- downloadHandler(
    filename = paste("Correlation table using ", input$corrplotMethod, " MVApp.csv"),
    content <- function(file) {
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
      write.csv(results, file)}
  )
  
  
  
  output$tricky_table <- renderPrint({
    
    if(input$cor_data_subset == F){
      cat("")
    }
    else{
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
  
  list <- unique(df[,input$CorIV_sub])
  
  df2 <- subset(df, df[,input$CorIV_sub] == list[1])
  
  res <-
    rcorr(as.matrix(df2[, beginCol:endCol]), type = input$corMethod)
  
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
  result <- result[1:3]
  
  
  for(i in 2:length(list)){
    df2 <- subset(df, df[,input$CorIV_sub] == list[i])
    
    res <-
      rcorr(as.matrix(df2[, beginCol:endCol]), type = input$corMethod)
    
    flattenCorrMatrix <- function(cormat, pmat) {
      ut <- upper.tri(cormat)
      data.frame(
        row = rownames(cormat)[row(cormat)[ut]],
        column = rownames(cormat)[col(cormat)[ut]],
        cor  = (cormat)[ut],
        p = pmat[ut]
      )
    }
    
    result3 <- flattenCorrMatrix(res$r, res$P)
    res2 <- result3[3]
    result <- cbind(result, res2)
  }
  
  result <- as.data.frame(result)
  for(f in 1:nrow(result)){
    lista_g <- result[f,3]
    for(g in 4:ncol(result)){
      lista_g <- c(lista_g, result[f,g])
    }
    result$vaRiance[f] <- var(lista_g)   
  }
  
  maxymum <- tail(sort(result$vaRiance),5)
  cat(paste("The highest variability in correlation across ", input$CorIV_sub, " was observed for: "))
  cat("\n")
  
  for(a in 1:length(maxymum)){
    tijdelijk <- subset(result, result[,"vaRiance"] == maxymum[a])
    cor_a <- tijdelijk[,1]
    cor_b <- tijdelijk[,2]
    cat(paste(a,". ", cor_a, " and ", cor_b))
    cat("\n")
  }}
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
          label = "X-axis:",
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
          label = "Y-axis:",
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
    paste("The R square value is", signif(r2, 3))
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
    if(is.null(ItemList())){return()}
    else
      tagList(
        selectizeInput(
          inputId = "PCA_data",
          label = "Dataset for PCA:",
          choices = c("raw data", "missing values removed", "outliers removed"), multiple = F))
  })  
 
  PCA_data_type <- eventReactive(input$Go_PCAdata,{
    if(input$PCA_data == "raw data"){
      PCA_data_type <- my_data()
    }
    if(input$PCA_data == "missing values removed"){
      PCA_data_type <- my_data_nona()
    }
    if(input$PCA_data == "outliers removed"){
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
        label = "Phenotypes for the PCA",
        choices = c(input$SelectDV),
        multiple = T
      )
    )
  })
  output$PCA_subset_trait <- renderUI({
    if(input$PCA_data_subset == "full dataset"){
      return()
    }
    else{
      tagList(
        selectizeInput(
          inputId = "PCA_subset_T",
          label = "Independent Variables to subset the data",
          choices=c(input$SelectGeno, input$SelectIV, input$SelectTime),
          multiple=T
        ))}
  })
  
  lista_PCA <- eventReactive(input$PCA_subset_T,{
    subset_lista <- input$PCA_subset_T
    id_lista <- c(input$SelectGeno, input$SelectIV, input$SelectTime)
    id_lista2 <- setdiff(id_lista, subset_lista)
    temp <- PCA_data_type()
    temp$subset_id <- do.call(paste,c(temp[c(subset_lista)], sep="_"))
    the_list <- unique(temp$subset_id)
    the_list
  })
  
  output$PCA_subset_specific <- renderUI({
    if(is.null(input$PCA_subset_T)){
      return()
    }
    else{
      subset_lista <- input$PCA_subset_T
      id_lista <- c(input$SelectGeno, input$SelectIV, input$SelectTime)
      id_lista2 <- setdiff(id_lista, subset_lista)
      temp <- PCA_data_type()
      temp$subset_id <- do.call(paste,c(temp[c(subset_lista)], sep="_"))
      the_list <- unique(temp$subset_id)
      
      tagList(
        selectizeInput(
          inputId = "PCA_subset_S",
          label = "Select subset:",
          choices=c(the_list),
          multiple=F
        ))}
  })
  
  PCA_final_data <- eventReactive(input$Go_PCA,{
    temp <- data.frame(PCA_data_type())
    temp <- subset(temp, select=c(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID, input$PCA_pheno))
    
  if(input$PCA_data_subset == "subsetted dataset"){
    subset_lista <- input$PCA_subset_T
      id_lista <- c(input$SelectGeno, input$SelectIV, input$SelectID, input$SelectTime)
      id_lista2 <- setdiff(id_lista, subset_lista)
      temp$id <- do.call(paste,c(temp[c(id_lista2)], sep="_"))
      temp$sub_id <- do.call(paste,c(temp[c(subset_lista)], sep="_"))
      temp2 <- subset(temp, temp$sub_id == input$PCA_subset_S)
      temp2 <- subset(temp2, select = c("id", input$PCA_pheno))
      }
  if(input$PCA_data_subset == "full dataset"){
      temp$id <- do.call(paste,c(temp[c(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID)], sep="_"))
      temp2 <- subset(temp, select = c("id", input$PCA_pheno))
    }
    
  return(temp2)
  })
  
  output$PCA_final_table <- renderDataTable({
    PCA_final_data()
  })
  
  PCA_eigen_data <- eventReactive(input$Go_PCA,{
    beginCol <-2
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
  lines(x = 1:nrow(eigenvalues), eigenvalues[, 2], 
          type="b", pch=19, col = "red")
  })
  
  output$Eigen_download_button <- renderUI({
    if(is.null(PCA_eigen_data())){
      return()}
    else
      downloadButton("Eigen_data", label="Download Eigen values")
  })
  
  output$Eigen_data <- downloadHandler(
    filename = "Eigen values MVApp.csv",
    content <- function(file) {
      write.csv(PCA_eigen_data(), file)}
  )
  
  output$Eigen_data_table <- renderDataTable({
    PCA_eigen_data()
  })
  
  
  output$PCA1_select <- renderUI({
    if ((input$Go_PCAdata == FALSE)) {
      return()
    } else
      eigenvalues <- PCA_eigen_data()
    list_avail_PCs <- unique(1:(nrow(eigenvalues)))
    tagList(
      selectizeInput(
        inputId = "Which_PC1",
        label = "PC x-axis",
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
    list_avail_PCs <- unique(2:(nrow(eigenvalues)))
    tagList(
      selectizeInput(
        inputId = "Which_PC2",
        label = "PC y-axis",
        choices = list_avail_PCs,
        multiple = F
      )
    )
  })
  
  output$PCA_contribution_plot <- renderPlot({
    beginCol <-
      length(c(
        input$SelectIV,
        input$SelectGeno,
        input$SelectTime,
        input$SelectID
      )) 
    endCol <-ncol(PCA_final_data())
    PCA_ready <- PCA_final_data()
    PCA_ready <- PCA_ready[, beginCol : endCol]
    res.pca <- PCA(PCA_ready, graph = FALSE)
    mid=median(res.pca$var$contrib)
    fviz_pca_var(res.pca, axes = c(as.numeric(input$Which_PC1),as.numeric(input$Which_PC2)), col.var="contrib", geom ="auto", labelsize = 4, repel=T, label="var", addlabels=T, invisible = "none") +
      scale_color_gradient2(low="grey", mid="purple", 
                            high="red", midpoint=mid)+theme_bw()
  })
  
  output$PCA_contrib_select <- renderUI({
    if ((input$Go_PCAdata == FALSE)) {
      return()
    } else
      eigenvalues <- PCA_eigen_data()
    list_avail_PCs <- unique(1:(nrow(eigenvalues)))
    tagList(
      selectizeInput(
        inputId = "Which_PC_contrib",
        label = "PC to plot:",
        choices = list_avail_PCs,
        multiple = F
      )
    )
  })
  
  output$Contrib_trait_plot <- renderPlot({
        beginCol <-
        length(c(
        input$SelectIV,
        input$SelectGeno,
        input$SelectTime,
        input$SelectID
      )) 
    endCol <-ncol(PCA_final_data())
    PCA_ready <- PCA_final_data()
    PCA_ready <- PCA_ready[, beginCol : endCol]
    res.pca <- PCA(PCA_ready, graph = FALSE)
    fviz_contrib(res.pca, choice = 'var', axes = c(as.numeric(input$Which_PC_contrib)), xtickslab.rt = 90)
  })

  PCA_contrib_var <- eventReactive(input$Go_PCA,{
    beginCol <-2
    endCol <-ncol(PCA_final_data())
    PCA_ready <- PCA_final_data()
    PCA_ready <- PCA_ready[, beginCol : endCol]
    res.pca <- PCA(PCA_ready, graph = FALSE)
    #for_labels <- PCA_final_data()
    #for_labels <- for_labels[1:beginCol-1]
    #new_stuff <- cbind(for_labels, res.pca$ind$contrib)
    #plot(input$Which_PC1 ~ input$Which_PC2, data = new_stuff, fill = input$SelectIV)
    #color <- input$SelectIV
    contrib_var <- res.pca$var$contrib
    contrib_var
  })
  
  output$PCA_contribution_var <- renderDataTable({
    PCA_contrib_var()
  })
  
  output$Contrib_download_var <- renderUI({
    if(is.null(PCA_final_data())){
      return()}
    else
      downloadButton("contrib_var", label="Download PCA contribution by variable")
  })
  
  output$contrib_var <- downloadHandler(
    filename = "PCA contrib var MVApp.csv",
    content <- function(file) {
      write.csv(PCA_final_data(), file)}
  )
  
  output$PCA_colorby <- renderUI({
    if(is.null(PCA_final_data())){
      return()}
    else
      tagList(
        selectizeInput(
          inputId = "PCA_Color",
          label = "Color by:",
          choices = c(input$SelectIV, input$SelectGeno, input$SelectTime),
          multiple = F
        )
      )
  })
  
  
  
  PCA_coord_ind <- eventReactive(input$Go_PCA,{
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
    
    ###### ADDING REFERENCE DATA HERE #######
    temp <- data.frame(PCA_data_type())
    temp <- subset(temp, select=c(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID, input$PCA_pheno))
    
    if(input$PCA_data_subset == "subsetted dataset"){
      subset_lista <- input$PCA_subset_T}
    if(input$PCA_data_subset == "full dataset"){{
      temp$id <- do.call(paste,c(temp[c(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID)], sep="_"))
      temp2 <- temp
    }}
    
    ##### END REFERENCE DATA HERE ######   
    temp3 <- subset(temp2, select = c(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID))
    
    temporary <- res.pca$ind$coord
    
    Le_table <- cbind(temp3, temporary)
    names(Le_table) <- gsub("Dim.", "", names(Le_table), fixed=T)
    Le_table
  })
  
  output$PCA_coordinates_ind <- renderDataTable({
    PCA_coord_ind()
  })
  
  output$Coord_download_ind <- renderUI({
    if(is.null(PCA_coord_ind())){
      return()}
    else
      downloadButton("coord_ind", label="Download individual PCA coordinates")
  })
  
  output$coord_ind <- downloadHandler(
    filename = "PCA coord ind MVApp.csv",
    content <- function(file) {
      write.csv(PCA_coord_ind(), file)}
  )
  
  output$PCA_scatterplot <- renderPlotly({
    Le_table <- PCA_coord_ind()
    Le_table$x_axis <- Le_table[,input$Which_PC1]
    Le_table$y_axis <- Le_table[,input$Which_PC2]
    Le_table$color <- Le_table[,input$PCA_Color]
    super_plot <- ggplot(data = Le_table, aes(x = x_axis, y= y_axis, colour = color))
    super_plot <- super_plot + geom_point()
    super_plot
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # - - - - - - - - - - - - >> CLUSTER ANALYSIS IN 8th TAB << - - - - - - - - - - -
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # =  = = = = = = = = >> INPUT GADGETS << = = = = = = = = = = = = = 
  output$Select_data_cluster <- renderUI({
    if(is.null(ItemList())){return()}
    else
      tagList(
        selectizeInput(
          inputId = "Cluster_data",
          label = "Dataset for clustering analysis",
          choices = c("raw data", "missing values removed", "outliers removed"), multiple = F))
  })
  
  
  output$Select_clustering_method <- renderUI({
    if(is.null(ItemList())){return()}
    else
      tagList(
        selectizeInput(
          inputId = "Cluster_method",
          label = "Clustering method:",
          choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"), multiple = F))
  })   
  
  output$Select_data_cluster_validation <- renderUI({
    if(is.null(input$Split_cluster)){
      return()
    }
    else{
      tagList(
        selectizeInput(
          "Clust_test",
          label = "View:",
          choices = input$SelectDV
        )
      )
    }
  })
  
  Data_for_cluster <- eventReactive(exists(input$Cluster_data),{
    if(input$Cluster_data == "raw data"){
      cluster_data <- my_data()
    }  
    if(input$Cluster_data == "missing values removed"){
      cluster_data <- my_data_nona()
    }
    if(input$Cluster_data == "outliers removed"){
      cluster_data <- Outlier_free_data()
    }
    return(cluster_data)
  })
  
  output$Data_cluster_table <- renderDataTable({
    Data_for_cluster()
  })
  
  output$Select_phenotypes_cluster <- renderUI({
    if(is.null(Data_for_cluster())){return()}
    else
    {
      tagList(
        selectizeInput(
          inputId = "Cluster_pheno", 
          label = "Phenotypes to include in clustering analysis:",
          choices=c(input$SelectDV),
          multiple = T))}
  })
  
  output$Cluster_subset_trait <- renderUI({
    if(input$Cluster_subset_Q == F){
      return()
    }
    else{
      tagList(
        selectizeInput(
          inputId = "Cluster_subset_T",
          label = "Subset by Independent Variables:",
          choices=c(input$SelectGeno, input$SelectIV, input$SelectTime),
          multiple=T
        ))}
  })
  
  lista_cluster <- eventReactive(input$Cluster_subset_t,{
    subset_lista <- input$Cluster_subset_T
    id_lista <- c(input$SelectGeno, input$SelectIV, input$SelectTime)
    id_lista2 <- setdiff(id_lista, subset_lista)
    temp <- Data_for_cluster()
    temp$subset_id <- do.call(paste,c(temp[c(subset_lista)], sep="_"))
    the_list <- unique(temp$subset_id)
    the_list
  })
  
  output$Cluster_subset_specific <- renderUI({
    if(is.null(input$Cluster_subset_T)){
      return()
    }
    else{
      subset_lista <- input$Cluster_subset_T
      id_lista <- c(input$SelectGeno, input$SelectIV, input$SelectTime)
      id_lista2 <- setdiff(id_lista, subset_lista)
      temp <- Data_for_cluster()
      temp$subset_id <- do.call(paste,c(temp[c(subset_lista)], sep="_"))
      the_list <- unique(temp$subset_id)
      
      tagList(
        selectizeInput(
          inputId = "Cluster_subset_S",
          label = "Subset:",
          choices=c(the_list),
          multiple=F
        ))}
  })
  
  # = = = = = = = = = = = >> MAIN CALCULATIONS AND TABLES << = = = = = = = = = = = = = = = 
  
  Final_data_cluster <- eventReactive(input$Go_cluster,{
    temp <- Data_for_cluster()
    if(input$Cluster_subset_Q == T){
      subset_lista <- input$Cluster_subset_T
      if(input$Cluster_pre_calc == F){
        id_lista <- c(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID)
        id_lista2 <- setdiff(id_lista, subset_lista)
        temp$id <- do.call(paste,c(temp[c(id_lista2)], sep="_"))
        temp$sub_id <- do.call(paste,c(temp[c(subset_lista)], sep="_"))
        temp2 <- subset(temp, temp$sub_id == input$Cluster_subset_S)
        temp2 <- subset(temp2, select = c("id", input$Cluster_pheno))}
      if(input$Cluster_pre_calc == T){
        id_lista <- c(input$SelectGeno, input$SelectIV, input$SelectTime)
        id_lista2 <- setdiff(id_lista, subset_lista)
        temp$id <- do.call(paste,c(temp[c(id_lista2)], sep="_"))
        temp$sub_id <- do.call(paste,c(temp[c(subset_lista)], sep="_"))
        temp <- subset(temp, temp$sub_id == input$Cluster_subset_S)
        temp2 <- subset(temp, select = c("id", input$Cluster_pheno))
        temp2 <- summaryBy(.~ id, data=temp2)}
    }
    if(input$Cluster_subset_Q == F){
      if(input$Cluster_pre_calc == F){
        temp$id <- do.call(paste,c(temp[c(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID)], sep="_"))
        temp2 <- subset(temp, select = c("id", input$Cluster_pheno))
      }
      if(input$Cluster_pre_calc == T){
        temp$id <- do.call(paste,c(temp[c(input$SelectGeno, input$SelectIV, input$SelectTime)], sep="_"))
        temp2 <- subset(temp, select = c("id", input$Cluster_pheno))
        temp2 <- summaryBy(.~ id, data=temp2)  
      }}
    return(temp2)
  })
  
  output$Final_cluster_table <- renderDataTable({
    Final_data_cluster()
  })
  
  # = = = = = = = = = >> OUTPUT PLOTS AND SENTENCES << = = = = = = = = = = = = = = 
  
  output$ClusterTree <- renderPlot({
    clust_temp <- Final_data_cluster()
    clust_temp <- na.omit(clust_temp)
    clust_matrix <- clust_temp[,2:ncol(clust_temp)]
    row.names(clust_matrix) <- clust_temp$id
    clust_matrix = as.matrix(clust_matrix)
    clust_t_matrix = t(clust_matrix)
    clust_t_cor = cor(clust_t_matrix,method="pearson")
    clust_t_dist = dist(clust_t_cor)
    clust_t_clust = hclust(clust_t_dist, method=input$Cluster_method)
    
    plot(as.dendrogram(clust_t_clust), horiz=T)
    
  })
  
  output$HotHeatMap <- renderPlot({
    clust_temp <- Final_data_cluster()
    clust_temp <- na.omit(clust_temp)
    clust_matrix <- clust_temp[,2:ncol(clust_temp)]
    row.names(clust_matrix) <- clust_temp$id
    clust_matrix = as.matrix(clust_matrix)
    clust_t_matrix = t(clust_matrix)
    clust_t_cor = cor(clust_t_matrix,method="pearson")
    clust_t_dist = dist(clust_t_cor)
    clust_t_clust = hclust(clust_t_dist, method=input$Cluster_method)
    heatmap.2(clust_t_matrix, Colv=as.dendrogram(clust_t_clust), col=blue2red(100),scale=c("row"),density.info="none",trace="none", cexRow=0.7)
  })
  
  output$Dendro_sentence <- renderPrint({
    if(is.null(Cluster_table_data())){
      cat("NUMERIC value at which to cut the dendrogram to segregate the samples into separate clusters:")
    }
    else{
      clust_temp <- Final_data_cluster()
      clust_temp <- na.omit(clust_temp)
      clust_matrix <- clust_temp[,2:ncol(clust_temp)]
      row.names(clust_matrix) <- clust_temp$id
      clust_matrix = as.matrix(clust_matrix)
      clust_t_matrix = t(clust_matrix)
      clust_t_cor = cor(clust_t_matrix,method="pearson")
      clust_t_dist = dist(clust_t_cor)
      clust_t_clust = hclust(clust_t_dist, method=input$Cluster_method)
      cluster <- as.data.frame(cutree(clust_t_clust,h=as.numeric(input$Split_cluster)))
      names(cluster)[1] <- "cluster"
      clust_number <- length(unique(cluster$cluster))
      
      cat("Cutting the dengrodram at ", input$Split_cluster, " will result in ", clust_number, " clusters.")
      cat("\n")
      cat("Please be aware that clustering your data into too many clusters might not be informative.")
      
    }
  })

  Cluster_table_data <- eventReactive(input$Split_cluster,{
    # perform clustering on the selected dataset  
    clust_temp <- Final_data_cluster()
    clust_temp <- na.omit(clust_temp)
    clust_matrix <- clust_temp[,2:ncol(clust_temp)]
    row.names(clust_matrix) <- clust_temp$id
    clust_matrix = as.matrix(clust_matrix)
    clust_t_matrix = t(clust_matrix)
    clust_t_cor = cor(clust_t_matrix,method="pearson")
    clust_t_dist = dist(clust_t_cor)
    clust_t_clust = hclust(clust_t_dist, method=input$Cluster_method)
    
    cluster <- as.data.frame(cutree(clust_t_clust,h=as.numeric(input$Split_cluster)))
    names(cluster)[1] <- "cluster"
    
    # import the data containing ALL the phenotypes
    
    temp <- Data_for_cluster()
    
    # check whether you want to subset your data - IF YES 
    if(input$Cluster_subset_Q == T){
      # Subset data by this trait:
      subset_lista <- input$Cluster_subset_T
      # Check if you want to summarize your data - if no then:
      if(input$Cluster_pre_calc == F){
        # list of all possible identifiers
        id_lista <- c(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID)
        # extract which id is being used for subsetting
        id_lista2 <- setdiff(id_lista, subset_lista)
        # make one column with ids which are NOT used for subsetting
        temp$id <- do.call(paste,c(temp[c(id_lista2)], sep="_"))
        # make column with ONLY the ids being used for subseting
        temp$sub_id <- temp[,input$Cluster_subset_T]
        # subset for whatever value is chosen
        temp2 <- subset(temp, temp$sub_id == input$Cluster_subset_S)
        #temp2 <- subset(temp, select != c("sub_id", input$SelectIV, input$SelectTime, input$SelectID))
      }
      if(input$Cluster_pre_calc == T){
        id_lista <- c(input$SelectGeno, input$SelectIV, input$SelectTime)
        id_lista2 <- setdiff(id_lista, subset_lista)
        temp$id <- do.call(paste,c(temp[c(id_lista2)], sep="_"))
        temp$sub_id <- do.call(paste,c(temp[c(subset_lista)], sep="_"))
        temp2 <- subset(temp, temp$sub_id == input$Cluster_subset_S)
        #temp2 <- subset(temp, select != c("sub_id", input$SelectIV, input$SelectTime, input$SelectID))
        temp2 <- summaryBy(.~ id, data=temp2)
        #temp2 <- cSplit(temp2, "id", "_")
      }
    }
    if(input$Cluster_subset_Q == F){
      if(input$Cluster_pre_calc == F){
        temp$id <- do.call(paste,c(temp[c(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID)], sep="_"))
        #temp2 <- subset(temp, select = c("id", input$Cluster_pheno))
        temp2 <- temp
      }
      if(input$Cluster_pre_calc == T){
        temp$id <- do.call(paste,c(temp[c(input$SelectGeno, input$SelectIV, input$SelectTime)], sep="_"))
        # temp2 <- subset(temp, select = c("id", input$Cluster_pheno))
        temp2 <- summaryBy(.~ id, data=temp)
        #temp2 <- cSplit(temp2, "id", "_")
      }}
    
    row.names(temp2) <- temp2$id
    
    new_shait <- merge(cluster, temp2, by="row.names")
    return(new_shait)
  })
  
 
  output$Cluster_table <- renderDataTable({
  Cluster_table_data()
  })
  
  output$HotAnovaNews <- renderPrint({
    
    clust_temp <- Final_data_cluster()
    clust_temp <- na.omit(clust_temp)
    clust_matrix <- clust_temp[,2:ncol(clust_temp)]
    row.names(clust_matrix) <- clust_temp$id
    clust_matrix = as.matrix(clust_matrix)
    clust_t_matrix = t(clust_matrix)
    clust_t_cor = cor(clust_t_matrix,method="pearson")
    clust_t_dist = dist(clust_t_cor)
    clust_t_clust = hclust(clust_t_dist, method=input$Cluster_method)
    
    cluster <- as.data.frame(cutree(clust_t_clust,h=as.numeric(input$Split_cluster)))
    names(cluster)[1] <- "cluster"
    
    # import the data containing ALL the phenotypes
    
    temp <- Data_for_cluster()
    
    # check whether you want to subset your data - IF YES 
    if(input$Cluster_subset_Q == T){
      # Subset data by this trait:
      subset_lista <- input$Cluster_subset_T
      # Check if you want to summarize your data - if no then:
      if(input$Cluster_pre_calc == F){
        # list of all possible identifiers
        id_lista <- c(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID)
        # extract which id is being used for subsetting
        id_lista2 <- setdiff(id_lista, subset_lista)
        # make one column with ids which are NOT used for subsetting
        temp$id <- do.call(paste,c(temp[c(id_lista2)], sep="_"))
        # make column with ONLY the ids being used for subseting
        temp$sub_id <- temp[,input$Cluster_subset_T]
        # subset for whatever value is chosen
        temp2 <- subset(temp, temp$sub_id == input$Cluster_subset_S)
        #temp2 <- subset(temp, select != c("sub_id", input$SelectIV, input$SelectTime, input$SelectID))
      }
      if(input$Cluster_pre_calc == T){
        id_lista <- c(input$SelectGeno, input$SelectIV, input$SelectTime)
        id_lista2 <- setdiff(id_lista, subset_lista)
        temp$id <- do.call(paste,c(temp[c(id_lista2)], sep="_"))
        temp$sub_id <- do.call(paste,c(temp[c(subset_lista)], sep="_"))
        temp2 <- subset(temp, temp$sub_id == input$Cluster_subset_S)
        #temp2 <- subset(temp, select != c("sub_id", input$SelectIV, input$SelectTime, input$SelectID))
        temp2 <- summaryBy(.~ id, data=temp2)
        names(temp2) <- gsub(".mean", "", names(temp2), fixed=T)
        #temp2 <- cSplit(temp2, "id", "_")
      }
    }
    if(input$Cluster_subset_Q == F){
      if(input$Cluster_pre_calc == F){
        temp$id <- do.call(paste,c(temp[c(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID)], sep="_"))
        #temp2 <- subset(temp, select = c("id", input$Cluster_pheno))
        temp2 <- temp
      }
      if(input$Cluster_pre_calc == T){
        temp$id <- do.call(paste,c(temp[c(input$SelectGeno, input$SelectIV, input$SelectTime)], sep="_"))
        # temp2 <- subset(temp, select = c("id", input$Cluster_pheno))
        temp2 <- summaryBy(.~ id, data=temp)
        names(temp2) <- gsub(".mean", "", names(temp2), fixed=T)
        #temp2 <- cSplit(temp2, "id", "_")
      }}
    
    row.names(temp2) <- temp2$id
    
    new_shait <- merge(cluster, temp2, by="row.names")
    
    sig_listxxx <- " "
    
    
    for(i in 1:length(input$SelectDV)){
      to_test <- new_shait[,c("cluster",input$SelectDV[i])]
      names(to_test)[2] <- "phenotype"
      to_test$cluster <- as.factor(to_test$cluster)
      amod <- aov(phenotype ~ cluster, data = to_test)
      
      if(summary(amod)[[1]][["Pr(>F)"]] < 0.05){
        significantna_lista <- input$SelectDV[i]
      } 
      sig_listxxx <- c(sig_listxxx, significantna_lista)
    }
    
    lista_cudow <- unique(sig_listxxx)
    #sentence <- paste("significant effect of clustering was observed for ", lista_cudow, <font color=\"#FF0000\"><b>)
    
    cat("Significant effect of clusters observed for:")
    cat("\n")
    cat(lista_cudow)
    
  })
  
  
  output$HotANOVA <- renderPlot({
    clust_temp <- Final_data_cluster()
    clust_temp <- na.omit(clust_temp)
    clust_matrix <- clust_temp[,2:ncol(clust_temp)]
    row.names(clust_matrix) <- clust_temp$id
    clust_matrix = as.matrix(clust_matrix)
    clust_t_matrix = t(clust_matrix)
    clust_t_cor = cor(clust_t_matrix,method="pearson")
    clust_t_dist = dist(clust_t_cor)
    clust_t_clust = hclust(clust_t_dist, method=input$Cluster_method)
    
    cluster <- as.data.frame(cutree(clust_t_clust,h=as.numeric(input$Split_cluster)))
    names(cluster)[1] <- "cluster"
    
    # import the data containing ALL the phenotypes
    
    temp <- Data_for_cluster()
    
    # check whether you want to subset your data - IF YES 
    if(input$Cluster_subset_Q == T){
      # Subset data by this trait:
      subset_lista <- input$Cluster_subset_T
      # Check if you want to summarize your data - if no then:
      if(input$Cluster_pre_calc == F){
        # list of all possible identifiers
        id_lista <- c(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID)
        # extract which id is being used for subsetting
        id_lista2 <- setdiff(id_lista, subset_lista)
        # make one column with ids which are NOT used for subsetting
        temp$id <- do.call(paste,c(temp[c(id_lista2)], sep="_"))
        # make column with ONLY the ids being used for subseting
        temp$sub_id <- temp[,input$Cluster_subset_T]
        # subset for whatever value is chosen
        temp2 <- subset(temp, temp$sub_id == input$Cluster_subset_S)
        #temp2 <- subset(temp, select != c("sub_id", input$SelectIV, input$SelectTime, input$SelectID))
      }
      if(input$Cluster_pre_calc == T){
        id_lista <- c(input$SelectGeno, input$SelectIV, input$SelectTime)
        id_lista2 <- setdiff(id_lista, subset_lista)
        temp$id <- do.call(paste,c(temp[c(id_lista2)], sep="_"))
        temp$sub_id <- do.call(paste,c(temp[c(subset_lista)], sep="_"))
        temp2 <- subset(temp, temp$sub_id == input$Cluster_subset_S)
        #temp2 <- subset(temp, select != c("sub_id", input$SelectIV, input$SelectTime, input$SelectID))
        temp2 <- summaryBy(.~ id, data=temp2)
        names(temp2) <- gsub(".mean", "", names(temp2))
        #temp2 <- cSplit(temp2, "id", "_")
      }
    }
    if(input$Cluster_subset_Q == F){
      if(input$Cluster_pre_calc == F){
        temp$id <- do.call(paste,c(temp[c(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID)], sep="_"))
        #temp2 <- subset(temp, select = c("id", input$Cluster_pheno))
        temp2 <- temp
      }
      if(input$Cluster_pre_calc == T){
        temp$id <- do.call(paste,c(temp[c(input$SelectGeno, input$SelectIV, input$SelectTime)], sep="_"))
        # temp2 <- subset(temp, select = c("id", input$Cluster_pheno))
        temp2 <- summaryBy(.~ id, data=temp)
        names(temp2) <- gsub(".mean", "", names(temp2))
        #temp2 <- cSplit(temp2, "id", "_")
      }}
    
    row.names(temp2) <- temp2$id
    
    new_shait <- merge(cluster, temp2, by="row.names")
    
    to_test <- new_shait[,c("cluster",input$Clust_test)]
    names(to_test)[2] <- "phenotype"
    to_test$cluster <- as.factor(to_test$cluster)
    amod <- aov(phenotype ~ cluster, data = to_test)
    tuk <- glht(amod, linfct = mcp(cluster = "Tukey"))
    tuk.cld <- cld(tuk)   
    old.par <- par( mai=c(1,1,1.25,1))
    shaka_laka <- plot(tuk.cld, las=1, col=topo.colors(n=length(unique(to_test$cluster))), ylab=input$Clust_test)
    shaka_laka
  })
  
  
  output$Cluster_download_button <- renderUI({
    if(is.null(Cluster_table_data())){
      return()}
    else
      downloadButton("data_clustered", label="Download Cluster data")
  })
  
  output$data_clustered <- downloadHandler(
    filename = paste("Cluster analysis based on ", input$Cluster_pheno, " with split at ", input$Split_cluster, " MVApp.csv"),
    content <- function(file) {
      
      write.csv(Cluster_table_data(), file)}
  )
    
  # end of the script
}