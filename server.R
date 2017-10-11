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
    colnames(things_to_model)[4] <- "DELTA"
    colnames(things_to_model)[5] <- "INTERCEPT"
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
  data_model_plot <- eventReactive(input$Model_graph_fit_select,{
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
    })
  
  # Adding data to the existing table
  # this is not working, as I am adding the data to something that doesnt exist. 
  Saved_model_data <- eventReactive(input$Go_Model,{
      saved <- Model_temp_data()
      saved <- subset(saved, select=c(1:4))
      saved[,input$SelectTime] <- "DELTA"
      melted_model <- melt(saved, id=c(input$SelectGeno, input$SelectIV, input$SelectID, input$SelectTime))
      melted_model$variable <- input$ModelPheno
      melted_model
      })
  
  
  output$Model_download_button <- renderUI({
      if(is.null(Saved_model_data())){
        return()}
      else
        downloadButton("Download_model_data", label="Download Fitted data")
    })  
    
  output$Download_model_data <- downloadHandler(
    filename = paste("Modelled_",input$ModelPheno, "_with_", input$model ,"_MVApp.csv"),
    content <- function(file) {
      write.csv(Model_temp_data(), file)}
  )
  
  # Fusing the model data to the data that can be used for Summary Stats
  Full_set_from_modeling <- eventReactive(input$Go_SaveModelData,{
    melted_icecream <- melt(my_data(), id=c(input$SelectGeno, input$SelectIV, input$SelectID, input$SelectTime))
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
    if(is.null(ItemList())){return()}
    else
    tagList(
      selectizeInput("IV_outliers",
                     label = "Select the Indepentent Variables for which you would like to group yor phenotypes for outlier selection",
                     choices=c(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID),
                     multiple=TRUE)
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
      sliderInput("outlier_cutoff", "Remove the samples which are characterized as an outlier in at least x traits:", min_out, max_out, value = 2, step = 1)  
      }
    })
    
  # Chose the phenotype based on which you want to select the outliers
  
  output$Pheno_outliers <- renderUI({
    if(is.null(ItemList()) | (input$Out_pheno_single_multi == "All phenotypes")){return()}
    else
      tagList(
        selectizeInput("DV_outliers",
                       label = "Select the phenotype for which you would like to determine the outliers",
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
                       label = "Select the phenotype for which you would like to examine graphically",
                       choices= input$SelectDV,
                       selected = input$SelectDV[1],
                       multiple=F)
      )})
  
  
  output$Q_facet <- renderUI({
    if(input$outlier_facet == T){
      tagList(
        selectInput("Facet_choice", "Select variable for which to facet",
                    choices = c(input$SelectGeno, input$SelectIV, input$SelectTime))
      )
    }
    else{
      return()
    }
  })
  
  # - - - - - - - - - - - - - >>  MAIN CALCULATIONS << - - - - - - - - - - - - - -
  
  # General outlier testing table => highlighting the plants with problems in multiple traits:
  
  Outlier_overview <- eventReactive(input$Go_outliers,{
    faka_boom <- my_data()
    faka_boom$id_test <- do.call(paste,c(faka_boom[c(input$IV_outliers)], sep = "_"))

    for(i in 1:length(input$SelectDV)){
      
      if(input$outlier_method == "1.5*IQR away from the mean"){
        
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
  
        data_outl <- my_data()
        outl <- subset(data_outl, select=c(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID, input$DV_outliers))
        outl$id_test <- do.call(paste,c(outl[c(input$IV_outliers)], sep = "_"))
        
        outl$pheno <- outl[,input$DV_outliers]
        
    # outliers based on 1.5IQR
    if(input$outlier_method == "1.5*IQR away from the mean"){
       
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
          
     return(outl)
        
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
  # NOT YET DONE! WOULD BE NICE IF WE CAN FORMAT THIS TABLE BUT I DONOT KNOW YET HOW???
  output$Outlier_overview_table <- DT::renderDataTable({
    test <- Outliers_final_data()
    datatable(test)%>% formatStyle(
      "Add_outliers",
      target = 'row',
      backgroundColor = styleInterval((input$outlier_cutoff-1), c("white", "pink"))
    )
  })
  
  
  # OUTLIER FREE
  Outlier_free_data <- eventReactive(input$Go_outliers,{
    if(input$Out_pheno_single_multi == "All phenotypes"){
      good_shit <- Outliers_final_data()
      good_shit2 <- subset(good_shit, good_shit$Add_outliers < input$outlier_cutoff)
    }
    if(input$Out_pheno_single_multi == "Single phenotype"){
    good_shit <-  Outliers_final_data()
    good_shit2 <- subset(good_shit, good_shit$outlier == FALSE)
    }
    return(good_shit2)
  })
  
  # OUTLIER ONLY
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
    filename = paste("Marked_outliers_based_on_",input$Out_pheno_single_multi,"_", input$DV_outliers ,"_identified_with_", input$outlier_method, "_MVApp.csv"),
    content <- function(file) {
      write.csv(Outliers_final_data(), file)}
  )
  
  
   # Download table with the outliers:
  
  output$Pheno_outlier_download <- renderUI({
    if(is.null(Outliers_final_data())){
      return()}
    else{
      downloadButton("data_outliers", label="Download table containing the outlier values")}
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
    filename = paste("Data_free_from_outliers_based_on",input$Out_pheno_single_multi,"_", input$DV_outliers ,"_identified_with_", input$outlier_method, "_MVApp.csv"),
    content <- function(file) {
      write.csv(Outlier_free_data(), file)}
  )
  
  
  
  # = = = >> GRAPH CONTAINING ALL THE DATA << = = = 
  output$outlier_graph <- renderPlotly({
    data_outl <- my_data()
    outl <- subset(data_outl, select=c(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID, input$DV_graph_outliers))
    lista <- input$IV_outliers
    
    if(input$outlier_facet == T){
    listb <- input$Facet_choice   ## Is this saving data? Or just the label?
    outl$listb <- outl[,input$Facet_choice]    ## WHY? Essentially cloning the facet grouping factor column... Why not add 5 lines up when subseting)
    lista <- setdiff(lista, listb)}
    phenotype <- input$DV_graph_outliers
    outl$pheno <- outl[,input$DV_graph_outliers]
    
    # listc <- c(lista, input$SelectID)
    
    outl$id_test <- do.call(paste,c(outl[lista], sep = "_"))
    # outl$id_test2 <- do.call(paste,c(outl[listc], sep = "_"))
    
    if(input$outlier_graph_type == "bar plot"){
      outl$pheno <- as.numeric(outl$pheno)
      if(input$outlier_facet == T){
      out_sum <- summaryBy(pheno ~ id_test + listb, data = outl, FUN = function(x) { c(m = mean(x), s = sd(x), se = std.error(x)) })  
      }
      else{
      out_sum <- summaryBy(pheno ~ id_test, data = outl, FUN = function(x) { c(m = mean(x), s = sd(x), se = std.error(x)) })
      }
      taka <- ggplot(out_sum, aes(x = id_test, y= pheno.m))
      taka <- taka + geom_bar(stat="identity")
      taka <- taka + geom_errorbar(aes(ymin=pheno.m-pheno.se, ymax=pheno.m+pheno.se))
    }
    
    
    if(input$outlier_graph_type == "box plot"){
      taka <- ggplot(outl, aes(x = id_test, y= pheno))    
      taka <- taka + geom_boxplot()}
    
    if(input$outlier_graph_type == "violin plot"){
      taka <- ggplot(outl, aes(x = id_test, y= pheno))    
      taka <- taka + geom_violin(trim=FALSE)}
    
    if(input$outlier_graph_type == "scatter plot"){
      taka <- ggplot(outl, aes(x = id_test, y= pheno))
      taka <- taka + geom_point()}
    
    if(input$outlier_facet == T){
    taka <- taka + facet_wrap(~listb, ncol=3)}
    
    taka <- taka + theme(axis.title.x=element_blank(),
                           axis.text.x = element_text(angle = 90, hjust = 1),
                         axis.title.y = element_text(input$DV_graph_outliers))
    
    
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
      phenotype <- input$DV_graph_outliers
      clean_data$pheno <- clean_data[,input$DV_graph_outliers]
      
      clean_data$id_test <- do.call(paste,c(clean_data[lista], sep = "_"))
      
      if(input$outlier_graph_type == "bar plot"){
        clean_data$pheno <- as.numeric(clean_data$pheno)
        if(input$outlier_facet == T){
          clean_sum <- summaryBy(pheno ~ id_test + listb, data = clean_data, FUN = function(x) { c(m = mean(x), s = sd(x), se = std.error(x)) })  
        }
        else{
          clean_sum <- summaryBy(pheno ~ id_test, data = clean_data, FUN = function(x) { c(m = mean(x), s = sd(x), se = std.error(x)) })
        }
        jaka <- ggplot(clean_sum, aes(x = id_test, y= pheno.m))
        jaka <- jaka + geom_bar(stat="identity")
        jaka <- jaka + geom_errorbar(aes(ymin=pheno.m-pheno.se, ymax=pheno.m+pheno.se))
      }
      
      
      if(input$outlier_graph_type == "box plot"){
        jaka <- ggplot(clean_data, aes(x = id_test, y= pheno))    
        jaka <- jaka + geom_boxplot()}
      
      if(input$outlier_graph_type == "violin plot"){
        jaka <- ggplot(clean_data, aes(x = id_test, y= pheno))    
        jaka <- jaka + geom_violin(trim=FALSE)}
      
      if(input$outlier_graph_type == "scatter plot"){
        jaka <- ggplot(clean_data, aes(x = id_test, y= pheno))
        jaka <- jaka + geom_point()}
      
      if(input$outlier_facet == T){
        jaka <- jaka + facet_wrap(~listb, ncol=3)}
      
      jaka <- jaka + theme(axis.title.x=element_blank(),
                           axis.text.x = element_text(angle = 90, hjust = 1),
                           axis.title.y = element_text(input$DV_graph_outliers))
      
      
      jaka
    })
  
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
  
  
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # - - - - - - - - - - - - >> DATA EXPLORATION IN 5th TAB << - - - - - - - - - - -
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  
  ## Added new input "$SelectSumm"  and output "$CustomSumm"  %% Mitch %%
  
  output$Data_for_SummaryStats <- renderUI({
    if(is.null(ItemList())){return ()
    } else tagList(
      selectizeInput(inputId = "SelectDataSumm",
                     label = "Select the dataset to be used for the summary stats",
                     choices= c("raw data", "modelled data", "outliers removed"), selected="raw data", multiple = F))
    })
  
  output$CustomSumm <- renderUI({
    if((is.null(ItemList()))){return ()
    } else tagList(
      selectizeInput(inputId = "SelectSumm", 
                     label = "Select desired summary statistics calculations to be performed", 
                     choices=c("Mean", "Median", "StdDev", "StdErr", "Min", "Max", "Sum"), multiple=T))
  })
  
  
  ## Added list of summary functions "summfuns"   %% Mitch %%
  summfuns<-list(Mean = function(x) mean(x),
                 Median = function(x) median(x),
                 StdDev = function(x) sd(x),
                 StdErr = function(x) std.error(x),
                 Min = function(x) min(x),
                 Max = function(x) max(x),
                 Sum = function(x) sum(x))
  
  sum_data <- eventReactive(input$Go_SummaryStat, {
    if(input$SelectDataSumm == "raw data"){
      melted_icecream <- my_data()
      drops <-input$SelectID
      melted_icecream <- melted_icecream[ , !(names(melted_icecream)%in% drops)]
      melted_icecream <- melt(melted_icecream, id=c(input$SelectGeno, input$SelectIV, input$SelectTime))
    }
    if(input$SelectDataSumm == "modelled data"){
      melted_icecream <- Full_set_from_modeling()
      drops <-input$SelectID
      melted_icecream <- melted_icecream[ , !(names(melted_icecream)%in% drops)]
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
  downloadButton("data_sum", label="Download Summary Stats data")
    })
  
  output$data_sum <- downloadHandler(
    filename = "Summary_stats_MVApp.csv",
   content <- function(file) {
        write.csv(sum_data(), file)}
  )
  
  

  
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
          choices = c(
            input$SelectDV
          ),
          multiple = F
        )
      )
  })
  
  
  output$Plot_facet <- renderUI({
    if(input$plot_facet == T){
      tagList(
        selectInput("Plotfacet_choice", "Select variable for which to facet",
                    choices = c(input$SelectGeno, input$SelectIV, input$SelectTime))
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
    my_his_data<-my_data()[,c(input$HisDV,input$HisIV,input$Plotfacet_choice)]
    groupIV<-input$HisIV
    
    if(input$plot_facet ==T){
      facetIV<-input$Plotfacet_choice
      my_his_data$facetIV<-my_his_data[,input$Plotfacet_choice]
      groupIV<-setdiff(groupIV, facetIV)}
    
      plotDV<-input$HisDV
      my_his_data$plotDV<-my_his_data[,input$HisDV]
      
      my_his_data$groupID<-do.call(paste, c(my_his_data[groupIV], sep="_"))
      
### These hashed out lines are trying to work in groupID (equivalent to Magda's id_test) which allows grouping by multiple IVs
### Should eventually replace lines 1219 - 1230      
            
#      if (input$HistType == "HistCount") {
#         fit <- ggplot(my_his_data, aes(x=groupID, y=plotDV)) + xlab(names(my_his_data$groupID)) + geom_histogram(size=0.6, alpha=0.3, col="black") 
#      }
#      if (input$HistType == "HistDensity" ) { 
#        fit <- ggplot(my_his_data, aes(x=groupID, y=plotDV)) + xlab(names(my_his_data$groupID)) + geom_density(alpha = 0.3)
#      }
      
  
    if (input$HistType == "HistCount") {
       fit <- ggplot(my_his_data, aes(x=my_his_data[,1], fill=my_his_data[,2])) + xlab(names(my_his_data[1])) + geom_histogram(size=0.6, alpha=0.3, col="black") 
    }
    if (input$HistType == "HistDensity" ) { 
      fit <- ggplot(my_his_data, aes(x=my_his_data[,1], fill=my_his_data[,2])) + xlab(names(my_his_data[1]))  + geom_density(alpha = 0.3)
    }
    
    if(input$plot_facet ==T){
      fit <- fit + facet_wrap(~facetIV, ncol=3)}
    ggplotly(fit)
    
     }) 
  
  

   
    ##STILL TO DO:
        #       try to do subset by multiple variables
    output$Boxes <- renderPlotly({
      hisdata2<-my_data()[,c(input$SelectGeno, input$HisDV,input$HisIV)]
      box_graph <- ggplot(hisdata2, aes(x=hisdata2[,3], y=hisdata2[,4])) + xlab(names(hisdata2[2])) + ylab(names(hisdata2[1])) + geom_boxplot()
      ggplotly(box_graph)
    })
    
    
    
  
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # - - - - - - - - - - - - >> DATA CORRELATION IN 6th TAB << - - - - - - - - - - -
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    
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
      if(is.null(ItemList())){return()}
      else
      tagList(
        selectizeInput(
          inputId = "PCA_data",
          label = "Select the dataset that you would like to use for PCA",
          choices = c("raw data", "NA removed", "outliers removed", "Summary Stats data"), multiple = F))
  })  
  
# we need to put all possible datasets in the same format - let's melt it all!
  
  my_data_melt <- eventReactive(input$Go_PCAdata,{
    my_melt <- melt(my_data(), id=c(input$SelectGeno, input$SelectIV, input$SelectID, input$SelectTime))
    my_melt
    })
  
 my_data_no_outl_melt <- eventReactive(input$Go_PCAdata,{
    my_melt <- melt(Outlier_free_data(), id=c(input$SelectGeno, input$SelectIV, input$SelectID, input$SelectTime))
    my_melt
  })
 
 my_data_nona_melt <- eventReactive(input$Go_PCAdata,{
   my_melt <- melt(NA_free_data(), id=c(input$SelectGeno, input$SelectIV, input$SelectID, input$SelectTime))
   my_melt
 })
 
 sum_data_melt <- eventReactive(input$Go_PCAdata,{
   my_melt <- melt(sum_data(), id=c(input$SelectGeno, input$SelectIV, input$SelectID, input$SelectTime, "Dependent_Variable"))
   names(my_melt)[names(my_melt) == "variable"] <- "summary_stat"
   my_melt$variable <- paste(my_melt$Dependent_Variable,"_",my_melt$summary_stat, separate="")
   my_melt
   
 })
 
 PCA_data_type <- eventReactive(input$Go_PCAdata,{
   if(input$PCA_data == "raw data"){
     PCA_data_type <- my_data_melt()
   }
   if(input$PCA_data == "NA removed"){
     PCA_data_type <- my_data_nona_melt()
   }
   if(input$PCA_data == "outliers removed"){
     PCA_data_type <- Outlier_free_data()
   }
   if(input$PCA_data == "Summary Stats data"){
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
 
 output$PCA_eigen_plot <- renderPlotly({
 eigenvalues <- PCA_eigen_data()
 Y= (eigenvalues[, 2])
 X= (names.arg=1:nrow(eigenvalues))
 bar <-  ggplot(eigenvalues, aes(x=X, y=Y)) + geom_bar(stat="identity")+
xlab("Principal Components") + ylab("Percentage of variances") +
ggtitle("Variances")
 ggplotly(bar)
 })
 
 output$PCA1_select <- renderUI({
   if ((input$Go_PCAdata == FALSE)) {
     return()
   } else
     eigenvalues <- PCA_eigen_data()
     list_avail_PCs <- unique(1:(nrow(eigenvalues)-2))
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
   list_avail_PCs <- unique(1:(nrow(eigenvalues)-2))
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
   endCol <-ncol(PCA_final_data())
   PCA_ready <- PCA_final_data()
   PCA_ready <- PCA_ready[, beginCol : endCol]
   res.pca <- PCA(PCA_ready, graph = FALSE)
  mid=median(res.pca$var$contrib)
  fviz_pca_var(res.pca, axes = c(as.numeric(input$Which_PC1),as.numeric(input$Which_PC2)), col.var="contrib", geom ="auto", labelsize = 4, repel=T, label="var", addlabels=T, invisible = "none") +
   scale_color_gradient2(low="grey", mid="purple", 
                         high="red", midpoint=mid)+theme_bw()
 })
 
 output$PCA_scatter_plot <- renderPlotly({
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
   fviz_pca_ind(res.pca, axes = c(as.numeric(input$Which_PC1),as.numeric(input$Which_PC2)), col.ind="cos2", repel=T) +
     scale_color_gradient2(low="grey", mid="purple", 
                           high="red", midpoint=mid1)+
     theme_minimal()
 })
 
 # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # - - - - - - - - - - - - >> CLUSTER ANALYSIS IN 8th TAB << - - - - - - - - - - -
 # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
 
  # end of the script
}
