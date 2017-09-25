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
    return(my_data)
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
      
      else{
      }
    }
    things_to_model
  })
  
  output$Model_data <- renderTable({
    Model_temp_data()
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
    melted_icecream <- melt(my_data_nona(), id=c(input$SelectGeno, input$SelectIV, input$SelectID, input$SelectTime))
  #change day/time into factor becuase it is now numeric and does not show in melt.
    
    ## Added call to selected summary stats functions "FUN=summfuns[input$SelectSumm]"     %% Mitch %%
    sum_my_data<-summaryBy(value ~., data=melted_icecream, FUN=summfuns[input$SelectSumm])
    
    ## Label columns based on chosen summary stats     %% Mitch %%
    colnames(sum_my_data)<-c(input$SelectGeno, input$SelectIV, input$SelectID, "Dependent Variable", input$SelectSumm)
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
      fit <- ggplot(my_his_data, aes(x=my_his_data[,1], fill=my_his_data[,2])) + xlab(names(my_his_data[1]))  + geom_density(alpha=0.3)
    }
    ggplotly(fit)
     }) 
  

     
     ##WORKEDDD but has to be 1 dependent variable and 1 independent only!!. Also problem with group"day" because there are too many...
    

   # my_hisdata2<-eventReactive(input$Go_Boxplot, {
    #  hisdata2<-my_data()[,c(input$HisDV,input$HisIV)]
    #})
    
  
 
    
    ##try to do subset by multiple variables
    output$Boxes <- renderPlotly({
      my_his_data <- my_hisdata()
      
      #a <- aov(my_his_data[,1]~my_his_data[,2], data=my_his_data)
      #tHSD <- TukeyHSD(a, ordered = FALSE, conf.level = 0.95)
      
      #generate_label_df <- function(HSD, flev){
        # Extract labels and factor levels from Tukey post-hoc 
       # Tukey.levels <- HSD[[flev]][,4]
        #Tukey.labels <- multcompLetters(Tukey.levels)['Letters']
        #plot.labels <- names(Tukey.labels[['Letters']])
        
        # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
        # upper quantile and label placement
        #boxplot.df <- ddply(d, flev, function (x) max(fivenum(x$y)) + 0.2)
        
        # Create a data frame out of the factor levels and Tukey's homogenous group letters
        #plot.levels <- data.frame(plot.labels, labels = Tukey.labels[['Letters']],
                                 # stringsAsFactors = FALSE)
        
        # Merge it with the labels
        #labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
        
        #return(labels.df)
      #}
      
      #box_graph <- ggplot(my_his_data, aes(x=my_his_data[,2], y=my_his_data[,1])) + xlab(names(my_his_data[2])) + ylab(names(my_his_data[1])) + geom_boxplot()
      #+ geom_text(data = generate_label_df(tHSD, 'my_his_data[,2]'), aes(x = plot.labels, y = V1, label = labels))
      
      
      
     box_graph <- ggplot(my_his_data, aes(x=my_his_data[,2], y=my_his_data[,1])) + xlab(names(my_his_data[2])) + ylab(names(my_his_data[1])) + geom_boxplot()
      
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
    
    corrplot(
      cor(my_data()[, beginCol:endCol]),
      type = "upper",
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
    # selector <- as.character(input$CorIV_sub)
    # my_data2 <- subset(my_data, input$CorIV_sub == input$CorIV_val)
    names(my_data) <- sub(input$CorIV_sub, "Cor_baby", names(my_data))
    my_data2 <- subset(my_data, Cor_baby == input$CorIV_val)
    my_data2 <- na.omit(my_data2)
    corrplot(
      cor(my_data2[, beginCol:endCol]),
      type = "upper",
      tl.col  = "black",
      Rowv = F,
      Colv = F
    )
  })
  
  # end of the script
}
