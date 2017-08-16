function(input,output){


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#  - - - - - - - - - - >> GADGETS FOR 2nd TAB <<- - - - - - - - - - - - - - - - -   
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
ItemList = reactive(
    if(is.null(input$your_data)){return()
    } else {
      d2 = read.csv(input$your_data$datapath)
            return(colnames(d2))}
  )

# - - - - - - - - - - - - - - - - - >> Reactive widgets << - - - - - - - - - - - -
# Select SampleID
output$CustomID <- renderUI({
  if((is.null(ItemList())) | (input$IdCheck== FALSE)) {return ()
  } else tagList(
    selectizeInput(inputId = "SelectID", 
                   label = "Select here your sample ID", 
                   choices = ItemList(), multiple=F))
})

# Select Independent Variable (genotype, treatment et al.)
output$CustomDepVar <- renderUI({
    if(is.null(ItemList())){return ()
    } else tagList(
      selectizeInput(inputId = "SelectIV", 
                         label = "Select here your independent variables", 
                         choices = ItemList(), multiple=T))
})

# Select time column
output$CustomTimepoint <- renderUI({
  if((is.null(ItemList())) | (input$TimeCheck==F)){return ()
  } else tagList(
    selectizeInput(inputId = "SelectTime", 
                   label = "Select here column with time", 
                   choices = ItemList(), multiple=F))
})

# Select Dependent Variables (phenotypes)
output$CustomPheno <- renderUI({
  if(is.null(ItemList())){return ()
  } else tagList(
    selectizeInput(inputId = "SelectDV", 
                       label = "Select here your dependent variables", 
                       choices = ItemList(), multiple=T))
})

# - - - - - - - - - - - - - - - - - >> Tables output in Tab2 << - - - - - - - - - - - -

# Table in the Tab2 - main window - uploaded file overview
output$Data_tabl <- renderDataTable({
  if(is.null(input$your_data)){return(NULL)} else{
    read.csv(input$your_data$datapath)
  }
})

# Table in the Tab2 - main window - selected variables by the user
my_data <- eventReactive(input$Go_Data, {
  d2 = read.csv(input$your_data$datapath)
  my_data <- subset(d2, select=c(input$SelectIV,input$SelectID, input$SelectTime, input$SelectDV))
  return(my_data)
})

output$my_data <- renderDataTable({
  my_data()
  })

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - >> MODELING IN 3rd TAB << - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$Pheno_to_model <- renderUI({
  if((is.null(input$SelectDV)) | (input$TimeCheck== FALSE)) {return ()
  } else tagList(
    selectizeInput(inputId = "ModelPheno", 
                   label = "Select a phenotype for which you would like estimate kinetics", 
                   choices = c(input$SelectDV), multiple=F))
})

output$IV_to_model <- renderUI({
  if((is.null(input$SelectIV)) | (input$TimeCheck== FALSE)) {return ()
  } else tagList(
    selectizeInput(inputId = "ModelIV", 
                   label = "Select an independent variable for which you would like estimate kinetics", 
                   choices = c(input$SelectIV), multiple=F))
})

output$IV_subset_model <- renderUI({
  if((is.null(input$SelectIV)) | (input$TimeCheck== FALSE)) {return ()
  } else tagList(
    selectizeInput(inputId = "ModelSubIV", 
                   label = "Select an independent variable for which you would to subset", 
                   choices = c(input$SelectIV), multiple=F))
})

# Calculations for the model - temporary
Model_temp_data <- eventReactive(input$Go_Model,{
  temp <- subset(my_data(), select=c(input$ModelIV, input$ModelSubIV, input$SelectID, input$SelectTime, input$ModelPheno))
  sub_set <- c(input$ModelIV, input$ModelSubIV, input$SelectID)
  things_to_model <- unique(temp[sub_set])
  
  for (i in 1:nrow(things_to_model)){
    super_temp <- subset(temp, temp[,1] == things_to_model[i,1])
    super_temp2 <- subset(super_temp, super_temp[,2] == things_to_model[i,2])
    super_temp3 <- subset(super_temp2, super_temp2[,3] == things_to_model[i,3])
  
  if(input$model == "lin") {
    fit <- lm(super_temp3[,4] ~ super_temp3[,5])
    things_to_model[i,4] <- coefficients(fit)[2]
    things_to_model[i,5] <- coefficients(fit)[1]
    things_to_model[i,6] <- summary(fit)$r.squared
  }

  if(input$model == "quad") {
    super_temp3$transformed <- sqrt(super_temp3[,5])
      fit <- lm(super_temp3[,4] ~ super_temp3$transformed)
      things_to_model[i,4] <- (coefficients(fit)[2])^2
      things_to_model[i,5] <- (coefficients(fit)[1])^2
      things_to_model[i,6] <- summary(fit)$r.squared
    }
  
  if(input$model == "exp") {
    super_temp3$transformed <- log(super_temp3[,5])
      fit <- lm(super_temp3[,4] ~ super_temp3$transformed)
      things_to_model[i,4] <- log(coefficients(fit)[2])
      things_to_model[i,5] <- coefficients(fit)[1]
      things_to_model[i,6] <- summary(fit)$r.squared
    }  

  if(input$model == "sqr") {
    super_temp3$transformed <- (super_temp3[,5])^2
      fit <- lm(super_temp3[,4] ~ super_temp3$transformed)
      things_to_model[i,4] <- sqrt(coefficients(fit)[2])
      things_to_model[i,5] <- sqrt(coefficients(fit)[1])
      things_to_model[i,6] <- summary(fit)$r.squared
  } 
    
  else{}
}
  things_to_model})
 
   # if(input$model == "quad"){
      
  #  stemp[,6] <- sqrt(stemp[,5])
   # fit <- lm(stemp[,4]~ stemp[,6])
      
    #  model_data[i,2] <- things_to_model[i,1]
     # model_data[i,3] <- things_to_model[i,2]
      #model_data[i,4] <- things_to_model[i,3]
      # growth factor
      #model_data[i,5] <- (coefficients(fit)[2])^2
      # START
    #  model_data[i,6] <- (coefficients(fit)[1])^2
      # R.squared
     # model_data[i,7] <- summary(fit)$r.squared
    #}
    
  #  if(input$model == "exp"){
      
   #   stemp[,6] <- log(stemp[,5])
    #  fit <- lm(stemp[,4]~ stemp[,6])
      
     # model_data[i,2] <- things_to_model[i,1]
    #  model_data[i,3] <- things_to_model[i,2]
    #  model_data[i,4] <- things_to_model[i,3]
      # growth factor
    #  model_data[i,5] <- coefficients(fit)[2]
      # START
     # model_data[i,6] <- log(coefficients(fit)[1])
      # R.squared
    #  model_data[i,7] <- summary(fit)$r.squared
    #}
    
  #  if(input$model == "sqrt"){
      
   #   stemp[,6] <- (stemp[,5])^2
    #  fit <- lm(stemp[,4]~ stemp[,6])
      
     # model_data[i,2] <- things_to_model[i,1]
    #  model_data[i,3] <- things_to_model[i,2]
    #  model_data[i,4] <- things_to_model[i,3]
      # growth factor
    #  model_data[i,5] <- sqrt(coefficients(fit)[2])
      # START
    #  model_data[i,6] <- sqrt(coefficients(fit)[1])
      # R.squared
    #  model_data[i,7] <- summary(fit)$r.squared
    #}    
  #}
  
  #return(things_to_model)
#})

output$Model_data <- renderTable({
  Model_temp_data()
})


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#  - - - - - - - - - - >> SUMMARY STATISTICS IN 4th TAB <<- - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Table in Tab4 - main window - summary of the data based on the selected calculations
sum_data <- eventReactive(input$Go_SummaryStat, {
  melted_icecream <- melt(my_data(), id=c(input$SelectIV,input$SelectID, input$SelectTime))
  sum_my_data <- summaryBy(value ~., data=melted_icecream) 
  return(sum_my_data)
})

output$sum_data <- renderTable({
  sum_data()
})

# To select 

output$Corr_IVSelect <- renderUI({
  if((is.null(input$SelectIV)) | (input$TimeCheck== FALSE)) {return ()
  } else tagList(
    selectizeInput(inputId = "Cor_SubIV", 
                   label = "Select an independent variable for which you would like to subset before the correlation", 
                   choices = c(input$SelectIV, input$SelectTime, input$SelectGeno), multiple=F))
})


# end of the script
	}