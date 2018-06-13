cat("#R code for K means clustering:")
	cat("\n")
cat("# Make sure you have installed theses packages and loaded the corresponding libraries: 'factoextra','NbClust', and 'ggplot2'.")
cat("\n")

cat("library('factoextra')")
cat("\n")
cat("library('NbClust')")
cat("\n")
cat("library('ggplot2')")
cat("\n")

cat("#Choose your data")
cat("\n")
if(input$KMCluster_data == "raw data"){
	cat("KMC_data_type <- my_data)")
	cat("\n")
    }
    if(input$KMCluster_data == "missing values removed data"){
    cat("KMC_data_type <- my_data_nona")
    cat("\n")
    }
    if(input$KMCluster_data == "r2 fitted curves curated data"){
      cat("KMC_data_type <- curve_data")
      cat("\n")
    }
    if(input$KMCluster_data == "r2 fitted curves curated data with missing values removed"){
      cat("KMC_data_type <- curve_data_nona")
      cat("\n")
    }
    if(input$KMCluster_data == "outliers removed data"){
      cat("KMC_data_type <- no_outl_data")
      cat("\n")
    }


cat("#Prepare Data")
cat("\n")
cat("# K means will not take missing data, so either use 'na.omit' or impute data. For simplification purposes we use 'na.omit'")
cat("\n")
cat("KMC_data_type <- na.omit(KMC_data_type) # listwise deletion of missing data")
cat("\n")

if(input$KMC_use_means == T){
cat("# If you have multiple replicates and have cleaned your data from outliers, you can calculate k means clustering using the means. To obtain a new data frame with means, use the following code:")
cat("\n")
cat("library(doBy)")
 cat("\n")
     cat("KMC_data_type <- subset(KMC_data_type, 
     select=c('", paste(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID, input$SelectDV, sep="', '"), "')")
      cat("\n")
     cat("genotype<-c('",input$SelectGeno,"')")
     cat("\n")
     cat("KMC_data <- summaryBy(list(c('",input$SelectDV,"'),c('", paste(input$SelectGeno, input$SelectIV, input$SelectTime, sep="', '"),"')), data=KMC_data, id=c('",input$SelectID,"'),FUN=mean)")
cat("\n")     
}

 if(input$KMC_use_means == F){
      
      cat("KMC_data <- subset(KMC_data_type, select=c('",paste(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID, input$SelectDV, sep="', '"),"'))")
      cat("\n")          
    }


#KMC_data_for_matrix <- reactive({
  #  object <- KMC_data()
    
    if(input$KMC_subset_Q == T){
      cat("KMC_data$split <- KMC_data[,'",input$KMC_trait_sub,"']")
      cat("\n")
      cat("the_one <- '",input$KMC_trait_sub_spec,"'")
      cat("\n")
      cat("KMC_data <- subset(KMC_data, KMC_data$split == the_one)")
      cat("\n")
    }
    
    if(input$KMC_use_means == T){
    	
      cat("pheno<-paste('",input$KMC_pheno,"','mean', sep = '.')")
      cat("\n")
      cat("sel <- subset(KMC_data, select=c('",paste(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID,pheno, sep="', '"),"'))")
      cat("\n")
    }
    
    if(input$KMC_use_means == F){
      
      cat("sel <- subset(KMC_data, select=c('",paste(input$SelectGeno, input$SelectIV, input$SelectTime, input$SelectID,input$KMC_pheno, sep="', '"),"'))")
      cat("\n")
      
    }
    cat("#To move only the numerical data into a new matrix:")
    cat("\n")
    cat("beginCol <-
      length(c('",paste(input$SelectIV,
        input$SelectGeno,
        input$SelectTime,
        input$SelectID, sep"', '"),"')) + 1")
    cat("\n")
    
    cat("endCol <- ncol(sel)")
    cat("\n")
    
    cat("KMC_matrix <- sel[,(beginCol:endCol)]")
    cat("\n") 
    cat("KMC_matrix <- na.omit(KMC_matrix)")
    cat("\n") 
    
    if(input$KMCluster_scale_Q == T){
      cat("# When using variables with different units of measurement, it is highly reccomendable to scale your data prior to analysis.")
		cat("\n")
		cat("KMC_matrix <- scale(KMC_matrix) # standardize variables")
		cat("\n")     
		}   

   	cat("#K means clustering:")
	cat("\n")
	cat("set.seed(20)")
	cat("\n")
	cat("KMClusters <- kmeans(KMC_matrix,", input$kmclusters, ", nstart = 20)")
	cat("\n")
	cat("KMClusters$cluster<- as.factor(KMClusters$cluster)")
   cat("\n")
    cat("# To append cluster number assigned to the individual samples in the original data:")
    cat("\n")
    cat("KMC_data$cluster<-KMClusters$cluster")

		
   	




cat("#R code for determining the optimal number of K means clusters")
cat("\n")
cat("# Elbow method")
cat("\n")
cat("fviz_nbclust(mydata, kmeans, method = 'wss') +")
cat("\n")
    cat("geom_vline(xintercept = 4, linetype = 2)+")
    cat("\n")
  cat("labs(title = 'Elbow method')")
  cat("\n")
cat("# Silhouette method")
cat("\n")
cat("fviz_nbclust(mydata, kmeans, method = 'silhouette')+")
cat("\n")
  cat("labs(title = 'Silhouette method')")
  cat("\n")
cat("#Majority of 30 indices")
cat("\n")
cat("nb <- NbClust(mydata, distance = 'euclidean', min.nc = 2,
        max.nc = 10, method = 'kmeans')")
        cat("\n")
		cat("fviz_nbclust(nb)")

cat("\n")

cat("# K means bar plot")
cat("\n")  
 
   #output$R_Kclu_bar <- renderPrint({

    cat("# One way of visualising k means clustering of your data is using bar plots. To plot your data and the assigned clusters, first select a variable.")
    cat("\n")
    cat("listIV<-c('", paste(input$SelectGeno, input$SelectIV, input$SelectTime, sep="', '"), "')")
  		cat("\n")
	 cat("facet<-'",input$facet_KMC_barplot,"'")
    cat("\n")
	 cat("listNonSel<-setdiff(listIV,facet)")
    cat("\n")
	if(input$KMC_split_barplot == T){
     	 cat("KMC_data$id <- do.call(paste, c(KMC_data[c(listNonSel)], sep='_'))")
     	 cat("\n")
     	 }
    if(input$KMC_split_barplot == F){
      cat("KMC_data$id <- do.call(paste, c(KMC_data[c(listIV)], sep='_'))")
      cat("\n")
      }
   
    if(input$KMC_use_means == T){
      if(input$KMC_split_barplot == F){
        cat("p <- ggplot(KMC_data, aes(x =reorder(id,-KMC_data[,'",input$KMC_trait_to_plot,"']) , y = KMC_data[,'",input$KMC_trait_to_plot,"'], fill = cluster) )+")
         cat("\n")
	 		cat("geom_bar(stat='identity')+xlab(' ') +ylab(' ') +")
         cat("\n")
	 cat("theme(axis.text.x = element_text(angle = 90, hjust = 1))")
          }
      if(input$KMC_split_barplot == T){
        cat("KMC_data<- KMC_data %>%")
          cat("\n")
	 		cat("ungroup() %>%")
          cat("\n")
	 		cat("arrange(KMC_data[,'",input$facet_KMC_barplot,"'],-KMC_data[,'",input$KMC_trait_to_plot,"']) %>%")
          cat("\n")
	 		cat("mutate(.r=row_number())")
        cat("\n")
	 		cat("p <- ggplot(KMC_data, aes(x =.r,y = KMC_data[,'",input$KMC_trait_to_plot,"'], fill = cluster) )+")
          cat("\n")
	 		cat("geom_bar(stat='identity')+xlab(' ') +ylab(' ') +")
          cat("\n")
	 		cat("theme(axis.text.x = element_text(angle = 90, hjust = 1))")
        cat("\n")
	 		cat("p<-p + facet_wrap(~ KMC_data[,'",input$facet_KMC_barplot,"'], scale = '",input$Select_KMC_barplot_sc,"')+")
	 		cat("\n")
	 		cat("scale_x_continuous(breaks=KMC_data$.r,labels=KMC_data$id)")
        cat("\n")
        }
      if(input$Select_KMC_background_barplot == T){
        cat("p <- p + theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))")
        cat("\n")
        }
      if(input$Select_KMC_grid_barplot == T){
        cat("p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1),panel.grid.major = element_blank())")
        cat("\n")
        }
    }
    if(input$KMC_use_means == F){
      cat("p <- ggplot(KMC_data, aes(x = reorder(id,-KMC_data[,'",input$KMC_trait_to_plot,"']), y = KMC_data[,'",input$KMC_trait_to_plot,"'], color = cluster)) + ")
      cat("\n")
      cat("geom_point() +xlab(' ') +ylab(' ')")
      cat("\n")
      if(input$KMC_split_barplot == T){
        cat("p<-p + facet_wrap(~ KMC_data[,'",input$facet_KMC_barplot,"'], scale = '",input$Select_KMC_barplot_sc,"')")
        cat("\n")
        }
      if(input$Select_KMC_background_barplot == T){
        cat("p <- p + theme_minimal()")
        cat("\n")
        }
      if(input$Select_KMC_grid_barplot == T){
        cat("p <- p + theme(panel.grid.major = element_blank())")
        cat("\n")
        }
    }
    cat("p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))")
    cat("\n")
    cat("p")
    cat("\n")
       
  })
# K means scatter plot

output$R_Kclu_scat <- renderPrint({
    cat("Another, more common, way of visualising k means clustering of your data is using scatter plots. To plot your data and the assigned clusters:")
    cat("\n")

 #scatterplotData <- reactive({

 
 cat("listIV<-c('", paste(input$SelectGeno, input$SelectIV, input$SelectTime, sep="', '"), "')")
  		cat("\n")
	 cat("facet<-'",input$facet_KMC_plot,"'")
    cat("\n")
	 cat("listNonSel<-setdiff(listIV,facet)")
    cat("\n")
	if(input$KMC_split_scatterplot == T){
     	 cat("KMC_data$id <- do.call(paste, c(KMC_data[c(listNonSel)], sep='_'))")
     	 cat("\n")
     	 }
    if(input$KMC_split_scatterplot == F){
      cat("KMC_data$id <- do.call(paste, c(KMC_data[c(listIV)], sep='_'))")
      cat("\n")
      }

 #kmeans_SCP <- reactive({
       cat("p <- ggplot(KMC_data, aes(x = KMC_data[,'",input$xcol_kmeans,"'], y = KMC_data[,'",input$ycol_kmeans,"'], color = cluster)) + ")
       cat("\n")
      cat("geom_point(aes(text=id))+")
      cat("\n")
      cat("xlab('",input$xcol_kmeans,"') +ylab('",input$ycol_kmeans,"') ")
      cat("\n")
    if(input$KMC_split_scatterplot == T){
     cat("p<- p+ facet_wrap(~ KMC_data[,'",input$facet_KMC_plot,"'], scale = '",input$Select_KMC_facet_sc,"')")
     cat("\n")
     }
    if(input$Select_KMC_background == T){
      cat("p <- p + theme_minimal()")
      cat("\n")
      }
    if(input$Select_KMC_grid == T){
      cat("p <- p + theme(panel.grid.major = element_blank())")
      cat("\n")
      }
    cat("p")
    cat("\n")
 # })
  
    })