# MVApp
Glittery data analysis and multi-variate analysis for all kinds of data

## The objective of the App / overview

The app is currently being created to streamline data analysis for all kinds of biological data - from the mutant phenotypes to study of natural variation. Although the group developing the app is armpit-deep in Plant Science we are trying to make the App as applicable as possible for the other disciplines of Biology (or even beyond)

With the MVApp, we indend to provide a platform allowing:
IN FUTURE => links will be linked to those individual points - directing you to the specific sections 
1. Fitting the simple curves (linear / quadratic / exponential / square root) to the time-course data as well as fitting polynomial curves (cubic and smoothed splines)
2. Data exploration and removal of outliers
3. Analysis of significant differences between the individual genotypes / conditions studied
4. Analysis of variance among genotypes / conditions studied
5. Perform Correlation analysis between different phenotypes - using the entire data set as well as subsetted data per chosen condition / genotype
6. Principal Component Analysis with and without the normalization
7. Cluster Analysis based on the chosen traits 

### 1. DATA UPLOAD - What kind of data can we use?

All kinds of data can be used. We require that the format of your data will contain the following
* collumn with the genotype (if you use only one - please include the name)
* collumn(s) with the Independent Variable identifier (such as "condition", "position", "experiment number")
* collumn(s) with the Dependent Variables (phenotypes)

Additionally, if you would like to perform fitting the curves to your data, you should also include collumns containing:
* Time - this column has to contain only numeric values
* Sample ID - if you would like to model the change in the phenotype in your individual samples separately, please include Sample ID

The Example dataset would look like this:
![mvapp_data](https://user-images.githubusercontent.com/14832460/32609292-48dc541c-c570-11e7-8f87-54ca02b646d8.png)


You upload your data in the tab called "Upload your data". Click on the "Browse" button and upload the data in the .csv format:
![mvapp_data_upload1](https://user-images.githubusercontent.com/14832460/32609364-8044b6ce-c570-11e7-9002-dd9f21a700cf.png)

Select the columns containing Genotype, Independent Variables, Phenotype information. Alternatively, you can add columns containing Time / gradient data (MUST be numeric) and Sample ID:
![mvapp_data_upload2](https://user-images.githubusercontent.com/14832460/32609363-801c9950-c570-11e7-809b-d9922fb9a606.png)

"Click to set the data" and view the newly uploaded data table in sub-tab "New Data"
![mvapp_data_upload3](https://user-images.githubusercontent.com/14832460/32609362-7ff50520-c570-11e7-8191-d55065fbfdd6.png)

### 2. FITTING THE CURVES => Magda

If you have time component in your experiment, you might want to estimate how quickly are your plants / bacteria grow. You can also fit the curves in order to estimate the response type of your organism to a specific compound. In any case - fitting the curves will simplify your data. 

###### what kind of curves can we fit?
At the moment, you can fit the simple functions: linear, quadratic, exponential and square root functions. For all of those functions, we fit linear model (using lm() function) between the Independent Variable indicated in the "Time" column and the dependent variable (phenotype). In case of models other than linear, the dependent variable is transformed:
- square root transformation for fitting quadratic function
- log transformation for fitting exponential function
- quadratic transformation for fitting the square root function

We extract "y-intercept" (called "INTERCEPT") and the first regression coefficient (called "DELTA").

First select how you would like to group your samples in the side panel and which phenotype you would like to model. 

If you don't know which function will fit best, you can click on "Unleash model estimation" button". The best model will be selected based on the r2 values presented in the table:

![mvapp_model_estimation](https://user-images.githubusercontent.com/14832460/32610095-c63f08da-c572-11e7-88a8-65724913ef0a.png)

Decide on which model you would like to apply for entire dataset and click "Unleash the model":

![mvapp_model_calculated](https://user-images.githubusercontent.com/14832460/32610094-c60a9d0c-c572-11e7-82f1-9af9fbacf375.png)

Whenever you fit the curves to the chosen Dependent Variable (phenotype), the MVApp will automatically calculate coefficient of correlation (r2) that will indicate how well is the function fitting to the observed data. The number of samples that have a poor fit (r2 < 0.7) will be indicated in the message box above the table.

You can view the lowest r2 values by sorting the samples based on r2 by clicking on the r2 column sorting arrow:

![mvapp_model_rsq_sorted](https://user-images.githubusercontent.com/14832460/32610092-c5e8c0f6-c572-11e7-92ca-7934dd13e8ea.png)

If your data contains multiple time-points, you might consider fitting a polynomial curve.

For the [cubic splines](http://mathworld.wolfram.com/CubicSpline.html), we use lm(phenotype ~ bs(time, knots=X)) function in R, where the position of knots is indicated by the user

![mvapp_model_cubic](https://user-images.githubusercontent.com/14832460/32610090-c5c3bfcc-c572-11e7-87c2-d4090b8beb0d.png)

For the smoothed splines, we use smooth.spline() function in R, with either the degrees of freedom being selected automaticall, or indicated by the user:

![mvapp_model_smoothed](https://user-images.githubusercontent.com/14832460/32610089-c59f6abe-c572-11e7-90a1-7a50f1dd4cb4.png)

###### How can you use modelling to select outliers?

You can view how your data fits to the model by by viewing individual fit-plots - the names of the samples are merged by "Genotype_IndependentVariable_SampleID". You can either scroll through the sample list or type in the sample name.

![mvapp_model_single_plot](https://user-images.githubusercontent.com/14832460/32610100-c6cd6332-c572-11e7-973f-45a99169dfa8.png)

Alternatively, you can view multiple plots simultaneously. The plots can be sorted by either increasing or decreasing r2 values.

![mvapp_model_multiple](https://user-images.githubusercontent.com/14832460/32610099-c6aa4078-c572-11e7-80b6-98efb766b710.png)

By examining the data, you can select the samples that are odd and consider removing them from the dataset. 

###### Comparing the dynamics among Genotypes and Independent Variables

Finally, you can have a look how the calculated DELTAs or Coefficients extracted from the models differ between Genotypes and Independent Variables, by clicking on sub-tab "Examine differences". The message box at the top provides ANOVA results, with the p-value threshold indicated below the graph.

![mvapp_model_summarystats](https://user-images.githubusercontent.com/14832460/32610654-75e89eda-c574-11e7-9cf3-bdb69c4d299f.png)

By scrolling further down, you will find a message containing the significance groups, as calculated per Tukey's pair-wise test, with the same p-value threshold as ANOVA. 

![mvapp_model_summarystats](https://user-images.githubusercontent.com/14832460/32610098-c682f090-c572-11e7-8945-d5257b591ca3.png)


### 3. OUTLIER SELECTION => Mitch

###### How to select the dependent variables (exclude ID)
###### Methods for selection (include links to the methods - preferably paper that uses it as an example or Wikipedia)
###### Outliers based on ALL traits
###### Outliers based on ONE trait

### 4. DATA EXPLORATION => Stephanie

###### Summary stats
###### Histograms + variation analysis + testing for normality and variance (including ftest ;P )
###### ANOVA + boxplots

### 5. CORRELATIONS => Gege

###### General Correlations
###### Subsetted Correlations
###### Scatter plots

### 6. PCA => Mariam

###### Selecting the dataset & traits
-   As mentioned previously, the user has the option to set which dataset is used to perform the ["principle component analysis (PCA)"](https://en.wikipedia.org/wiki/Principal_component_analysis) from the dropdown menu of 'raw data, 'missing values removed' or 'outliers removed'. 

-   After setting the data, the user can select which DVs (phenotypes) will go into the PCA.

-   The user can decide to perform the PCA on the values as they are or on the averaged data per IV (e.g. genotype, treatment, time).

-   The user also has a further option to use the full dataset or decide to subset based on a chosen IV or IVs.

-   All these options can be reset by clicking on 'set the dataset' and choosing other options to compare between different PCA results.

-   The final selected dataset for the PCA can be found in the tab 'Final data for PCA'.

###### Visualizing the PCs

-   When clicking on the ['Eigenvalues'](https://en.wikipedia.org/wiki/Eigenvalues_and_eigenvectors) tab, the ["Scree plot"](http://www.improvedoutcomes.com/docs/WebSiteDocs/PCA/Creating_a_Scree_Plot.htm) would be displayed showing the main principle components generated from the PCA in decreasing order of percentage variance.

-   The Scree plot can be copied as an image into MS Office platforms.

-   The table summarizing the eigenvalues of each principle component, their percentage of variance and the cumulative percentages that add to 100% can be found below the plot and can be downloaded as a CSV file.

###### Visualizing the general contribution of DVs per PC

-   When clicking on the 'Contribution per variable' tab, the user can visualize the contribution of the chosen DVs with respect to two PCs at a time that the user can choose from the two dropdown menus. 

-   The contribution plot can be copied as an image into MS Office platforms.

###### Visualizing the contribution of each DV value

-   When clicking on the 'Scatterplot of individuals' tab, the visualization moved from the PC contribution of the variables (DVs) to the individuals (DV values).

-   The dropdown menu above the plot allows the user to change the color scheme to highlight different IVs.

-   The table summarizes each DV value and its contribution to the generated PCs, which can be downloaded as a CSV file.

###### Visualizing the contribution of each DV per PC

-   A different representation of the 'contribution per variable' tab is to look at the 'Contribution per PC' in detail in a bar plot format and download the specific percentange contribution data per PC.

### 7. Cluster analysis => Magda

###### Selecting the data
###### What kind of clustering
###### Select the distance for cluster separation
###### Validate the cluster
