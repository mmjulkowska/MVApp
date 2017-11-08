# >>> MVApp <<< 
Glittery data analysis and multi-variate analysis for all kinds of data

## The objective of the App / overview

The app is currently being created to allow the easy and straightforward data analysis of all kinds of data - from the mutant phenotypes to study of natural variation. Although the group developing the app is armpit-deep in Plant Science we are trying to make the App as applicable as possible for the other disciplines of Biology (or even beyond)

With the MVApp, we indend to provide a platform allowing:
IN FUTURE => links will be linked to those individual points - directing you to the specific sections 
1. Fitting the curves (linear / quadratic / exponential / square root) to the time-course data
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
=> IN FUTURE => SCREENSHOT OF THE DATA IN HERE open in EXCEL for example

### 2. FITTING THE CURVES => Magda

###### what kind of curves can we fit?
###### How does it fit?
###### How can you use it to select outliers?

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


![screenshot 2017-10-04 18 41 49](https://user-images.githubusercontent.com/14832460/31326897-f3cd90e4-acd3-11e7-9cbf-06da5e9ccc02.png)
