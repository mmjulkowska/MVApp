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
###### Selecting informative PCs
###### Looking at the contribution and how your individuals are spaced in the PC
###### downloading the data

### 7. Cluster analysis => Magda

###### Selecting the data
###### What kind of clustering
###### Select the distance for cluster separation
###### Validate the cluster


![screenshot 2017-10-04 18 41 49](https://user-images.githubusercontent.com/14832460/31326897-f3cd90e4-acd3-11e7-9cbf-06da5e9ccc02.png)
