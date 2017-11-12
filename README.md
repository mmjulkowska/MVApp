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

The Example dataset looks like this:
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


### 3. OUTLIER SELECTION => Magda

If you ever run a large(ish) scale experiment, you probably went through a data curation. You picked up the samples either based on the Standard Deviation from the mean, or outside of the 1.5 interquartile range. !!! INCLUDE LINKS TO STATS PAGE ON OUTLIER SELECTION !!!

MVApp facilitates automatic highlighting of the outliers based on single phenotype or all the phenotypes. 

###### Select dataset & methods to highlight the outliers

First what you need to do is select the Independent Variables by which we can group the samples. MVApp assumes that you would like to group the variables by Genotype, ALL Independent Variables and Time (Sample ID is excluded).

You can remove missing values by clicking the check-box in the top-left corner of the side-bar.

Subsequently, you select which method would you like to use to highlight the outliers. MVApp provides following methods:
!!!! TO INCLUDE LINKS TO WIKIPEDIA HERE!!!!
- 1.5 x Interquartile Range - this is the most commonly used method and is visually very appealing, as the outliers will be identified on the boxplot as the "dots" outside of the whiskers
- Cook's distance - this algorithm is actually used to determine the most influential points - meaning the points that could make the most difference in the correlation tests and such. However, if the sample is considered to be "influential" in majority of the traits, it might be worthwhile to check whether everything is OK with it.
- Bonferoni test - this is a test useing car::outlierTest() function from R
- 1xStandard Deviation from the Median - all the samples that are further than 1xSD on each side of the median are considered as outliers (this test is VERY strict and we do not recomend it)
- 2x Standard Deviation from the Median - all the samples that are further than 2xSD on each side of the median are considered as outliers
- 2.5 Standard Deviation from the Median - all the samples that are further than 2.5xSD on each side of the median are considered as outliers
- 3 Standard Deviation from the Median - all the samples that are further than 3xSD on each side of the median are considered as outliers

![mvapp_outlier_methods](https://user-images.githubusercontent.com/14832460/32647173-51f8df5e-c5f0-11e7-8dc4-8ce1d9da152a.png)

Click on "Unleash outlier highlighter" to see the table with highlighted outliers. This might take a while - depending on the size of your data.

In the main tab, the outlier message will appear, listing the number of the outliers selected, and the table with your data. If you scroll to the right, you will see the columns marked "outl_Phenotype" (for example "outl_AREA"), where "true" will indicate this sample as being an outlier per genotype / day / independent variables selected.

If you select outliers based on all phenotypes, you can change the threshold of a sample marked as an outlier by changing the slider input in the side panel. 

![mvapp_outlier_output1](https://user-images.githubusercontent.com/14832460/32647171-51b52f48-c5f0-11e7-8a46-6ae78ad193c3.png)

###### Examine the data containing the outliers

After we examined how many outliers might be in our data, you can have a look at how the data looks with and without outliers.

Go to sidepanel sub-tab "Tweak the graphs" and select the phenotype you wish to examine. 

Click on the main panel sub-tab "Graph containing outliers" to see the boxplot: 

![mvapp_outlier_graph_bugs](https://user-images.githubusercontent.com/14832460/32647170-517e6c4c-c5f0-11e7-99a1-89a3adc59e1c.png)

If you wish to color code your samples or split the plots depending on Independent Variable, you can select it in the side panel:

![mvapp_outlier_graph_facet_color](https://user-images.githubusercontent.com/14832460/32647168-507f1f26-c5f0-11e7-9f9f-c0f162cf7f95.png)

If you want to alter the order of your samples - you can swap the order of the Independent Variables in the sidebar sub-panel "Outlier selection"

![mvapp_outlier_graph_even_nicer](https://user-images.githubusercontent.com/14832460/32647167-5066effa-c5f0-11e7-9388-9620294702ff.png)

###### Compare the data with outlier removed

If you want to look at the graphs with no outliers (as defined in main panel "The outliers test"), click on the main panel "Graphs with outliers removed". You can click between "Graph containing outlier" and "Graph with outliers removed" to compare both datasets.

![mvapp_outliers_graph_no_outliers](https://user-images.githubusercontent.com/14832460/32647177-528bf596-c5f0-11e7-89dd-a8c85a287a47.png)

IMPORTANT NOTE: This outlier test was developed to facilitate data curation. Please do NOT remove any data before making absolutely sure that there is a very good reason that the sample is not representative 

PICTURE OF MOUSE EATING A PLANT

###### Perform summary statistics

MVApp can calculate summary statistics functions in the main panel sub-tab "Summary data".

Select the data-set in the upper left corner of the tab, and select the functions that you want to be calculated in the upper right corner:

NOTE: If you removed missing values prior to outlier selection, the dataset without outliers will NOT contain missing values. If you didnot removed missing values, the dataset with outliers removed will contain missing values. 

![mvapp_outlier_summ_stats](https://user-images.githubusercontent.com/14832460/32647166-503e1d64-c5f0-11e7-98ff-54ef9cc5fa02.png)

Click on "Unleas Summary Statistics" and the table will appear in the main panel:

![mvapp_outlier_summstats2](https://user-images.githubusercontent.com/14832460/32647174-52602bd2-c5f0-11e7-87f7-616859da1044.png)

### 4. DATA EXPLORATION => Stephanie
In this tab, the user is able to explore their data by testing for normality, homoscedasticity (i.e. equal variances), and differences in means. 
On the left pane of the window, the user chooses:
- The dataset to be used (raw data, data with missing values removed, or data with outliers removed). The default value is raw data.

- The independent variable to subset the data (e.g: Treatment)
- The trait to be used as the dependent variable ("Phenotype to plot")
- The p-value threshold to be used in subsequent tests in the tab (e.g.: Levene's test, ANOVA, etc.). The default value is 0.05.
- Whether they want to facet their graphs by another variable. When the user ticks the checkbox "Would you like to facet the graph?", a dropdown menu with a list of independent variables appears. The user can choose the independent variable to split the plot.
Once the choices are made, the user can proceed to the different tests available in the DATA EXPLORATION tab:

###### Testing normal distribution
In this sub-tab, the user can choose the "Plot type". Two options are available to display the histograms: "Histograms with  counts on y-axis" or "Histograms with  density on y-axis". From these plots, the user can visualize if their data is normally distributed or skewed. 
A message appears below the histograms. This message summarizes the groups/subgroups that seem not to have a normal distribution, where the p-value of the Shapiro-Wilk test is larger than the p-value threshold already chosen. 
The user can see the results of the Shapiro-Wilk test for all groups/subgroups along with their QQ-plots by ticking the checkbox "Would you like to see detailed Shapiro-Wilk test and QQplots?". The table shows p-value of the Shapiro-Silk tests performed for each groups/subgroups. If the p-value of the Shapiro-Silk test for a group is larger than the p-value threshold already chosen, a message "Data has NORMAL distribution" appears. In this case there is not enough evidence to reject the null hypothesis, where the data is considered from a normally distributed population. If the p-value of the Shapiro-Silk test for a group is smaller than the p-value threshold already chosen, a message "Data might NOT be normally distributed" appears. In this case the null hypothesis is rejected and the data is considered not from normally distributed population. However, sample size affects the Shapiro-Silk test and hence, the user is strongly encouraged to check the QQ-plots.
When the user checks "Would you like to see detailed Shapiro-Wilk test and QQplots?", sliders for QQ-plots also appear. These sliders help choose the optimum number of columns and plots for display.
The first slider " Display QQ plots in ... columns:" allows the user to choose the number of columns for the display of the QQ-plots. If the number of plots is very large to be displayed all at once in the window, a second slider "Plot portion of the data starting from element number..." appears and the user can choose the portion of plots to be displayed. 
Based on the results obtained in this sub-tab, the user can have a better judgement in the following sub-tab whether to check Bartlett or Levene test for equal variances.

###### Testing equal variance 
In this sub-tab, the user can have a look at the results of the Bartlett test and Levene test of homogeneity of variances between the different groups and for each sub-groups. 
The null hypothesis of the Bartlett test and Levene tests assumes that the variances in each of the groups are the same. Bartlett test is more robust when the data comes from a normal distribution, while Levene test is more robust in case of departures from normality.
The first table displays the results of the Bartlett test and the second table displays those of the Levene test.
The tables show the p-value of tests performed for each groups/subgroups. If the p-value of the test for a group is larger than the p-value threshold already chosen, a message "Equal" appears. In this case there is not enough evidence to reject the null hypothesis, where the variances are considered equal. If the p-value of the test for a group is smaller than the p-value threshold already chosen, a message "Not equal" appears. In this case the null hypothesis is rejected and the variances are not equal.
The results of this sub-tab and the previous sub-tab are needed to for the ANOVA test performed in the following sub-table.
ANOVA assumes the data comes from a normal distribution and the variances are equal.

###### Testing significant differences
In this sub-tab, the user can check for signifcant differences in the means between different groups using analysis of variance (ANOVA). A table displays the p-value of the ANOVA test between different levels of the independent variable.
If the p-value of the ANOVA test for a group is larger than the p-value threshold already chosen, a message "Cannot reject H0" appears. In this case there is not enough evidence to reject the null hypothesis and the means of the groups are assumed equal. If the p-value of the ANOVA test for a group is smaller than the p-value threshold already chosen, a message "Significant difference in means" appears. In this case the null hypothesis, where the means of the group are considered equal, is rejected and the means of the groups are considered to be significantly different. 

Boxplots display the distribution of the data for a specific trait (dependent variable) for the levels of the independent variable. The boxplots are facetted by the second independent variable, which the user chose when they ticked the checkbox for "Would you like to facet the graph?". 

A second table displays the significant groups based on Tukey's pairwise comparison. Groups that share a common letter do not have significantly different means for a trait. In the example below, there is no significant differences in the perimeter between the salt and control groups for the Accession C24. While there is a significant differences in the perimeter between the salt and control groups for the Accession Te.



###### Summary stats
###### Histograms + variation analysis + testing for normality and variance (including ftest ;P )
###### ANOVA + boxplots

### 5. CORRELATIONS => Gege

###### General Correlations
###### Subsetted Correlations
###### Scatter plots

### 6. PCA => Mariam

["Principle component analysis (PCA)"](https://en.wikipedia.org/wiki/Principal_component_analysis) is often used to simplify the data into fewer dimentions, and also check which traits explain majority of the variation in the population studied. However, the PCA is often not explored to its full potential. You can for example run PCA on a subset of your data - for example subsetting your data per treatment or genotype - and run PCA separately on those subsets and observe how the individual phenotypes contribute to explaining observed variation. MVApp will allow you to do simple PCA on all of your data, or on the subsetted data per any of the Independent Variables selected. 

###### Selecting the dataset & traits

Select the dataset from the dropdown menu at the top of the side panel and click "Set the dataset":

![mvapp_pca_data_input1](https://user-images.githubusercontent.com/14832460/32647215-7829f348-c5f0-11e7-8416-c339543a8e9f.png)

Then, select which Dependent Variables / phenotypes are going to be used in PCA and click "Unleash the PCA monster":

![mvapp_pca_data_input2](https://user-images.githubusercontent.com/14832460/32647214-78109808-c5f0-11e7-8303-663bb39cb05e.png)

In the first two sub-tabs in the main panel window you will have the tables that you selected. 

###### Visualizing the PCs

In the third sub-tab, ['Eigenvalues'](https://en.wikipedia.org/wiki/Eigenvalues_and_eigenvectors) tab, the ["Scree plot"](http://www.improvedoutcomes.com/docs/WebSiteDocs/PCA/Creating_a_Scree_Plot.htm) are displayed showing the main principle components generated from the PCA in decreasing order of percentage variance explained by each Principle Component.

![mvapp_pca_eigen](https://user-images.githubusercontent.com/14832460/32647217-788202b8-c5f0-11e7-9f1d-85f44ccc6a10.png)

The table summarizing the eigenvalues of each principle component, their percentage of variance and the cumulative percentages that add to 100% can be found below the plot and can be downloaded as a CSV file.

###### Visualizing the general contribution of DVs per PC

'Contribution per variable' tab displays the contribution of the chosen DVs with respect to two PCs at a time. You can select individual PCs to be plotted on x- and y-axis from the two dropdown menus.

![mvapp_pca_trait_contrib](https://user-images.githubusercontent.com/14832460/32647213-77f03ef0-c5f0-11e7-8033-922fc073a36d.png)

###### Visualizing the contribution of each DV value

By scrolling down, you can see the PC coordinates of the individual samples - the x- and y-axis are controlled by the same dropdown menu as the contribution plots. You can color the plot by any of the Independent Variable to see if you have separation in PC coordinates between your genotype / treatment / time points:

![mvapp_pca_scatter](https://user-images.githubusercontent.com/14832460/32647923-ef68a3c0-c5f3-11e7-9321-ede2f7620bef.png)

###### Visualizing the contribution of each DV per PC

In the sub-tab 'Contribution per PC' the contribution of individual Phenotypes for each PC are displayed. You can download the specific percentange contribution data per PC below the graph:

![mvapp_pca_trait_contrib2](https://user-images.githubusercontent.com/14832460/32647216-78663bdc-c5f0-11e7-82ef-dd418670a6f8.png)

### 7. Cluster analysis => Magda

###### Selecting the data

![mvapp_hcluster_hotmap](https://user-images.githubusercontent.com/14832460/32647242-8e58ae48-c5f0-11e7-85fe-7501207fbfcd.png)

###### What kind of clustering

###### Select the distance for cluster separation

![mvapp_hcluster_dendro](https://user-images.githubusercontent.com/14832460/32647240-8df05d2a-c5f0-11e7-99dc-9b75a5430c96.png)


![mvapp_hcluster_dendro2](https://user-images.githubusercontent.com/14832460/32647243-8e890e94-c5f0-11e7-8312-08bf29ef78b2.png)

###### Validate the cluster

![mvapp_hcluster_clustervalidation](https://user-images.githubusercontent.com/14832460/32647239-8db955f0-c5f0-11e7-9e90-67fe21ae6f65.png)
