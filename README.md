# MVApp
Glittery multi-variate analysis platform for all kinds of data

## Purpose - What is MVApp for?
MVApp was created to streamline data analysis for all kinds of biological queries - from investigating mutant phenotypes or the effects of an experimental treatment, to studying natural variation. 

Although the MVApp development team is armpit-deep in Plant Science we are trying to make the App as applicable as possible for all biological disciplines and beyond...

With the MVApp, we indend to provide a platform allowing:

1. Fitting of simple curves (linear/quadratic/exponential/square root) as well as polynomial curves (cubic and smoothed splines) for datasets with a continuous Independent Variable (e.g. time, dose concentrations, latitude...)
2. Data curation through detection and removal of outliers based on standard statistical methods
3. Data exploration through analysis of distrubution, variance and significant differences across Independent Variables (e.g. individual genotypes, experimental treatment)
5. Correlation Analyses between different Dependent Variables (i.e. phenotypes) across user defined data subsets (e.g. for a given genotype, experimental treatment)
6. Principal Component Analysis and data normalization
7. Cluster Analysis based on selected dependent variables 

## Table of contents:
[1. DATA UPLOAD](#1-data-upload)
#
[2. DATA MODELLING](#2-data-modelling)
#
[3. OUTLIER SELECTION](#3-outlier-selection)
#
[4. DATA EXPLORATION](#4-data-exploration)
#
[5. CORRELATIONS](#5-correlations)
#
[6. PRINCIPLE COMPONENT ANALYSIS](#6-principle-component-analysis)
#
[7. CLUSTER ANALYSIS](#7-cluster-analysis)
#

### 1. DATA UPLOAD

#### How should I format my data?
MVApp can handle all kinds of data. It only requires that your data be in .csv format, with at least the following:
* column with the genotype (if you use only one - please include the name)
* column(s) with an Independent Variable (e.g. treatment, position, experiment number)
* column(s) with a Dependent Variable (i.e. phenotypes)

To fit curves to your data, you should also include columns containing:
* Time (or other continuous Independent Variable) - this variable MUST be numeric (e.g. "1" instead of "Day 1")

To perform data analysis on individual replicates, you should also include a column containing:
* Sample ID - an identifier for each individual sample

Your data should look something like the Example dataset:
![mvapp_data](https://user-images.githubusercontent.com/14832460/32609292-48dc541c-c570-11e7-8f87-54ca02b646d8.png)

#### Upload and annotate your data
To upload your data, navigate to the "Upload your data" tab in the uppermost panel. Click on the "Browse" button and locate your .csv data file:
![mvapp_data_upload1](https://user-images.githubusercontent.com/14832460/32609364-8044b6ce-c570-11e7-9002-dd9f21a700cf.png)

Select the columns pertaining to Genotype, Independent Variables, Dependent Variables and, optionally, continuous Independent Variables and Sample IDs:
![mvapp_data_upload2](https://user-images.githubusercontent.com/14832460/32609363-801c9950-c570-11e7-809b-d9922fb9a606.png)

Finally, click on the "Lock in raw dataset" button to finalise data upload with selected columns and annotations (unselected columns from the original dataset will be dropped at this point). View the newly uploaded dataset in sub-tab "New Data":
![mvapp_data_upload3](https://user-images.githubusercontent.com/14832460/32609362-7ff50520-c570-11e7-8191-d55065fbfdd6.png)

### 2. DATA MODELLING

####  Why model your data?
If you have a continuous Independent Variable in your experiment, you might want to estimate how your Dependent Variables change across it. For example, you could investigate the the dynamics of plant/bacterial growth over time, or the dose dependency of a phenotypic response to a chemical treatment. [Fitting curves](https://en.wikipedia.org/wiki/Curve_fitting) will allow you to observe and model these response dynamics.

At the moment, MVApp helps you to fit simple functions: linear, quadratic, exponential and square root functions. For these functions, we fit linear model (using lm() function) between the continuous Independent Variable indicated in the "Time" column and the Dependent Variable (phenotype). 

Modelling of non-linear functionsis also relies on fitting a linear function, but after transformation of the Dependent Variable:
- square root transformation for fitting quadratic function
- log transformation for fitting exponential function
- quadratic transformation for fitting the square root function

MVApp extracts the model parameters: y-intercept ("INTERCEPT") and the first regression coefficient ("DELTA"), as well as the r2 values to determine model performance.


#### Fit curves
First, in the side panel, select which the Independent Variable(s) you wish to group your samples by, and which Dependent Variable you would like to model. 

If you don't know which function will fit best, you can click on "Unleash model estimation" button. The best model will be indicated based on the r2 values presented in the table:

![mvapp_model_estimation](https://user-images.githubusercontent.com/14832460/32610095-c63f08da-c572-11e7-88a8-65724913ef0a.png)

Decide which model you would like to apply for entire dataset and click "Unleash the model":

![mvapp_model_calculated](https://user-images.githubusercontent.com/14832460/32610094-c60a9d0c-c572-11e7-82f1-9af9fbacf375.png)

Whenever you fit the curves to the chosen Dependent Variable (phenotype), the MVApp will automatically calculate the coefficient of correlation (r2) that will indicate how well the fitted function models the observed data. The number of samples that have a poor fit (r2 < 0.7) will be indicated in the message box above the table.

You can view the lowest r2 values by sorting the samples based on r2 by clicking on the r2 column sorting arrow:

![mvapp_model_rsq_sorted](https://user-images.githubusercontent.com/14832460/32610092-c5e8c0f6-c572-11e7-92ca-7934dd13e8ea.png)

If your data shows signs of complex dynamics across your continuous Independent Variable (often particularly applicable for long time-series), you might consider fitting a polynomial curve.

For the [cubic splines](http://mathworld.wolfram.com/CubicSpline.html), we use lm(phenotype ~ bs(time, knots=X)) function in R, where the position of knots is indicated by the user:

![mvapp_model_cubic](https://user-images.githubusercontent.com/14832460/32610090-c5c3bfcc-c572-11e7-87c2-d4090b8beb0d.png)

For the smoothed splines, we use smooth.spline() function in R, with a choice between automatic or user-defined selection degrees of freedom:

![mvapp_model_smoothed](https://user-images.githubusercontent.com/14832460/32610089-c59f6abe-c572-11e7-90a1-7a50f1dd4cb4.png)


#### Visualise fit-plots
You can view how your data fits to the model by viewing single fit-plots - the names of the samples are merged by "Genotype_IndependentVariable_SampleID". You can either scroll through the sample list or type in the sample name:

![mvapp_model_single_plot](https://user-images.githubusercontent.com/14832460/32610100-c6cd6332-c572-11e7-973f-45a99169dfa8.png)

Alternatively, you can view multiple plots simultaneously. The plots can be sorted by either increasing or decreasing r2 values:

![mvapp_model_multiple](https://user-images.githubusercontent.com/14832460/32610099-c6aa4078-c572-11e7-80b6-98efb766b710.png)

Note: By examining the data, you can select the samples with odd dynamics with respect to other replicates and consider removing them from the dataset. 


#### Assess and compare the dynamics between Genotypes and other Independent Variables
Finally, you can have a look how the calculated DELTAs or Coefficients extracted from the models differ between Genotypes and Independent Variables, by clicking on sub-tab "Examine differences". The message box at the top provides ANOVA results, with the p-value threshold indicated below the graph:

![mvapp_model_summarystats](https://user-images.githubusercontent.com/14832460/32610654-75e89eda-c574-11e7-9cf3-bdb69c4d299f.png)

By scrolling further down, you will find a message containing the significance groups, as calculated per Tukey's pair-wise test, with the same p-value threshold as ANOVA.:

![mvapp_model_summarystats](https://user-images.githubusercontent.com/14832460/32610098-c682f090-c572-11e7-8945-d5257b591ca3.png)


### 3. OUTLIER SELECTION

#### Why identify potential outliers?
For those familiar with large(ish) scale experiments, you have probably had to curate your data, removing [outlier](https://en.wikipedia.org/wiki/Outlier#Working_with_outliers) samples that stem from experimental errors or even mistakes made while recording data. This will help avoid making spurrious conclusions based on unrepresentative data. 

You likely identified these problem samples by simple graphical means, or based on their distance from the median in terms of the Standard Deviation or the Interquartile Range.

MVApp helps to automatically highlight potential outliers based on a single or multiple Dependent Variables, using various approaches. However, be careful, outliers should not be removed mindlessly. It is good practice to justify outlier samples, perhaps refering to notes or images taken during the experiment that might explain the unusual result. It is possible that a "potential outlier" is in fact a valuable, if extreme, result. 


#### Highlight potential outliers
Begin by deciding whether or not to remove samples with missing values by clicking the check-box in the top-left corner of the side-bar.

Then, select the Independent Variable(s) by which to group the samples, and the Dependent Variables you would like to investigate for outliers. By default, MVApp assumes that you would like to group the variables by Genotype, ALL Independent Variables and Time (Sample ID is excluded), across all Dependent Variables.

Next, select which method you would like to use to highlight potential outliers. MVApp provides following methods:
- 1.5x [Interquartile Range](http://www.purplemath.com/modules/boxwhisk3.htm): this is the most commonly used method and is visually very appealing, as the potential outliers will be identified on the boxplot as the "dots" outside of the whiskers
- [Cook's distance](https://en.wikipedia.org/wiki/Cook%27s_distance): this algorithm is actually used to determine the most influential points - meaning the points that could make the most difference in the correlation tests and such. However, if the sample is considered to be "influential" in majority of the traits, it might be worthwhile to check whether everything is OK with it.
- [Bonferoni test](http://polisci.msu.edu/jacoby/icpsr/regress3/lectures/week3/11.Outliers.pdf): this is a test using the car::outlierTest() function from R
- 1x Standard Deviation from the Median - all the samples that are further than 1xSD from the median are highlighted as potential outliers (this test is VERY strict and we do not recommend it)
- 2x Standard Deviation from the Median - all the samples that are further than 2xSD from the median are highlighted as potential outliers
- 2.5x Standard Deviation from the Median - all the samples that are further than 2.5xSD from the median are highlighted as potential outliers
- 3x Standard Deviation from the Median - all the samples that are further than 3xSD from the median are highlighted as potential outliers

![mvapp_outlier_methods](https://user-images.githubusercontent.com/14832460/32647173-51f8df5e-c5f0-11e7-8dc4-8ce1d9da152a.png)

Finally, if you decided to select outliers based on all Dependent Variables, use the slider input in the side panel to select the number of Dependent Variables a given sample must be an outlier in order to consider it an outlier across the whole experiment, i.e. the samples that extreme across so many phenotypes that they warrant being removed from the data analysis.

![mvapp_outlier_output1](https://user-images.githubusercontent.com/14832460/32647171-51b52f48-c5f0-11e7-8a46-6ae78ad193c3.png)

Click on "Unleash outlier highlighter" to see the table with highlighted outliers. This might take a while - depending on the size of your dataset.

In the main tab, the outlier message will appear indicating the number of potential outliers highlightes, as well as a table of your data. If you scroll to the right, you will see the columns marked "outl_Dependent Variable" (for example "outl_AREA"), where "true" will indicate this sample as being an outlier per genotype / day / independent variables selected. If considering all Dependent Variables, the final column will indicate whether a given sample is a potential outlier in a number of Dependent Variables that meets or exceeds the user-defined threshold (annotated as "true").


#### Examine the data with and without potential outliers
After highlighting potential outliers in your data, you can look at how the data looks with and without them.

Go to sidepanel sub-tab "Tweak the graphs" and select the Dependent Variable you wish to examine. You can also select the type of plot you would like: box plot, scatter plot or bar plot (we find box plots most informative).

Click on the main panel sub-tab "Graph containing outliers" to see your plots prior to removing potential outliers: 

![mvapp_outlier_graph_bugs](https://user-images.githubusercontent.com/14832460/32647170-517e6c4c-c5f0-11e7-99a1-89a3adc59e1c.png)

If you wish to color code your samples or split the plots based on your Independent Variables, you can do this in the side panel:

![mvapp_outlier_graph_facet_color](https://user-images.githubusercontent.com/14832460/32647168-507f1f26-c5f0-11e7-9f9f-c0f162cf7f95.png)

If you want to alter the order of your samples, you can swap the order of the Independent Variables in the sidebar sub-panel "Outlier selection":

![mvapp_outlier_graph_even_nicer](https://user-images.githubusercontent.com/14832460/32647167-5066effa-c5f0-11e7-9388-9620294702ff.png)


#### Compare the data with outlier removed
If you want to look at the graphs with potential outliers removed (as highlighted in main panel "The outliers test"), click on the main panel "Graphs with outliers removed". You can click between "Graph containing outliers" and "Graph with outliers removed" to compare both datasets.

![mvapp_outliers_graph_no_outliers](https://user-images.githubusercontent.com/14832460/32647177-528bf596-c5f0-11e7-89dd-a8c85a287a47.png)

IMPORTANT NOTE: This outlier test was developed to facilitate data curation. Please do NOT remove any data before making absolutely sure that there is a very good reason that the sample is not representative. We recommend downloading the dataset with outliers highlighted , manually removing samples that you can reasonably explain, and reuploading the curated dataset before continuing with your analysis.

PICTURE OF MOUSE EATING A PLANT


#### Calculate summary statistics
MVApp can calculate [summary statistics](https://en.wikipedia.org/wiki/Summary_statistics) functions (e.g. mean, median, standard deviation) in the main panel sub-tab "Summary data".

Select the dataset in the upper left corner of the tab, and select the functions that you want to be calculated in the upper right corner.

NOTE: If you removed missing values prior to outlier selection, the dataset without outliers will NOT contain missing values. If you did not removed missing values, the dataset with outliers removed WILL contain missing values. 

![mvapp_outlier_summ_stats](https://user-images.githubusercontent.com/14832460/32647166-503e1d64-c5f0-11e7-98ff-54ef9cc5fa02.png)

Click on "Unleas Summary Statistics" and the table will appear in the main panel:

![mvapp_outlier_summstats2](https://user-images.githubusercontent.com/14832460/32647174-52602bd2-c5f0-11e7-87f7-616859da1044.png)


### 4. DATA EXPLORATION
Once your data is nice and clean and ready to go, it's time to start having a proper look at it. A good place to start is to check out how your data is distributed using histograms and boxplots, grouping samples according to your various Independent Variables. From these you can get an idea of how your different genotypes are behaving, how your treatments are affecting your phenotypes, how variable your data is. 

Beyond eyeballing, you can apply statistical tests such as ANOVA to test whether there are significant differences between groups. These are all easy things to do in MVApp, which also helps you check the assumptions of these statistical tests, such as normal distribution and homoscedasticity (i.e. equal variance).

In the side panel, you can choose:
- The dataset to be used (raw data, data with missing values removed, or data with outliers removed). The default value is raw data.
- The Independent Variable to subset the data by
- The Dependent Variable you want to plot
- The p-value threshold to be used in subsequent tests in the tab (e.g.: Levene's test, ANOVA, etc.). The default value is 0.05
- If you want to facet your graphs by another Independent Variable, tick the checkbox "Would you like to facet the graph?" a dropdown menu with a list of independent variables appears. The user can choose the independent variable to split the plot.

<img width="1115" alt="data exploration input choices" src="https://user-images.githubusercontent.com/15339112/32780896-6bf2d2aa-c8ef-11e7-81f1-45add2e36d98.png">

Once the choices are made, the user can proceed to the different tests available in the DATA EXPLORATION tab.

#### Examine distribution
Start in the side-panel by selecting the dataset you want to examine, the Independent Variable you want to group your samples by, and the Dependent Variable you want to plot. If you want to split the graphs by another Independent Variable, tick the "Split graph?" checkbox and select the Independent Variable in the dropdown menu that appears. You can also select the p-value threshold for the subsequent statistical tests.

Your histograms will appear in the "Testing normal distribution" sub-tab, where you can chose between having "Histograms with  counts on y-axis" or "Histograms with density on y-axis".

From these plots, you can look at the spread of your data across the Independent Variable groupings selected in the side-panel. Below the histograms, you will find a message that summarizes the groups/subgroups that seem to not have a normal distribution, where the p-value of the Shapiro-Wilk test is larger than the p-value threshold selected in the side-panel. Normal distribution is a requirement for performing an ANOVA test (less so for large sample sizes).

<img width="1119" alt="testing normal distribution" src="https://user-images.githubusercontent.com/15339112/32780913-79ca2e14-c8ef-11e7-85e9-cdec4d163525.png">

If you want to see the detailed results of the Shapiro-Wilk test for all groups/subgroups along with their QQ-plots, tick the checkbox "See detailed Shapiro-Wilk test and QQ-plots". The table shows p-value of the Shapiro-Wilk tests performed for each groups/subgroups. If the p-value of the Shapiro-Wilk test for a group is larger than the selected p-value threshold, in the final column the group will be noted with "Data has NORMAL distribution" appears.

In this case there is not enough evidence to reject the null hypothesis, where the data is considered to be sampled from a normally distributed population. If the p-value of the Shapiro-Wilk test for a group is smaller than the selected p-value threshold, a message "Data might NOT be normally distributed" appears. In this case the null hypothesis is rejected, meaning the data appears not to be sampled from a normally distributed population. However, sample size affects the Shapiro-Wilk test and hence (the more the merrier), the user is strongly encouraged to check the QQ-plots.

<img width="1038" alt="shapirowilk detailed and qq" src="https://user-images.githubusercontent.com/15339112/32780927-82b5c4ca-c8ef-11e7-8cf0-e95da533b0d5.png">

If you ticked the "See detailed Shapiro-Wilk test and QQ-plots?" checkbox, sliders for QQ-plots also appear. These sliders help choose the optimum number of columns and plots for display. The first slider " Display QQ plots in ... columns:" allows the user to choose the number of columns for the display of the QQ-plots. If the number of plots is too large to be displayed all at once in the window, a second slider "Plot portion of the data starting from element number..." appears and the user can choose the portion of plots to be displayed. 

<img width="1075" alt="qqplot slides" src="https://user-images.githubusercontent.com/15339112/32780970-a9d7add4-c8ef-11e7-9622-4dbb3fac5f76.png">

Based on the results obtained in this sub-tab, you can have a better judgement in the following sub-tab whether to check Bartlett or Levene test for equal variances.

#### Examine variance
In the "Testing equal variance" sub-tab, you can have a look at the results of the Bartlett test and Levene test of homogeneity of variances between the different groups and for each sub-groups. Equal variances, or homoscedasticity, is also a requirement for performing an ANOVA test.

The null hypothesis of the Bartlett test and Levene tests assumes that the variances in each of the groups are the same. The Bartlett test is more robust when the data comes from a normal distribution, while Levene test is more robust in case of departures from normality.

<img width="1113" alt="bartlett and levene test result" src="https://user-images.githubusercontent.com/15339112/32780973-b0b08f86-c8ef-11e7-865b-c87f88cb2a8a.png">

The first table displays the results of the Bartlett test and the second table displays those of the Levene test. The tables show the p-value of tests performed for each groups/subgroups. If the p-value of the test for a group is larger than the selected p-value threshold, groups are noted as "Equal". In this case there is not enough evidence to reject the null hypothesis, where the variances are considered equal. If the p-value of the test for a group is smaller than the selected p-value threshold, groups are noted as "Not equal". In this case the null hypothesis is rejected and the variances are considered not equal.

As indicated previously, the results of this sub-tab and the previous sub-tab are needed to for the ANOVA test performed in the following sub-tab. ANOVA assumes the data comes from a normal distribution and the variances are equal.

#### Test significant differences between groups
In this sub-tab, you can check for signifcant differences in the means between different groups using analysis of variance (ANOVA). A table displays the p-value of the ANOVA test between different levels of the Independent Variable.

<img width="1118" alt="testing significant differences_1" src="https://user-images.githubusercontent.com/15339112/32781415-6d3a68f6-c8f1-11e7-9f33-37b8bc9812ba.png">

If the p-value of the ANOVA test for a group is larger than the selected p-value threshold, groups are noted with "Cannot reject H0". In this case there is not enough evidence to reject the null hypothesis and the means of the groups are assumed equal. If the p-value of the ANOVA test for a group is smaller than the selected p-value threshold, groups are noted with "Significant difference in means". In this case the null hypothesis, where the means of the group are considered equal, is rejected and the means of the groups can be considered significantly different. 

Boxplots display the distribution of the data for a specific trait (dependent variable) for the levels of the independent variable. The boxplots are split by the second Independent Variable, selected when if you ticked the checkbox for "Split graph?". 

A second table displays the significant groups based on Tukey's pairwise comparison. Groups that share a common letter do not have significantly different means for the selected Dependent Variable. In the example below, there is no significant differences in PERIMETER between the salt and control groups for the Genotype C24. While there is a significant difference in PERIMETER between the salt and control groups for the Genotype Te.

<img width="354" alt="tukey letters" src="https://user-images.githubusercontent.com/15339112/32781182-8d4c2360-c8f0-11e7-931c-2d64589dc1ce.png">


### 5. CORRELATIONS
This tab is to check how correlated the selected dependent variables (phenotypes) are in your data by creating a correlation matrix of the selected variables. Correlation coefficientss and p.values are provided for each variable pair. 

#### Select the dataset
Select which dataset you would like to use to perform the correlation analysis. You can choose from the following options: 
raw data: use your selected data from “Upload your data” tab, you can check the data from the Data Magic - New data tab
missing values removed: use data without rows with missing values outliers removed: use outlier-free data from “Data curation” tab

#### Select the correlation method
There are two methods you can choose from to calculate correlations. The most common method is [Pearson] (https://en.wikipedia.org/wiki/Pearson_correlation_coefficient), but you can also use [Spearman correlation](https://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient).

<img width="960" alt="screenshot1" src="https://user-images.githubusercontent.com/8470158/33008572-110563d6-cd80-11e7-84be-084880b31d2c.png">

#### Correlation for subsetted data
The default is to perform correlation analysis across all dependent variables (phenotypes) across all independent variables. You can also choose to use a subset your data (for examples, phenotypes under a certain treatment, or from a certain day) to examine the correlation. 

Tick the checkbox “Subset your data for correlation analysis” and choose the condition to want to observe by choosing from the dropdown menu.

<img width="960" alt="screenshot3" src="https://user-images.githubusercontent.com/8470158/33008571-10e523d2-cd80-11e7-9f59-2d6f6f95d590.png">

#### Customize the plot
You can choose different plot method, plot type and label orders to customize your correlation plot by choosing from the dropdown menus on the left-side panel.

<img width="960" alt="screenshot2" src="https://user-images.githubusercontent.com/8470158/33008574-1146ca74-cd80-11e7-9270-e9b7bb13ebb7.png">

#### Scatterplots

To visualize the dependent variable data on a scatterplot,
  -choose the fist phenotype to be plotted on the X-axis
  -choose the second phenotype to be plotted on the Y-axis
You can also choose to color your data points by choosing a certain independent variable (such as day, or treatment).

<img width="960" alt="screenshot4" src="https://user-images.githubusercontent.com/8470158/33008573-1125b578-cd80-11e7-9676-9ac52742f056.png">

### 6. PRINCIPLE COMPONENT ANALYSIS
Principle component analysis [(PCA)](https://en.wikipedia.org/wiki/Principal_component_analysis) is often used to simplify the data into fewer dimensions, and also to check which traits explain majority of the variation in the population studied. However, the PCA is often not explored to its full potential. You can for example run PCA on data subsetted by an Independent Variable (e.g.treatment or genotype) and run PCA separately on those subsets to see how much each of your Dependent Variables contributes to explaining observed variation. MVApp will allows you to do all this!

#### Select data, subsets, and Dependent Variables
Select the dataset to analyse from the dropdown menu at the top of the side panel and click "Set the dataset":

![mvapp_pca_data_input1](https://user-images.githubusercontent.com/14832460/32647215-7829f348-c5f0-11e7-8416-c339543a8e9f.png)

Then, select which Dependent Variables you want to use in the PCA and click "Unleash the PCA monster":

![mvapp_pca_data_input2](https://user-images.githubusercontent.com/14832460/32647214-78109808-c5f0-11e7-8303-663bb39cb05e.png)

You can see your selected datasets in the first two sub-tabs.

#### Visualize the PCs
In the third sub-tab, [Eigenvalues](https://en.wikipedia.org/wiki/Eigenvalues_and_eigenvectors), the ["Scree plot"](http://www.improvedoutcomes.com/docs/WebSiteDocs/PCA/Creating_a_Scree_Plot.htm) are displayed showing the main principle components generated from the PCA in decreasing order of percentage variance explained by each Principle Component.

![mvapp_pca_eigen](https://user-images.githubusercontent.com/14832460/32647217-788202b8-c5f0-11e7-9f1d-85f44ccc6a10.png)

The table summarizing the eigenvalues of each Principle Component (PC), their percentage of variance EXPLAINED and the cumulative percentages that add to 100% can be found below the plot and can be downloaded as a .csv file.

#### Visualize the general contribution of DVs per PC
The 'Contribution per variable' sub-tab displays the contribution of the chosen Dependent Variables with respect to two PCs at a time. You can select individual PCs to be plotted on x- and y-axis from the two dropdown menus.

![mvapp_pca_trait_contrib](https://user-images.githubusercontent.com/14832460/32647213-77f03ef0-c5f0-11e7-8033-922fc073a36d.png)

#### Visualize the contribution of each DV value
By scrolling down, you can see the PC coordinates of the individual samples. The x- and y-axis are controlled by the same dropdown menu as the contribution plots. You can color the plot by any of the Independent Variable to see if you have separation in PC coordinates between your genotype / treatment / time points:

![mvapp_pca_scatter](https://user-images.githubusercontent.com/14832460/32647923-ef68a3c0-c5f3-11e7-9321-ede2f7620bef.png)

#### Visualize the contribution of each DV per PC
In the sub-tab 'Contribution per PC' the contribution of individual Dependent Variable for each PC are displayed. You can download the specific percentange contribution data per PC below the graph:

![mvapp_pca_trait_contrib2](https://user-images.githubusercontent.com/14832460/32647216-78663bdc-c5f0-11e7-82ef-dd418670a6f8.png)

### 7. CLUSTER ANALYSIS

#### Selecting the data

![mvapp_hcluster_hotmap](https://user-images.githubusercontent.com/14832460/32647242-8e58ae48-c5f0-11e7-85fe-7501207fbfcd.png)

#### What kind of clustering

#### Select the distance for cluster separation

![mvapp_hcluster_dendro](https://user-images.githubusercontent.com/14832460/32647240-8df05d2a-c5f0-11e7-99dc-9b75a5430c96.png)

![mvapp_hcluster_dendro2](https://user-images.githubusercontent.com/14832460/32647243-8e890e94-c5f0-11e7-8312-08bf29ef78b2.png)

###### Validate the cluster

![mvapp_hcluster_clustervalidation](https://user-images.githubusercontent.com/14832460/32647239-8db955f0-c5f0-11e7-9e90-67fe21ae6f65.png)
