# MVApp
Glittery multivariate analysis platform for all kinds of data

The app is available [here](http://mvapp.kaust.edu.sa/MVApp/) or you can run it locally from your device by typing the following command in your R window:

`install.packages(shiny)`
`library(shiny)`
`shiny::runGitHub("mmjulkowska/Salt_NV_RootApp", "mmjulkowska")`

(....it will take some time for the first time to upload all the libraries)

## Purpose statement - What is MVApp for?

MVApp was created to streamline data analysis for all kinds of biological queries - from investigating mutant phenotypes or the effects of an experimental treatment, to studying natural variation using any biological system.  

We believe that the MVApp will enhance data transparency and standardize data curation and analysis, save time, and empower the scientific community to perform complex analyses without extensive knowledge of R or statistics, while simultaneously it will improve the data analysis literacy in wider scientific community. 

Although the MVApp development team is burried armpit-deep in Plant Science, we are trying to make the App as applicable as possible for all biological disciplines and beyond. If you have any suggestions on what other analyses we can include, please check out our guidelines on how to [contribute](https://github.com/mmjulkowska/MVApp/blob/master/CONTRIBUTING.md). 

Currently MVApp has following features:
1. Identification of outliers using different methods based on one or multiple phenotypes 
2. Summary of the data dynamics by fitting simple functions or polynomial curves to data points
3. Hypothesis testing using parametric and non-parametric tests, including testing the assumptions of normality and equal variance 
4. Correlation analysis of measured traits within the specific subsets, as well as for the entire experiment
5. Reduction of data dimensionality and identifying the traits that explain the most data variance using principal component analysis and multidimensional scaling 
6. Clustering individual samples using hierarchical or k-means clustering 
7. Estimation of broad-sense heritability of measured traits 
8. Quantile regression analysis that allowa the identification of traits with significant contribution to traits of major interest

### How to cite the MVApp:

The app is not published anywhere (yet - we are working on it), but if you wish to cite the app, please use the following:
Julkowska, M.M., Saade, S., Gao, G., Morton, M.J.L., Awlia, M., Tester, M.A., "MVApp.pre-release_v2.0 mmjulkowska/MVApp: MVApp.pre-release_v2.0", DOI: 10.5281/zenodo.1067974

## Table of contents:
[1. DATA UPLOAD](#1-data-upload)


[2. CURVE FITTING](#2-curve-fitting)


[3. OUTLIER SELECTION](#3-outlier-selection)


[4. DATA EXPLORATION](#4-data-exploration)


[5. CORRELATIONS](#5-correlations)


[6. REDUCTION IN DIMENSIONALITY](#6-reduction-in-dimensionality)


[7. CLUSTER ANALYSIS](#7-cluster-analysis)


[8. HERITABILITY](#8-heritability)


[9. QUANTILE REGRESSION](#9-quantile-regression)


### 1. DATA UPLOAD

#### Data format:

MVApp can handle .csv files containing at least following collumns: 
* column with the genotype or the main Independent Variable (if you use only one genotype - we advise you to include one column with the genotype anyway and give the same name to all of your samples)
* One or multiple column(s) with an Independent Variable (e.g. treatment, position, experimental batch number)
* One or multiple column(s) containing Dependent Variable - numerical data of the measured traits - also known as the phenotypes

If you have a timeseries experiment, or any other gradient, and you want to fit curves to your data, the input data should include columns containing:
* Time (or other continuous Independent Variable) - this variable MUST be numeric (e.g. "1" instead of "Day 1")
* Sample ID - an identifier for each individual sample

Your data should look similar like the Example dataset, with ID and TIME column being optional:
![mvapp_data](https://user-images.githubusercontent.com/14832460/32609292-48dc541c-c570-11e7-8f87-54ca02b646d8.png)

#### Upload and annotate your data
To upload your data, navigate to the "Upload your data" tab in the uppermost panel. Click on the "Browse" button and locate your .csv data file:

![01_data_upload_01](https://user-images.githubusercontent.com/14832460/38307578-cbd420d8-381c-11e8-919b-a14eeec20648.png)

Select the columns pertaining to Genotype, Independent Variables, Dependent Variables (phenotypes):

![01_data_upload_02](https://user-images.githubusercontent.com/14832460/38307583-cda4fc3e-381c-11e8-8437-70812471e1cf.png)

![01_data_upload_03](https://user-images.githubusercontent.com/14832460/38307589-d143890a-381c-11e8-8ab7-cc1fbb00996c.png)

Optionally, continuous Independent Variables and Sample IDs:

![01_data_upload_04](https://user-images.githubusercontent.com/14832460/38307590-d164a694-381c-11e8-9a15-5b05c9123e61.png)

Finally, click on the "Click to set the data" button to finalise data upload with selected columns and annotations (unselected columns from the original dataset will be dropped at this point). 

View the newly uploaded dataset in "New Data" sub-tab:

![01_data_upload_05](https://user-images.githubusercontent.com/14832460/38307591-d188f8aa-381c-11e8-82fd-4ad52159d942.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

### 2. CURVE FITTING

####  Why model your data?

If you have a continuous Independent Variable in your experiment, you might want to estimate how your Dependent Variables change across it. For example, you could investigate the the dynamics of plant/bacterial growth over time, or the dose dependency of a phenotypic response to a chemical treatment. [Fitting curves](https://en.wikipedia.org/wiki/Curve_fitting) will allow you to observe and model these response dynamics.

#### Fitting simple functions

At the moment, MVApp helps you to fit simple functions: linear, quadratic, exponential and square root functions. For these functions, we fit linear model (using lm() function) between the continuous Independent Variable indicated in the "Time" column and the Dependent Variable (phenotype). 

Modelling of non-linear functionsis also relies on fitting a linear function, by transforming the Dependent Variable, so the linear model can be fitted:
- square root transformation for fitting quadratic function
- log transformation for fitting exponential function
- quadratic transformation for fitting the square root function

MVApp extracts the model parameters: y-intercept ("INTERCEPT") and the first regression coefficient ("DELTA"), as well as the r2 values to determine model performance.

#### Fit curves with MVApp

First, in the side panel, select which the Independent Variable(s) you wish to group your samples by, and which Dependent Variable you would like to model. 

![02_curve_fit_01](https://user-images.githubusercontent.com/14832460/38308597-4c0e8f10-3820-11e8-90f2-6de049425f80.png)

![02_curve_fit_02](https://user-images.githubusercontent.com/14832460/38308598-4c2f6186-3820-11e8-9875-6485bdcebbe8.png)

If you don't know which function will fit best, you can click on "Unleash model estimation" button. The best model will be indicated based on the r2 values presented in the table:

![02_curve_fit_03](https://user-images.githubusercontent.com/14832460/38308599-4c518a86-3820-11e8-91e4-70f2e6370f9d.png)

Decide which model you would like to apply for entire dataset and click "Unleash the model":

![02_curve_fit_04](https://user-images.githubusercontent.com/14832460/38308600-4c72bfbc-3820-11e8-88aa-a2f192ecdd2d.png)

Whenever you fit the curves to the chosen Dependent Variable (phenotype), the MVApp will automatically calculate the coefficient of correlation (r2) that will indicate how well the fitted function models the observed data. The number of samples that have a poor fit (r2 < 0.7) will be indicated in the message box above the table:

![02_curve_fit_05](https://user-images.githubusercontent.com/14832460/38308602-4c9905fa-3820-11e8-9037-42538b3f446a.png)

You can change the threshold of the r2 by changing the r2 cut-off value:

![02_curve_fit_06](https://user-images.githubusercontent.com/14832460/38308603-4cbbfc5e-3820-11e8-99a2-bad2d4b10339.png)

You can view the lowest r2 values by sorting the samples based on r2 by clicking on the r2 column sorting arrow:

![02_curve_fit_07](https://user-images.githubusercontent.com/14832460/38308604-4cdb8f6a-3820-11e8-9a6a-f5249ad58ca4.png)

You can download your data without the samples showing poor r2 fit by scrolling down and clicking on "Download curated data with r2 > cut-off" button:

![02_curve_fit_08](https://user-images.githubusercontent.com/14832460/38308605-4d0221b6-3820-11e8-88b5-1d7870c359d6.png)

#### Visualise goodness of fit of the dynamic curves with fit-plots

You can examine how good your data fits to the selected model by viewing fit-plots - the names of the samples are merged by "Genotype_IndependentVariable_SampleID". You can either scroll through the sample list or type in the sample name:

![02_curve_fit_09](https://user-images.githubusercontent.com/14832460/38308606-4d28e918-3820-11e8-89b2-fe4b26a8a8f9.png)

If you are having trouble interpreting the graphs, or need a quick figure legend describing the graph, you can select "show figure legend" for the default version of figure legend to pop-up:

![02_curve_fit_10](https://user-images.githubusercontent.com/14832460/38308607-4d4a1cdc-3820-11e8-80cd-fc991e1b7ae7.png)

You can view multiple fit-plots simultaneously. The plots can be sorted by either increasing or decreasing r2 values:

![02_curve_fit_11](https://user-images.githubusercontent.com/14832460/38308609-4d80554a-3820-11e8-94bc-809ef75d75ed.png)

You can scroll through the individual graphs with the slider on the right side of the main window:

![02_curve_fit_12](https://user-images.githubusercontent.com/14832460/38308610-4da68ec2-3820-11e8-9350-6f8f8c362304.png)

#### Assess and compare the dynamics between Genotypes and / or Independent Variables

Finally, you can have a look how the calculated DELTAs or Coefficients extracted from the models differ between your genotypes and other Independent Variables, such as "treatment". 

You can examine the differences by clicking on sub-tab "Examine differences". The message box at the top provides ANOVA results, with the p-value threshold indicated below the graph:

![02_curve_fit_13](https://user-images.githubusercontent.com/14832460/38308611-4dcabe1e-3820-11e8-9169-5a1741548029.png)

You can use raw data, or data with r2 above the threshold value:

![02_curve_fit_14](https://user-images.githubusercontent.com/14832460/38309174-fdb03c68-3821-11e8-83ce-920b2b75ad3e.png)

By scrolling further down, you will find a panel to control the design of the graph, as well as threshold p-value for the ANOVA:

![02_curve_fit_15](https://user-images.githubusercontent.com/14832460/38309240-2cfde01a-3822-11e8-8e77-27cc3e8cd755.png)

You can change the graph to for example bar graph, remove background or determine what is represented by the error bars. The default figure legend will update automatically too:

![02_curve_fit_18](https://user-images.githubusercontent.com/14832460/38309331-78926dfc-3822-11e8-803f-15762ed632b4.png)

By scrolling further down, you can find the significant groups, as calculated per Tukey's pair-wise test, with the same p-value threshold as ANOVA:

![02_curve_fit_20](https://user-images.githubusercontent.com/14832460/38309459-cb90b0e0-3822-11e8-9dd6-bb4c830b999c.png)

By scrolling down even further, you will find a table containing the summary statistics for your data for all fitted values:

![02_curve_fit_21](https://user-images.githubusercontent.com/14832460/38309493-db7246ae-3822-11e8-969e-5ab3def193c6.png)

#### Fit polynomial curves with MVApp

If your data shows signs of complex dynamics across your continuous Independent Variable (often particularly applicable for long time-series), you might consider fitting a polynomial curve.

For the [cubic splines](http://mathworld.wolfram.com/CubicSpline.html), we use lm(phenotype ~ bs(time, knots=X)) function in R, where the position of knots is indicated by the user:

![mvapp_model_cubic](https://user-images.githubusercontent.com/14832460/32610090-c5c3bfcc-c572-11e7-87c2-d4090b8beb0d.png)

For the smoothed splines, we use smooth.spline() function in R, with a choice between automatic or user-defined selection degrees of freedom:

![mvapp_model_smoothed](https://user-images.githubusercontent.com/14832460/32610089-c59f6abe-c572-11e7-90a1-7a50f1dd4cb4.png)


[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

### 3. OUTLIER SELECTION

#### Why identify potential outliers?

For those familiar with large(ish) scale experiments, you have probably had to curate your data, removing [outlier](https://en.wikipedia.org/wiki/Outlier#Working_with_outliers) samples that stem from experimental errors or even mistakes made while recording data. This will help avoid making spurrious conclusions based on unrepresentative data. 

You likely identified these problem samples by simple graphical means, or based on their distance from the median in terms of the Standard Deviation or the Interquartile Range.

MVApp helps to automatically highlight potential outliers based on a single or multiple Dependent Variables, using various approaches. However, be careful, outliers should not be removed mindlessly. It is good practice to justify outlier samples, perhaps refering to notes or images taken during the experiment that might explain the unusual result. It is possible that a "potential outlier" is in fact a valuable, if extreme, result. 

#### Highlight potential outliers

Begin by deciding which data-set you would like to use. Whether or not to remove samples with missing values or (in case you have performed curve fitting) dataset with curated r2 values:

![03_outliers_01](https://user-images.githubusercontent.com/14832460/38309888-d661cff8-3823-11e8-8c59-c3dd645f64a3.png)

![03_outliers_02](https://user-images.githubusercontent.com/14832460/38309889-d6870818-3823-11e8-8814-b65f44427386.png)

Select the Independent Variable(s) by which to group the samples, and whether you would like to select outliers based on one, some or all phenotypes / measured traits:

![03_outliers_03](https://user-images.githubusercontent.com/14832460/38310382-0f991b2c-3825-11e8-9d1d-2eb76e13a374.png)

Next, select which method you would like to use to highlight potential outliers. MVApp provides following methods:
- 1.5x [Interquartile Range](http://www.purplemath.com/modules/boxwhisk3.htm): this is the most commonly used method and is visually very appealing, as the potential outliers will be identified on the boxplot as the "dots" outside of the whiskers
- [Cook's distance](https://en.wikipedia.org/wiki/Cook%27s_distance): this algorithm is actually used to determine the most influential points - meaning the points that could make the most difference in the correlation tests and such. However, if the sample is considered to be "influential" in majority of the traits, it might be worthwhile to check whether everything is OK with it.
- [Bonferoni test](http://polisci.msu.edu/jacoby/icpsr/regress3/lectures/week3/11.Outliers.pdf): this is a test using the car::outlierTest() function from R
- 1x Standard Deviation from the Median - all the samples that are further than 1xSD from the median are highlighted as potential outliers (this test is VERY strict and we do not recommend it)
- 2x Standard Deviation from the Median - all the samples that are further than 2xSD from the median are highlighted as potential outliers
- 2.5x Standard Deviation from the Median - all the samples that are further than 2.5xSD from the median are highlighted as potential outliers
- 3x Standard Deviation from the Median - all the samples that are further than 3xSD from the median are highlighted as potential outliers

![03_outliers_04](https://user-images.githubusercontent.com/14832460/38309891-d6d85d3a-3823-11e8-8422-a38d88a8fdc9.png)

You can now click on the "unleash the outlier highlighter" to view the curated data. In the main tab, the outlier message will appear indicating the number of potential outliers highlightes, as well as a table of your data. If you scroll to the right, you will see the columns marked "outl_Dependent Variable" (for example "outl_AREA"), where "true" will indicate this sample as being an outlier per genotype / day / independent variables selected. If considering all Dependent Variables, the final column will indicate whether a given sample is a potential outlier in a number of Dependent Variables that meets or exceeds the user-defined threshold (annotated as "true").
The number of identified outliers will be shown in the text box above the table:


If you decided to select outliers based on all Dependent Variables, use the slider input in the side panel to select the number of Dependent Variables a given sample must be an outlier in order to consider it an outlier across the whole experiment, i.e. the samples that extreme across so many phenotypes that they warrant being removed from the data analysis.

![mvapp_outlier_output1](https://user-images.githubusercontent.com/14832460/32647171-51b52f48-c5f0-11e7-8a46-6ae78ad193c3.png)


#### Examine the data with and without potential outliers
After highlighting potential outliers in your data, you can look at how the data looks with and without them.

Go to sidepanel sub-tab "Tweak the graphs" and select the Dependent Variable you wish to examine. You can also select the type of plot you would like: box plot, scatter plot or bar plot (we find box plots most informative).

Click on the main panel sub-tab "Graph containing outliers" to see your plots prior to removing potential outliers: 

![03_outliers_08](https://user-images.githubusercontent.com/14832460/38309897-d79b8594-3823-11e8-9d6b-dbbfcc86d80a.png)

You can reduce the number of genotypes you are examining simultaneously by changing the slider "Show ... number of samples":

![03_outliers_09](https://user-images.githubusercontent.com/14832460/38309898-d7de9ab4-3823-11e8-8dc5-fadf8e51e148.png)

You can change the portion of the data that you examine by changing the slider "Plot portion of the data starting from the element number ...":

![03_outliers_10](https://user-images.githubusercontent.com/14832460/38309899-d80d3aae-3823-11e8-8f90-a5ac6e4cb43d.png)

If you wish to color code your samples or split the plots based on your Independent Variables, you can do this in the side panel:

![03_outliers_11](https://user-images.githubusercontent.com/14832460/38309900-d83248f8-3823-11e8-9234-57fe8fec05ca.png)

If you want to alter the order of your samples, you can swap the order of the Independent Variables in the sidebar sub-panel "Outlier selection":

![mvapp_outlier_graph_even_nicer](https://user-images.githubusercontent.com/14832460/32647167-5066effa-c5f0-11e7-9388-9620294702ff.png)


#### Compare the data with outlier removed
If you want to look at the graphs with potential outliers removed (as highlighted in main panel "The outliers test"), click on the main panel "Graphs with outliers removed". You can click between "Graph containing outliers" and "Graph with outliers removed" to compare both datasets.

![mvapp_outliers_graph_no_outliers](https://user-images.githubusercontent.com/14832460/32647177-528bf596-c5f0-11e7-89dd-a8c85a287a47.png)

IMPORTANT NOTE: This outlier test was developed to facilitate data curation. Please do NOT remove any data before making absolutely sure that there is a very good reason that the sample is not representative. We recommend downloading the dataset with outliers highlighted , manually removing samples that you can reasonably explain, and reuploading the curated dataset before continuing with your analysis.

#### Calculate summary statistics

MVApp can calculate [summary statistics](https://en.wikipedia.org/wiki/Summary_statistics) functions (e.g. mean, median, standard deviation) in the main panel sub-tab "Summary data".

Select the dataset that you would like to use in the upper left corner of the main tab, and select the functions that you want to be calculated in the upper right corner. After selecting everything click "Unleash summary statistics":

![03_outliers_17](https://user-images.githubusercontent.com/14832460/38311203-472c28a2-3827-11e8-846a-f89b377fb732.png)

Table containing all the calculations will appear in the main panel. You can download the data into your computer by clicking "Download summary statistics data" button:

![03_outliers_19](https://user-images.githubusercontent.com/14832460/38311204-474fdb62-3827-11e8-9852-b12780b37807.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

### 4. DATA EXPLORATION

Once your data is nice and clean and ready to go, it's time to start having a proper look at it. A good place to start is to check out how your data is distributed using histograms and boxplots, grouping samples according to your various Independent Variables. From these you can get an idea of how your different genotypes are behaving, how your treatments are affecting your phenotypes, how variable your data is. 

Beyond eyeballing, you can apply statistical tests such as ANOVA to test whether there are significant differences between groups. These are all easy things to do in MVApp, which also helps you check the assumptions of these statistical tests, such as normal distribution and homoscedasticity (i.e. equal variance).

In the side panel, you can choose:
- The dataset to be used (raw data, data with missing values removed, or data with outliers removed). The default value is raw data.
- The Independent Variable to subset the data by
- The Dependent Variable you want to plot
- The p-value threshold to be used in subsequent tests in the tab (e.g.: Levene's test, ANOVA, etc.). The default value is 0.05
- If you want to facet your graphs by another Independent Variable, tick the checkbox "Split the graph?" a dropdown menu with a list of independent variables appears. The user can choose the independent variable to split the plot.


![04_explore_01](https://user-images.githubusercontent.com/14832460/38313740-731447a0-382d-11e8-974c-e26a14023931.png)

Once the choices are made, the user can proceed to the different tests available in the DATA EXPLORATION tab.

#### Examine distribution

Your histograms will appear in the "Testing normal distribution" sub-tab, where you can chose between having "Histograms with  counts on y-axis" or "Histograms with density on y-axis".

![04_explore_03](https://user-images.githubusercontent.com/14832460/38313742-735eaf48-382d-11e8-81cf-c46d74818313.png)

![04_explore_04](https://user-images.githubusercontent.com/14832460/38313743-7384a234-382d-11e8-96a7-2b5427adb3ca.png)

You can split the graphs by Independent Variable, by ticking the box "Split the graph?" and chosing the Independent Variable that you would like to use for splitting:

![04_explore_05](https://user-images.githubusercontent.com/14832460/38313744-73a72ad4-382d-11e8-81ec-6dc65e9fa85a.png)

You can subset your data even further, by ticking the box "Subset the data?" in the main windown, and selecting yet another one of the Independent Variables, and selecting specific value of this variable that will be used for the displayed graphs:

![04_explore_06](https://user-images.githubusercontent.com/14832460/38313746-73cb34a6-382d-11e8-97fd-d185c69c2d99.png)

From these plots, you can look at the spread of your data across the Independent Variable groupings selected in the side-panel. Below the histograms, you will find a message that summarizes the groups/subgroups that seem to not have a normal distribution, where the p-value of the Shapiro-Wilk test is larger than the p-value threshold selected in the side-panel. Normal distribution is a requirement for performing an ANOVA test (less so for large sample sizes).

![04_explore_08](https://user-images.githubusercontent.com/14832460/38313752-7416180e-382d-11e8-9c00-a2251687c5a6.png)

If you want to see the detailed results of the Shapiro-Wilk test for all groups/subgroups along with their QQ-plots, tick the checkbox "See detailed Shapiro-Wilk test and QQ-plots". The table shows p-value of the Shapiro-Wilk tests performed for each groups/subgroups. If the p-value of the Shapiro-Wilk test for a group is larger than the selected p-value threshold, in the final column the group will be noted with "Data has NORMAL distribution" appears.

![04_explore_09](https://user-images.githubusercontent.com/14832460/38313755-743b55ba-382d-11e8-9d39-bc4bef2cc315.png)

Sample size affects the Shapiro-Wilk test and hence (the more the merrier), the user is strongly encouraged to check the QQ-plots. If you ticked the "See detailed Shapiro-Wilk test and QQ-plots?" checkbox, sliders for QQ-plots also appear. These sliders help choose the optimum number of columns and plots for display. The first slider " Display QQ plots in ... columns:" allows the user to choose the number of columns for the display of the QQ-plots. If the number of plots is too large to be displayed all at once in the window, a second slider "Plot portion of the data starting from element number..." appears and the user can choose the portion of plots to be displayed. 

Based on the results obtained in this sub-tab, you can have a better judgement in the following sub-tab whether to check Bartlett or Levene test for equal variances.

#### Examine variance

In the "Testing equal variance" sub-tab, you can have a look at the results of the Bartlett test and Levene test of homogeneity of variances between the different groups and for each sub-groups. Equal variances, or homoscedasticity, is also a requirement for performing an ANOVA test.

In the main window you see the boxplots for each group: left -  the observed data (y), middle - the data with the subtracted median (y-med(y)), right - the absolute deviations from the median (abs(y-med(y))).

![04_explore_11](https://user-images.githubusercontent.com/14832460/38313757-74758316-382d-11e8-85b2-60ceb99905db.png)

If you scroll lower, you will see the results of both Bartlett and Levene tests. The null hypothesis of the Bartlett and Levene tests assumes that the variances in each of the groups are the same. The Bartlett test is more robust when the data comes from a normal distribution, while Levene test is more robust in case of departures from normality.

The first table displays the results of the Bartlett test and the second table displays those of the Levene test. The tables show the p-value of tests performed for each groups/subgroups. If the p-value of the test for a group is larger than the selected p-value threshold, groups are noted as "Equal". In this case there is not enough evidence to reject the null hypothesis, where the variances are considered equal. If the p-value of the test for a group is smaller than the selected p-value threshold, groups are noted as "Not equal". In this case the null hypothesis is rejected and the variances are considered not equal.

![04_explore_12](https://user-images.githubusercontent.com/14832460/38313758-749abcc6-382d-11e8-9260-6599b19de290.png)

As indicated previously, the results of this sub-tab and the previous sub-tab are needed to for the ANOVA test performed in the following sub-tab. ANOVA assumes the data comes from a normal distribution and the variances are equal.

#### One / two sample test

In this subtab you can explore the differences between a certain value and your sample, or between two selected samples, with one/two sample t-test or Kolmogorov-Smirnov test (for non-parametric samples). 

First select which test you would like to perform and how you wish to group your samples in the left panel of the main window:

![04_explore_14](https://user-images.githubusercontent.com/14832460/38313759-74c039c4-382d-11e8-88bc-c1280fbb21b5.png)

In the case of one-sample t-test you should enter "mu value" - to test for significant difference between the value and the mean value of your selected sample group. The results of the one t-test will be displayed above the boxplot:

![04_explore_15](https://user-images.githubusercontent.com/14832460/38313760-74e2702a-382d-11e8-8782-00677e7419f9.png)

For the two-sample t-test or Kolmogorov-Smirnov test, you should select two specific samples. The results of the test will be shown above the graph:

![04_explore_16](https://user-images.githubusercontent.com/14832460/38313761-750774ec-382d-11e8-8177-f9f4cba93d4c.png)

#### Test significant differences between groups

In this sub-tab, you can check for signifcant differences in the means between different groups using analysis of variance (ANOVA) or a non-parametric test (Kruskal-Wallis). A text box displays the p-value of the ANOVA test between different groups of Independent Variables:

![04_explore_17](https://user-images.githubusercontent.com/14832460/38313762-752b6ece-382d-11e8-8b2b-a87275348507.png)

If the p-value of the ANOVA test for a group is larger than the selected p-value threshold, groups are noted with "NO significant difference in means". In this case there is not enough evidence to reject the null hypothesis and the means of the groups are assumed equal. If the p-value of the ANOVA test for a group is smaller than the selected p-value threshold, groups are noted with "SIGNIFICANT difference in means". In this case the null hypothesis, where the means of the group are considered equal, is rejected and the means of the groups can be considered significantly different. 

A second text bos displays the significant groups based on Tukey's pairwise comparison. Groups that share a common letter do not have significantly different means for the selected Dependent Variable. 

![04_explore_18](https://user-images.githubusercontent.com/14832460/38313763-754f0aaa-382d-11e8-8dad-7178ff45a294.png)

Boxplots display the distribution of the data for a specific trait (dependent variable) for the levels of the independent variable. The boxplots can be split by the second Independent Variable, selected when if you ticked the checkbox for "Split graph?". You can also change the main Independent Variable to compare differences between Genotypes across the individual subsets:

![04_explore_19](https://user-images.githubusercontent.com/14832460/38313764-7572a4c4-382d-11e8-9816-bbc60a8fdf3e.png)

You can also run a non-parametric test, by selecting it from the drop-down menu:

![04_explore_20](https://user-images.githubusercontent.com/14832460/38313765-759745e0-382d-11e8-9421-a0bf12a3c55e.png)

In case of non-parametric test, Wilcoxon / Mann-Whitney test will be used to make the pairwise comparison between individual groups. The results of the test are displayed in the lower text box:

![04_explore_22](https://user-images.githubusercontent.com/14832460/38313768-760ffa94-382d-11e8-8ca5-ea30de0ce04a.png)

#### Two-way ANOVA

In this sub-tab you can explore the effect of two Independent Variables and interaction between them simultaneously. Select Independent Variable 1 and 2 from the drop-down menu's in the main window:

![04_explore_23](https://user-images.githubusercontent.com/14832460/38313769-7638eb16-382d-11e8-9514-6781937ac340.png)

You can additionally subset your data for yet another Independent Variable, by selecting the "Subset the data?" box, and picking the Independent Variable and specific subset to be displayed / analysed:

![04_explore_24](https://user-images.githubusercontent.com/14832460/38313770-765d0a96-382d-11e8-8858-c2b960c7d5f6.png)

The results of two-way ANOVA are displayed in the text box below the interaction plot. The Independent Variable 1 (IV1) is the variable selected in the most left drop-down menu, while Independent Variable 2 (IV2) is the variable selected in the center drop-down menu:

![04_explore_25](https://user-images.githubusercontent.com/14832460/38313771-768a3c46-382d-11e8-94f6-daa863593fe4.png)

If you scroll down, you can see the residual plot of the two-way ANOVA shown above. The residuals should not show any pattern, indicating the linear relationship between independent variables. If this is not the case, the results of two-way ANOVA should not be trusted:

![04_explore_27](https://user-images.githubusercontent.com/14832460/38313775-76d5e650-382d-11e8-8a95-82e795896863.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

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

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

### 6. REDUCTION IN DIMENSIONALITY

#### PRINCIPAL COMPONENT ANALYSIS
Principal component analysis [(PCA)](https://en.wikipedia.org/wiki/Principal_component_analysis) is often used to simplify the data into fewer dimensions, and also to check which traits explain majority of the variation in the population studied. However, PCA is often not explored to its full potential. You can for example run PCA on data subsetted by an Independent Variable (e.g.treatment or genotype) and run PCA separately on those subsets to see how much each of your Dependent Variables contributes to explaining observed variation. MVApp will allows you to do all this!

#### Select data, subsets, and Dependent Variables
Select the dataset to analyse from the dropdown menu at the top of the side panel and click "Set the dataset":

![mvapp_pca_data_input1](https://user-images.githubusercontent.com/14832460/32647215-7829f348-c5f0-11e7-8416-c339543a8e9f.png)

Then, select which Dependent Variables you want to use in the PCA and click "Unleash the PCA monster":

![mvapp_pca_data_input2](https://user-images.githubusercontent.com/14832460/32647214-78109808-c5f0-11e7-8303-663bb39cb05e.png)

You can see your selected datasets in the first two sub-tabs.

#### Visualize the PCs
In the third sub-tab, [Eigenvalues](https://en.wikipedia.org/wiki/Eigenvalues_and_eigenvectors), the ["Scree plot"](http://www.improvedoutcomes.com/docs/WebSiteDocs/PCA/Creating_a_Scree_Plot.htm) are displayed showing the main principal components generated from the PCA in decreasing order of percentage variance explained by each Principal Component.

![mvapp_pca_eigen](https://user-images.githubusercontent.com/14832460/32647217-788202b8-c5f0-11e7-9f1d-85f44ccc6a10.png)

The table summarizing the eigenvalues of each principal component (PC), their percentage of variance EXPLAINED and the cumulative percentages that add to 100% can be found below the plot. The table can be downloaded as a .csv file.

#### Visualize the general contribution of DVs per PC
The 'Contribution per variable' sub-tab displays the contribution of the chosen Dependent Variables with respect to two PCs at a time. You can select individual PCs to be plotted on x- and y-axis from the two dropdown menus.

![mvapp_pca_trait_contrib](https://user-images.githubusercontent.com/14832460/32647213-77f03ef0-c5f0-11e7-8033-922fc073a36d.png)

#### Visualize the contribution of each DV value
By scrolling down, you can see the PC coordinates of the individual samples. The x- and y-axis are controlled by the same dropdown menu as the contribution plots. You can color the plot by any of the Independent Variable to see if you have separation in PC coordinates between your genotype / treatment / time points:

![mvapp_pca_scatter](https://user-images.githubusercontent.com/14832460/32647923-ef68a3c0-c5f3-11e7-9321-ede2f7620bef.png)

#### Visualize the contribution of each DV per PC
In the sub-tab 'Contribution per PC' the contribution of individual Dependent Variable for each PC are displayed. You can download the specific percentange contribution data per PC below the graph:

![mvapp_pca_trait_contrib2](https://user-images.githubusercontent.com/14832460/32647216-78663bdc-c5f0-11e7-82ef-dd418670a6f8.png)


#### MULTIDIMENSIONAL SCALING
Multidimensional scaling [(MDS)](https://en.wikipedia.org/wiki/Multidimensional_scaling) is a multivariate data analysis approach that is used to visualize the similarity/dissimilarity between samples by plotting points in two dimensional plots. The input data for MDS is a dissimilarity matrix representing the distances between pairs of objects. MDS is mathematically and conceptually similar to PCA and factor analysis. PCA is more focused on the dimensions themselves, and seek to maximize explained variance, whereas MDS is more focused on relations among the scaled objects.

#### Select data, subsets, and Dependent Variables
Select the dataset to analyse from the dropdown menu at the top of the side panel and click "Set the dataset":

![mds 1 dataset](https://user-images.githubusercontent.com/20439594/34207879-528ece2a-e59d-11e7-9e5e-5ac61043b839.png)

You can see your selected datasets in the first two sub-tabs.

Then, select which Dependent Variables you want to use in the MDS. You have the option of clicking the checkbox to scale the data. Then, click "Unleash the power of Multidimensional scaling":

![mds 2 dv](https://user-images.githubusercontent.com/20439594/34207880-52b47abc-e59d-11e7-8f84-ff4e970db7c3.png)

#### MDS of the samples
In the third sub-tab, a scatter plot showing the two dimensions resulting from the MDS is displayed. If the clustering option was checked, the clusters would be highlighted in different colors on the graph. This plot can be downloaded as a .png file.

![mds 3 run](https://user-images.githubusercontent.com/20439594/34207882-52d380ce-e59d-11e7-91b3-bd4a99c72564.png)

You can also click on the checkbox to perform K-means clustering of the MDS results. The default number of clusters is 3, but you can adjust it as required.

![mds 4 cluster](https://user-images.githubusercontent.com/20439594/34207883-52f35944-e59d-11e7-828e-5edc765811c0.png)

The table summarizing the dimensions of the MDS results are shown, with the K-clusters if the option was chosen. The table can be downloaded as a .csv file.

#### MDS on the individuals
In the sub-tab 'Scaling of the Dependent Variables' the dataset is transposed to perform the MDS with reference to each Dependent Variable. The plot showing the coordinates of each variable is displayed and color-coded by cluster number if that option was included.

![mds 5 scaling dv](https://user-images.githubusercontent.com/20439594/34207884-53192764-e59d-11e7-8c92-094b134ad743.png)

The table below the plot summarizes the coordinates of each Dependent Variable with regards to the two MDS dimensions, with the K-cluster number for each variable if the clustering option was chosen. The table can be downloaded as a .csv file.

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

### 7. CLUSTER ANALYSIS

#### Selecting the data

![mvapp_hcluster_hotmap](https://user-images.githubusercontent.com/14832460/32647242-8e58ae48-c5f0-11e7-85fe-7501207fbfcd.png)

#### What kind of clustering

#### Select the distance for cluster separation

![mvapp_hcluster_dendro](https://user-images.githubusercontent.com/14832460/32647240-8df05d2a-c5f0-11e7-99dc-9b75a5430c96.png)

![mvapp_hcluster_dendro2](https://user-images.githubusercontent.com/14832460/32647243-8e890e94-c5f0-11e7-8312-08bf29ef78b2.png)

###### Validate the cluster

![mvapp_hcluster_clustervalidation](https://user-images.githubusercontent.com/14832460/32647239-8db955f0-c5f0-11e7-9e90-67fe21ae6f65.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

### 8. HERITABILITY
Heritability is the proportion of the phenotypic variance that can be attributed to genetic variance. This statistic is important in the fields of genetics in order to assess if a trait is heritable (genetically controlled). MVapp allows you to calculate the broad-sense heritability, which is the ratio of total genetic variance to total phenotypic variance (https://www.ncbi.nlm.nih.gov/books/NBK21866/). 

Make sure you enter the "Year" and/or "Location" as independent variables when you upload the data. Enter the number of replicates per year/per location. If year and/or location is missing, you can choose the option "none" from the drop-down lists. You can also use to subset the data, by treatment for example, to calculate heritability within for each treatment. 
In the example below, we have a column for Year but none for location (experiment is replicated across years in the same location). There are two replicates per accession per year.
<img width="663" alt="heritability_1" src="https://user-images.githubusercontent.com/15339112/35803543-33443980-0a85-11e8-8973-67a87b97cf14.png">

The output as shown below gives the summary of the information entered (number of replications, number of years/locations and unique values per year/location) and the summary of the model used to calculate heritability.

<img width="667" alt="heritability2_a" src="https://user-images.githubusercontent.com/15339112/35804191-c54e73c0-0a87-11e8-8e55-6cf50569d389.png">

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

### 9. QUANTILE REGRESSION

[Quantile regression](https://en.wikipedia.org/wiki/Quantile_regression) is a way to estimate the conditional quantiles of a response variable distribution in the linear model that provides a complete view of possible causal relationships between variables. Quantile regression minimizes absolute errors and can provide a more comprehensive analysis of the effect of the predictors on the response variable than mean regression. Linear quantile regression is related to linear least-squares regression as both try to study the relationship between the predictor variables and the response variable, the only difference being that least-squares involves modeling the conditional mean of the response variable, whereas, quantile regression models the conditional quantile of response. It is especially useful in applications where extremes are important, such as environmental studies where upper quantiles of yield are critical.

#### When should you use it?

Quantile regression estimates are more robust against outliers in the response, so if your response variable has potential outliers or extreme data, then ordinary least squares (OLS) regression is more effected as mean is more effected by outliers, you can use median regression as a substitute. If your errors are non-normal then OLS is inefficient, but quantile regression is robust. If your data fails to satisfy the assumption of homoscedatcity of the error terms, then you can use this technique, as there is no such assumption required here. Beyond that,  quantile regression also provides a richer characterization of the data, allowing us to consider the impact of an explantory variable on the entire distribution of response, not merely its conditional mean.

#### Select the dataset

Select which dataset you would like to use to perform quantile regression. You can choose from the following options: raw data: use your selected data from “Upload your data” tab, you can check the data from the Data Magic - New data tab missing values removed: use data without rows with missing values outliers removed: use outlier-free data from “Data curation” tab. The dataset you chose to explore will be displayed in the 'Selected dataset' sub-tab.

![select dataset](https://user-images.githubusercontent.com/34129392/35613562-8e494a70-067d-11e8-8411-66407a9de61d.png)

#### Select reponse, explanatory variable, subsets

Select the phenotype you want as response of your quantile regression, you can only choose one variable. Select the independent varaibles to subset the data, you can choose a maximum of two variables. Then choose the explanatory variables of your quantile regression model, you can choose any number of explanatory variables. You can choose a p-value threshold to test the significance of the explantory variables. You have the option to scale the data which might be useful if your variables are in different units.

![select options](https://user-images.githubusercontent.com/34129392/35572617-0980bc3e-05e6-11e8-9dc7-f2477b3b8d3d.png)

#### Set the data

Once you are done choosing the data, you can click on "Click to set the data" and the final data will be locked for further analysis and a set of quantile regression model will be run, based on your chosen variables and subsets of the data.

![click to set](https://user-images.githubusercontent.com/34129392/35572616-091a773a-05e6-11e8-900f-9a1cccb206f4.png)

#### Final data for analysis

The final data used for analysis can be seen in the sub-tab 'Final data for analysis'.

![final data](https://user-images.githubusercontent.com/34129392/35572615-08f9c8e6-05e6-11e8-8134-638e82a5c0e4.png)

#### Results of quantile regression

The result of the quantile regression model can be seen in the sub-tab 'Modelled data'. The message box displays the significant phenotypes for lower, median and upper quantiles of the response for the particular subset chosen from the drop down list. You can choose the subset whose result you want to see in the message box.

![modelled data](https://user-images.githubusercontent.com/34129392/35572612-08cc6658-05e6-11e8-966c-c1160c2c9e32.png)

The results from all the quantile regression models for different subsets are tabulated. The table can be downloaded as a .csv will all the results. You can also search for a particular subset or a particular quantile level from the 'Search:' box if you want to look at a particular result.

![data table](https://user-images.githubusercontent.com/34129392/35572610-089b8074-05e6-11e8-8f81-b434390a042d.png)

#### Visualize the results

The plots of the regression models can be seen in the sub-tab 'Quantile plots'. You can choose the independent variable by which you want to group your plot. For example if you want to compare how a particular phenotype behaves in salt and control condition, then you can group your plot by Treatment. If you have chosen two independent variables to subset your data, then you can also choose the value of your another subset variable whose result you want to see. If you want to view a single plot, then choose the particular phenotype you want to view. If you want to view the results of all your phenotypes the you can choose 'multiple plots' from "View plots as:". Finallly click on "View plot(s)" button to display the plot(s).

![quantile plots](https://user-images.githubusercontent.com/34129392/35572609-0875f714-05e6-11e8-8092-caa76b3f762a.png)

#### Quantile plots

If you choose to view your plot as single plot, the quantile plot of the phenotype chosen will be displayed. The coefficients of the phenotype are plotted against the quantile level. The colored dots represent that the variable is significant for the particular quantile level and the cross sign represent that it is not significant. The different colors represent the different unique realizations of the grouping variable. The different lines can be used to compare the behavior of phenotypes in different conditions or different days, depending on the grouping variable. The plot can be downloade by using the "Download plot" button at the bottom.

![single plot](https://user-images.githubusercontent.com/34129392/35572608-0852d162-05e6-11e8-82b9-98582869480c.png)

If you choose to view your plots as multiple plots, the quantile plots of all the phenotypes will be displayed. You can download these plots using the "Download plot" button at the bottom.

![multiple plots](https://user-images.githubusercontent.com/34129392/35572607-0832032e-05e6-11e8-884f-b708e1aad2b7.png)

If you have more than four explanatory variables, then you can use the slider to view more plots.

![multiple plots slider](https://user-images.githubusercontent.com/34129392/35572605-080e38b8-05e6-11e8-9160-c9302cd56ac6.png)





[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

