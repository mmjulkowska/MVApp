# MVApp
Glittery multivariate analysis platform for all kinds of data. Follow us on twitter [@MVApp007](https://twitter.com/MVApp007). 

The app is available [here](http://mvapp.kaust.edu.sa/MVApp/) or you can run it locally from your device by typing the following command in your R window:

`install.packages("shiny")`
`library("shiny")`
`shiny::runGitHub("mmjulkowska/MVApp", "mmjulkowska")`

(....it will take some time for the first time to upload all the libraries)

## Purpose statement - What is MVApp for?

MVApp was created to streamline data analysis for all kinds of biological queries - from investigating mutant phenotypes, examing the effects of an experimental treatment, to studying natural variation using any biological system.  

We believe that MVApp will enhance data transparency and standardize data curation and analysis in the scientific community by empowering researchers to perform complex analyses without extensive knowledge of R or statistics, as well as improve the data analysis literacy in wider scientific community. 

Although the MVApp development team is buried armpit-deep in Plant Science, we are trying to make the App as applicable as possible for all biological disciplines and beyond. If you have any suggestions on other types of analyses we can include, please check out our guidelines on how to [contribute](https://github.com/mmjulkowska/MVApp/blob/master/CONTRIBUTING.md). 

Currently MVApp has following features:
1. Identification of outliers using different methods based on one or multiple phenotypes 
2. Summary of the data dynamics by fitting simple functions or polynomial curves to data points
3. Hypothesis testing using parametric and non-parametric tests, including testing the assumptions of normality and equal variance 
4. Correlation analysis of all measured traits in the experiment or within a specific subset of data
5. Reduction of data dimensionality and identifying the traits that explain the most data variance using principal component analysis and multidimensional scaling 
6. Clustering individual samples using hierarchical or k-means clustering 
7. Estimation of broad-sense heritability of measured traits 
8. Quantile regression analysis that allows the identification of traits with significant contribution to traits of major interest

You can read the instructions below, or watch one of our video-tutorials on [youtube](https://www.youtube.com/channel/UCeTCqj3dHWbjIbt9cXVjHMQ).

### How to cite the MVApp:

The app is not published yet, but you can find the pre-print version of the MVApp manuscript on figshare:
[Julkowska, Magdalena; Saade, Stephanie; Agarwal, Gaurav; Gao, Ge; Pailles, Yveline; Morton, Mitchell; Awlia, Mariam; Tester, Mark (2018): MVAPP – Multivariate analysis application for streamlined data analysis and curation. figshare. Paper.](https://figshare.com/articles/MVAPP_Multivariate_analysis_application_for_streamlined_data_analysis_and_curation/6291461)

If you wish to cite the app itself, please use the following:
Julkowska, M.M., Saade, S., Gao, G., Morton, M.J.L., Awlia, M., Tester, M.A., "MVApp.pre-release_v2.0 mmjulkowska/MVApp: MVApp.pre-release_v2.0", DOI: 10.5281/zenodo.1067974

## Table of contents:


[1. DATA UPLOAD](#1-data-upload)


[2. SPATIAL VARIATION](#2-spatial-variation)


[3. CURVE FITTING](#3-curve-fitting)


[4. OUTLIER SELECTION](#4-outlier-selection)


[5. DATA EXPLORATION](#5-data-exploration)


[6. CORRELATIONS](#6-correlations)


[7. PRINCIPAL COMPONENT ANALYSIS](#7-principal-component-analysis)


[8. MULTIDIMENSIONAL SCALING](#8-multidimensional-scaling)


[9. HIERARCHICAL CLUSTER ANALYSIS](#9-hierarchical-cluster-analysis)


[10. K-MEANS CLUSTER ANALYSIS](#10-k-means-cluster-analysis)


[11. HERITABILITY](#11-heritability)


[12. QUANTILE REGRESSION](#12-quantile-regression)


### 1. DATA UPLOAD

#### Data format:

MVApp can handle .csv files containing at least the following columns: 
* column with the genotype or the main Independent Variable (if you use only one genotype - we advise you to include one column with the genotype anyway and give the same name to all of your samples)
* One or multiple column(s) with an Independent Variable (e.g. treatment, position, experimental batch number)
* One or multiple column(s) containing Dependent Variable - numerical data of the measured traits - also known as phenotypes

If you have a timeseries experiment, or any other gradient, and you want to fit curves to your data, the input data should include columns containing:
* Time (or other continuous Independent Variable) - this variable MUST be numeric (e.g. "1" instead of "Day 1")
* Sample ID - an identifier for each individual sample

Your data should look similar to the Example dataset, with ID and TIME column being optional:

![mvapp_data](https://user-images.githubusercontent.com/14832460/32609292-48dc541c-c570-11e7-8f87-54ca02b646d8.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

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

### 2. SPATIAL VARIATION

### Why test spatial variation?

Any environment where plants are grown is susceptible to spatial variation resulting in e.g. light, temperature and humidity gradients. These can affect the growth of the plants, and thus have an effect on the obtained results. While we encourage the experimental designs which will minimize the effect of spatial variation, we also allow to test the spatial variation effects on the individual data. 

At the moment MVApp does not include specific models to correct for spatial variation, as these require advanced statistical insight and commercial R-packages which we cannot provide in a reliable way. Nevertheless, we are happy to recieve [contributions](https://github.com/mmjulkowska/MVApp/blob/master/CONTRIBUTING.md) from the community. 

### Upload the spatial information into the MVApp

In order to test the spatial effects, you need to include the collumns containing spatial information in the uploaded dataset. You can do it by selecting the box "Data contains information on spatial distribution of collected data?" and add more than one collumn containing spatial information:

![spatial_dataupload](https://user-images.githubusercontent.com/14832460/50540946-932a3800-0b69-11e9-8b90-ff013b727dd2.png)

### Examine the effect of spatial variation on individual phenotypes

By navigating into the Spatial Variation tab, you can select the phenotype you want to examine and the spatial components of which the effect will be tested. Once you have selected everything, click on the button "Unleash spatial viz" to run the analysis. In the main panel, you will find the graph indicating the trait value across the spatial gradient for visual examination:

![spatial_dataselect](https://user-images.githubusercontent.com/14832460/50540945-932a3800-0b69-11e9-9f0f-cb6282e0f1e4.png)

When you scroll down, you will find the ANOVA results of the effect of the selected spatial components:

![spatial_datatest](https://user-images.githubusercontent.com/14832460/50540944-9291a180-0b69-11e9-8666-d435e6b547b6.png)

You can also perform the spatial variation analysis on multiple spatial components, by selecting more than one component into the "Spatial Variable" widnow. Please press "Unleash spatial viz" button to update the analysis:

![spatial_multi_iv](https://user-images.githubusercontent.com/14832460/50540943-9291a180-0b69-11e9-9f15-4a552ae8cafe.png)

If you collected data across multiple time points / treatments, and would like to perform spatial analysis separately for each time point, you can select "Subset the data?" checkbox and select the specific subset you would like to explore. Please press "Unleash spatial viz" button to update the analysis:

![spatial_subset](https://user-images.githubusercontent.com/14832460/50540942-9291a180-0b69-11e9-8046-b8a8c4baa017.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

### 3. CURVE FITTING

####  Why model your data?

If you have a continuous Independent Variable in your experiment, you might want to estimate how your Dependent Variables change across it. For example, you could investigate the dynamics of plant/bacterial growth over time, or the dose dependency of a phenotypic response to a chemical treatment. [Fitting curves](https://en.wikipedia.org/wiki/Curve_fitting) will allow you to observe and model these response dynamics.

#### Fit simple functions

At the moment, MVApp helps you to fit simple functions: linear, quadratic, exponential and square root functions. For these functions, we fit a linear model (using lm() function) between the continuous Independent Variable indicated in the "Time" column and the Dependent Variable (phenotype). 

Modelling of non-linear functionsis also relies on fitting a linear function, by transforming the Dependent Variable, so the linear model can be fitted:
- square root transformation for fitting quadratic function
- log transformation for fitting exponential function
- quadratic transformation for fitting the square root function

MVApp extracts the model parameters: y-intercept ("INTERCEPT") and the first regression coefficient ("DELTA"), as well as the r2 values to determine model performance.

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Fit curves with MVApp

First, in the side panel, select which the Independent Variable(s) you wish to group your samples by, and which Dependent Variable you would like to model. 

![02_curve_fit_01](https://user-images.githubusercontent.com/14832460/38308597-4c0e8f10-3820-11e8-90f2-6de049425f80.png)

![02_curve_fit_02](https://user-images.githubusercontent.com/14832460/38308598-4c2f6186-3820-11e8-9875-6485bdcebbe8.png)

If you do not know which function will fit best, you can click on "Unleash model estimation" button. The best model will be indicated based on the r2 values presented in the table:

![02_curve_fit_03](https://user-images.githubusercontent.com/14832460/38308599-4c518a86-3820-11e8-91e4-70f2e6370f9d.png)

Decide on which model you would like to apply for entire dataset and click "Unleash the model":

![02_curve_fit_04](https://user-images.githubusercontent.com/14832460/38308600-4c72bfbc-3820-11e8-88aa-a2f192ecdd2d.png)

Whenever you fit the curves to the chosen Dependent Variable (phenotype), MVApp will automatically calculate the coefficient of correlation (r2) that indicates how well the fitted function models the observed data. The number of samples that have a poor fit (r2 < 0.7) will be indicated in the message box above the table:

![02_curve_fit_05](https://user-images.githubusercontent.com/14832460/38308602-4c9905fa-3820-11e8-9037-42538b3f446a.png)

You can change the threshold of the r2 by changing the r2 cut-off value:

![02_curve_fit_06](https://user-images.githubusercontent.com/14832460/38308603-4cbbfc5e-3820-11e8-99a2-bad2d4b10339.png)

You can view the lowest r2 values by sorting the samples based on r2 by clicking on the r2 column sorting arrow:

![02_curve_fit_07](https://user-images.githubusercontent.com/14832460/38308604-4cdb8f6a-3820-11e8-9a6a-f5249ad58ca4.png)

You can download your data without the samples showing poor r2 fit by scrolling down and clicking on "Download curated data with r2 > cut-off" button:

![02_curve_fit_08](https://user-images.githubusercontent.com/14832460/38308605-4d0221b6-3820-11e8-88b5-1d7870c359d6.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Visualise goodness of fit of the dynamic curves with fit-plots

You can examine how good your data fits to the selected model by viewing fit-plots - the names of the samples are merged by "Genotype_IndependentVariable_SampleID". You can either scroll through the sample list or type in the sample name:

![02_curve_fit_09](https://user-images.githubusercontent.com/14832460/38308606-4d28e918-3820-11e8-89b2-fe4b26a8a8f9.png)

If you are having trouble interpreting the graphs, or need a quick figure legend describing the graph, you can select "show figure legend" for the default version of figure legend to pop-up:

![02_curve_fit_10](https://user-images.githubusercontent.com/14832460/38308607-4d4a1cdc-3820-11e8-80cd-fc991e1b7ae7.png)

You can view multiple fit-plots simultaneously. The plots can be sorted by either increasing or decreasing r2 values:

![02_curve_fit_11](https://user-images.githubusercontent.com/14832460/38308609-4d80554a-3820-11e8-94bc-809ef75d75ed.png)

You can scroll through the individual graphs with the slider on the right side of the main window:

![02_curve_fit_12](https://user-images.githubusercontent.com/14832460/38308610-4da68ec2-3820-11e8-9350-6f8f8c362304.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Assess and compare the dynamics between Genotypes and / or Independent Variables

Finally, you can compare how the calculated DELTAs or Coefficients extracted from the models differ between your genotypes and other Independent Variables, such as "treatment". 

You can examine the differences by clicking on sub-tab "Examine differences". The message box at the top provides ANOVA results, with the p-value threshold indicated below the graph:

![02_curve_fit_13](https://user-images.githubusercontent.com/14832460/38308611-4dcabe1e-3820-11e8-9169-5a1741548029.png)

You can use raw data, or data with r2 above the threshold value:

![02_curve_fit_14](https://user-images.githubusercontent.com/14832460/38309174-fdb03c68-3821-11e8-83ce-920b2b75ad3e.png)

By scrolling further down, you will find a panel to control the design of the graph, as well as threshold p-value for the ANOVA:

![02_curve_fit_15](https://user-images.githubusercontent.com/14832460/38309240-2cfde01a-3822-11e8-8e77-27cc3e8cd755.png)

You can change the graph to, for example, bar graph, remove background or determine what is represented by the error bars. The default figure legend will update automatically too:

![02_curve_fit_18](https://user-images.githubusercontent.com/14832460/38309331-78926dfc-3822-11e8-803f-15762ed632b4.png)

By scrolling further down, you can find the significant groups, as calculated per Tukey's pairwise test, with the same p-value threshold as ANOVA:

![02_curve_fit_20](https://user-images.githubusercontent.com/14832460/38309459-cb90b0e0-3822-11e8-9dd6-bb4c830b999c.png)

By scrolling down even further, you will find a table containing the summary statistics for your data for all fitted values:

![02_curve_fit_21](https://user-images.githubusercontent.com/14832460/38309493-db7246ae-3822-11e8-969e-5ab3def193c6.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Fit polynomial curves with MVApp

If your data shows signs of complex dynamics across your continuous Independent Variable (often particularly applicable for long time-series), you might consider fitting a polynomial curve. The splines usually have very high r2 values, and are therefore not included in the "model estimation" for the best fitting curves.

For the [cubic splines](http://mathworld.wolfram.com/CubicSpline.html), we use lm(phenotype ~ bs(time, knots=X)) function in R, where you can indicate the position of a knot in the "timepoint to split the cubic spline" box. 

![002_curves_cubicspline_01](https://user-images.githubusercontent.com/14832460/38469032-2d3c84a4-3b57-11e8-9114-9b47bf702947.png)

The fit-plots for the cubic splines carry a dashed diagonal line at the knot position. 

![002_curves_cubicspline_02](https://user-images.githubusercontent.com/14832460/38469033-2d6424b4-3b57-11e8-97b3-9a4a3f415b15.png)

For the smoothed splines, we use smooth.spline() function in R, and you can select between between automatic or user-defined degrees of freedom. The user-defined degrees of freedom can be selected with the "Number of degrees of freedom" slider.

![002_curves_smoothedspline_01](https://user-images.githubusercontent.com/14832460/38469035-2db1b49a-3b57-11e8-8ccc-8a4fc9a5a6cb.png)

The fitplots for the smoothed splines are represented with the purple lines.

![002_curves_smoothedspline_02](https://user-images.githubusercontent.com/14832460/38469036-2dd8c8aa-3b57-11e8-845d-8af642454c60.png)

In case you choose to fit smoothed splines with automatically determined degrees of freedom, they will be displayed in the last column of the table in the sub-tab "Modelled data". Please be aware that the degree of freedom might differ between individual samples.

![002_curves_smoothedspline_03](https://user-images.githubusercontent.com/14832460/38469037-2e007738-3b57-11e8-91d3-fd645abc5681.png)

Although the polynomial functions commonly have better fit than the simple functions, like linear, quadratic or exponential ones, they often result in more coefficients describing the dynamics. So if you would like to use curve-fitting for simplifying your data, using polynomial functions might not be the best choice. 

![002_curves_cubicspline_03](https://user-images.githubusercontent.com/14832460/38469034-2d8c430e-3b57-11e8-8305-55a50c955bc6.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

### 4. OUTLIER SELECTION

#### Why identify potential outliers?

For those familiar with large(ish) scale experiments, you probably had to curate your data, removing [outlier](https://en.wikipedia.org/wiki/Outlier#Working_with_outliers) samples that stem from experimental errors or even human mistakes while recording data to avoid making spurious conclusions based on unrepresentative data. 

You likely identified these "weird" samples by simple graphical means, or based on their distance from the median in terms of the Standard Deviation or the Interquartile Range.

MVApp helps to automatically highlight potential outliers based on a single or multiple Dependent Variables, using various approaches. However, be careful, outliers should not be automatically removed. It is good practice to justify outlier samples, perhaps refering to notes or images taken during the experiment that might explain the unusual result. It is possible that a "potential outlier" is in fact a valuable, if extreme, result. 

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Highlight potential outliers

Begin by choosing which dataset you would like to use. Whether or not to remove samples with missing values or (in case you have performed curve fitting) dataset with curated r2 values:

![03_outliers_01](https://user-images.githubusercontent.com/14832460/38309888-d661cff8-3823-11e8-8c59-c3dd645f64a3.png)

![03_outliers_02](https://user-images.githubusercontent.com/14832460/38309889-d6870818-3823-11e8-8814-b65f44427386.png)

Select the Independent Variable(s) by which to group the samples, and whether you would like to select outliers based on one, some or all phenotypes / measured traits:

![03_outliers_03](https://user-images.githubusercontent.com/14832460/38310382-0f991b2c-3825-11e8-9d1d-2eb76e13a374.png)

Next, select which method you would like to use to highlight potential outliers. MVApp provides the following methods:
- 1.5x [Interquartile Range](http://www.purplemath.com/modules/boxwhisk3.htm): this is the most commonly used method and is visually very appealing, as potential outliers will be identified on the boxplot as "dots" outside of the whiskers.
- [Cook's distance](https://en.wikipedia.org/wiki/Cook%27s_distance): this algorithm is actually used to determine the most influential points - meaning the points that could make the most difference in the correlation tests and such. However, if the sample is considered to be "influential" in the majority of traits, it might be worthwhile to check whether everything is OK with it.
- [Bonferoni test](http://polisci.msu.edu/jacoby/icpsr/regress3/lectures/week3/11.Outliers.pdf): this is a test using the outlierTest() function from car package in R
- 1x Standard Deviation from the Median - all the samples that are further than 1xSD from the median are highlighted as potential outliers (this test is VERY strict and we do not recommend it)
- 2x Standard Deviation from the Median - all the samples that are further than 2xSD from the median are highlighted as potential outliers
- 2.5x Standard Deviation from the Median - all the samples that are further than 2.5xSD from the median are highlighted as potential outliers
- 3x Standard Deviation from the Median - all the samples that are further than 3xSD from the median are highlighted as potential outliers

![03_outliers_04](https://user-images.githubusercontent.com/14832460/38309891-d6d85d3a-3823-11e8-8422-a38d88a8fdc9.png)

You can now click on the "unleash the outlier highlighter" to view the curated data. In the main tab, the outlier message will appear indicating the number of potential outliers highlighted, as well as a table of your data. If you scroll to the right, you will see the columns marked "outl_Dependent Variable" (for example "outl_AREA"), where "true" will indicate this sample as being an outlier per genotype / day / independent variables selected. If considering all Dependent Variables, the final column will indicate whether a given sample is a potential outlier in the number of Dependent Variables that meets or exceeds the user-defined threshold (annotated as "true").
The number of identified outliers will be shown in the text box above the table:

![003_outliers_spare_02](https://user-images.githubusercontent.com/14832460/38468904-57e6c04a-3b55-11e8-9d2e-adca0d638b0f.png)

If you decided to select outliers based on all Dependent Variables (remove the entire column), and not replace them by NA (empty cells), use the slider input in the side panel to select the number of Dependent Variables a given sample must be an outlier in order to be considered an outlier across the whole experiment, i.e. the samples that are extreme across many phenotypes and thus should be removed from the data analysis. 

![003_outliers_spare_03](https://user-images.githubusercontent.com/14832460/38468905-580ada8e-3b55-11e8-8c56-5efaf05b8584.png)

As you slide the slider, the number of the outliers in the message box will change.

![003_outliers_spare_04](https://user-images.githubusercontent.com/14832460/38468906-582e9ff0-3b55-11e8-8302-b57f2c6ad2da.png)


[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Examine the data with and without potential outliers
After highlighting potential outliers in your data, you can look at how the data looks with and without them.

Go to sidepanel sub-tab "Tweak the graphs" and select the Dependent Variable you wish to examine. You can also select the type of plot you would like: box plot, scatter plot or bar plot (we find box plots to be the most informative).

Click on the main panel sub-tab "Graph containing outliers" to see your plots prior to removing potential outliers.

![03_outliers_08](https://user-images.githubusercontent.com/14832460/38309897-d79b8594-3823-11e8-9d6b-dbbfcc86d80a.png)

You can reduce the number of genotypes you are examining simultaneously by changing the slider "Show ... number of samples":

![03_outliers_09](https://user-images.githubusercontent.com/14832460/38309898-d7de9ab4-3823-11e8-8dc5-fadf8e51e148.png)

You can change the portion of the data that you examine by changing the slider "Plot portion of the data starting from the element number ...":

![03_outliers_10](https://user-images.githubusercontent.com/14832460/38309899-d80d3aae-3823-11e8-8f90-a5ac6e4cb43d.png)

If you wish to color code your samples or split the plots based on your Independent Variables, you can do this in the side panel:

![03_outliers_11](https://user-images.githubusercontent.com/14832460/38309900-d83248f8-3823-11e8-9234-57fe8fec05ca.png)

If you want to alter the order of your samples, you can swap the order of the Independent Variables in the sidebar sub-panel "Outlier selection":

![mvapp_outlier_graph_even_nicer](https://user-images.githubusercontent.com/14832460/32647167-5066effa-c5f0-11e7-9388-9620294702ff.png)

You can also change the order of the samples, by adjusting the order of the Independent Variables in the side-panel 

![003_outliers_spare_08](https://user-images.githubusercontent.com/14832460/38468910-58d9bebc-3b55-11e8-810d-c39f10897176.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)


#### Compare the data with outliers removed
If you want to look at the graphs with potential outliers removed (as highlighted in main panel "The outliers test"), click on the main panel "Graphs with outliers removed". You can click between "Graph containing outliers" and "Graph with outliers removed" to compare both datasets.

![003_outliers_spare_05](https://user-images.githubusercontent.com/14832460/38468907-58539fda-3b55-11e8-8cf4-2e2f9ee050b9.png)

![003_outliers_spare_06](https://user-images.githubusercontent.com/14832460/38468908-5878014a-3b55-11e8-9e26-6246ab360641.png)

IMPORTANT NOTE: This outlier test was developed to facilitate data curation. Please do NOT remove any data before making absolutely sure that there is a very good reason that the sample is not representative. We recommend downloading the dataset with outliers highlighted, manually removing samples that you can reasonably explain, and reuploading the curated dataset before continuing with your analysis.

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Calculate summary statistics

MVApp can calculate [summary statistics](https://en.wikipedia.org/wiki/Summary_statistics) functions (e.g. mean, median, standard deviation) in the main panel sub-tab "Summary data".

Select the dataset that you would like to use in the upper left corner of the main tab, and select the functions that you want to be calculated in the upper right corner.Then click "Unleash summary statistics":

![03_outliers_17](https://user-images.githubusercontent.com/14832460/38311203-472c28a2-3827-11e8-846a-f89b377fb732.png)

Table containing all the calculations will appear in the main panel. You can download the data into your computer by clicking "Download summary statistics data" button:

![03_outliers_19](https://user-images.githubusercontent.com/14832460/38311204-474fdb62-3827-11e8-9852-b12780b37807.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

### 5. DATA EXPLORATION

Once your data is nice and clean and ready to go, it is time to start having a proper look at it. A good place to start is to check out how your data is distributed using histograms and boxplots, grouping samples according to your various Independent Variables. From these you can get an idea of how your different genotypes are behaving, how your treatments are affecting your phenotypes, and how variable your data is. 

Beyond eyeballing, you can apply statistical tests such as ANOVA to test whether there are significant differences between groups. These are all easy things to do in MVApp, which also helps you check the assumptions of these statistical tests, such as normal distribution and homoscedasticity (i.e. equal variance).

In the side panel, you can choose:
- The dataset to be used (raw data, data with missing values removed, or data with outliers removed). The default value is raw data. If you did not perform outlier removal or curve fitting, the "outliers removed" and "r2 fitted curves curated data" will not work properly, so please do not select them.
- The Independent Variable to subset the data
- The Dependent Variable you want to plot
- The p-value threshold to be used in subsequent tests in the tab (e.g.: Levene's test, ANOVA, etc.). The default value is 0.05
- If you want to facet your graphs by another Independent Variable, tick the checkbox "Split the graph?" a dropdown menu with a list of independent variables appears. The user can choose the independent variable to split the plot.


![04_explore_01](https://user-images.githubusercontent.com/14832460/38313740-731447a0-382d-11e8-974c-e26a14023931.png)

Once the choices are made, the user can proceed to the different tests available in the DATA EXPLORATION tab.

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Examine distribution

Your histograms will appear in the "Testing normal distribution" sub-tab, where you can choose between "Histograms with  counts on y-axis" and "Histograms with density on y-axis".

![04_explore_03](https://user-images.githubusercontent.com/14832460/38313742-735eaf48-382d-11e8-81cf-c46d74818313.png)

![04_explore_04](https://user-images.githubusercontent.com/14832460/38313743-7384a234-382d-11e8-96a7-2b5427adb3ca.png)

You can split the graphs by Independent Variable, by ticking the box "Split the graph?" and chosing the Independent Variable that you would like to use for splitting:

![04_explore_05](https://user-images.githubusercontent.com/14832460/38313744-73a72ad4-382d-11e8-81ec-6dc65e9fa85a.png)

You can subset your data even further, by ticking the box "Subset the data?" in the main window, and selecting yet another one of the Independent Variables, and selecting specific value for this variable that will be used for the displayed graphs:

![04_explore_06](https://user-images.githubusercontent.com/14832460/38313746-73cb34a6-382d-11e8-97fd-d185c69c2d99.png)

From these plots, you can look at the spread of your data across the Independent Variable groupings selected in the side-panel. Below the histograms, you will find a message that summarizes the groups/subgroups that seem not to have a normal distribution, where the p-value of the Shapiro-Wilk test is larger than the p-value threshold selected in the side-panel. Normal distribution is a requirement for performing an ANOVA test (less so for large sample sizes).

![04_explore_08](https://user-images.githubusercontent.com/14832460/38313752-7416180e-382d-11e8-9c00-a2251687c5a6.png)

If you want to see the detailed results of the Shapiro-Wilk test for all groups/subgroups along with their QQ-plots, tick the checkbox "See detailed Shapiro-Wilk test and QQ-plots". The table shows p-value of the Shapiro-Wilk tests performed for each groups/subgroups. If the p-value of the Shapiro-Wilk test for a group is larger than the selected p-value threshold, in the final column the group will be noted with "Data has NORMAL distribution".

![04_explore_09](https://user-images.githubusercontent.com/14832460/38313755-743b55ba-382d-11e8-9d39-bc4bef2cc315.png)

Sample size affects the Shapiro-Wilk test and hence (the more the merrier), the user is strongly encouraged to check the QQ-plots. If you ticked the "See detailed Shapiro-Wilk test and QQ-plots?" checkbox, sliders for QQ-plots also appear. These sliders help choose the optimum number of columns and plots for display. The first slider " Display QQ plots in ... columns:" allows the user to choose the number of columns for the display of the QQ-plots. If the number of plots is too large to be displayed all at once in the window, a second slider "Plot portion of the data starting from element number..." appears and the user can choose the portion of plots to be displayed. 

Based on the results obtained in this sub-tab, you can have a better judgement in the following sub-tab whether to check Bartlett or Levene test for equal variances.

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Examine variance

In the "Testing equal variance" sub-tab, you can have a look at the results of the Bartlett test and Levene test of equal variances between the different groups and for each sub-groups. Equal variances, or homoscedasticity, is also a requirement for performing an ANOVA test.

In the main window, you see the boxplots for each group: left -  the observed data (y), middle - the data with the subtracted median (y-med(y)), right - the absolute deviations from the median (abs(y-med(y))).

![04_explore_11](https://user-images.githubusercontent.com/14832460/38313757-74758316-382d-11e8-85b2-60ceb99905db.png)

If you scroll lower, you will see the results of both Bartlett and Levene tests. The null hypothesis of the Bartlett and Levene tests assumes that variances across the groups are the same. The Bartlett test is more robust when the data comes from a normal distribution, while Levene test is more robust in case of departures from normality.

The first table displays the results of the Bartlett test and the second table displays those of the Levene test. The tables show the p-value of tests performed for each groups/subgroups. If the p-value of the test for a group is larger than the selected p-value threshold, groups are noted as "Equal". In this case there is not enough evidence to reject the null hypothesis, where the variances are considered equal. If the p-value of the test for a group is smaller than the selected p-value threshold, groups are noted as "Not equal". In this case, the null hypothesis is rejected and the variances are considered not equal.

![04_explore_12](https://user-images.githubusercontent.com/14832460/38313758-749abcc6-382d-11e8-9260-6599b19de290.png)

As indicated previously, the results of this sub-tab and the previous sub-tab are needed to for the ANOVA test performed in the following sub-tab. ANOVA assumes the data comes from a normal distribution and the variances are equal.

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### One / two sample test

In this subtab you can explore the differences between a certain value and your sample, or between two selected samples, with one/two sample t-test or Kolmogorov-Smirnov test (for non-parametric samples). 

First, select which test you would like to perform and how you wish to group your samples in the left panel of the main window:

![04_explore_14](https://user-images.githubusercontent.com/14832460/38313759-74c039c4-382d-11e8-88bc-c1280fbb21b5.png)

In the case of one-sample t-test you should enter "mu value" - to test for significant difference between the value and the mean value of your selected sample group. The results of the one t-test will be displayed above the boxplot:

![04_explore_15](https://user-images.githubusercontent.com/14832460/38313760-74e2702a-382d-11e8-8782-00677e7419f9.png)

For the two-sample t-test or Kolmogorov-Smirnov test, you should select two specific samples. The results of the test will be shown above the graph:

![04_explore_16](https://user-images.githubusercontent.com/14832460/38313761-750774ec-382d-11e8-8177-f9f4cba93d4c.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Test significant differences between groups

In this sub-tab, you can check for signifcant differences in the means between different groups using analysis of variance (ANOVA) or a non-parametric test (Kruskal-Wallis). A text box displays the p-value of the ANOVA test between different groups of Independent Variables:

![04_explore_17](https://user-images.githubusercontent.com/14832460/38313762-752b6ece-382d-11e8-8b2b-a87275348507.png)

If the p-value of the ANOVA test for a group is larger than the selected p-value threshold, groups are noted with "NO significant difference in means". In this case there is not enough evidence to reject the null hypothesis and the means of the groups are assumed equal. If the p-value of the ANOVA test for a group is smaller than the selected p-value threshold, groups are noted with "SIGNIFICANT difference in means". In this case the null hypothesis, where the means of the group are considered equal, is rejected and the means of the groups can be considered significantly different. 

A second text box displays the significant groups based on Tukey's pairwise comparison. Groups that share a common letter do not have significantly different means for the selected Dependent Variable. 

![04_explore_18](https://user-images.githubusercontent.com/14832460/38313763-754f0aaa-382d-11e8-8dad-7178ff45a294.png)

Boxplots display the distribution of the data for a specific trait (dependent variable) for the levels of the independent variable. The boxplots can be split by the second Independent Variable, which can be selected once you tick the checkbox for "Split graph?". You can also change the main Independent Variable to compare differences between Genotypes across the individual subsets:

![04_explore_19](https://user-images.githubusercontent.com/14832460/38313764-7572a4c4-382d-11e8-9816-bbc60a8fdf3e.png)

You can also run a non-parametric test, by selecting it from the drop-down menu:

![04_explore_20](https://user-images.githubusercontent.com/14832460/38313765-759745e0-382d-11e8-9421-a0bf12a3c55e.png)

In case of non-parametric test, Wilcoxon / Mann-Whitney test will be used to make the pairwise comparison between individual groups. The results of the test are displayed in the lower text box:

![04_explore_22](https://user-images.githubusercontent.com/14832460/38313768-760ffa94-382d-11e8-8ca5-ea30de0ce04a.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Two-way ANOVA

In this sub-tab you can explore the effect of two Independent Variables and the interaction between them. Select Independent Variable 1 and 2 from the drop-down menu in the main window:

![04_explore_23](https://user-images.githubusercontent.com/14832460/38313769-7638eb16-382d-11e8-9514-6781937ac340.png)

You can additionally subset your data for yet another Independent Variable, by selecting the "Subset the data?" box, and choosing the Independent Variable and a specific subset to be displayed / analysed:

![04_explore_24](https://user-images.githubusercontent.com/14832460/38313770-765d0a96-382d-11e8-8858-c2b960c7d5f6.png)

The results of two-way ANOVA are displayed in the text box below the interaction plot. The Independent Variable 1 (IV1) is the variable selected in the most left drop-down menu, while Independent Variable 2 (IV2) is the variable selected in the center drop-down menu:

![04_explore_25](https://user-images.githubusercontent.com/14832460/38313771-768a3c46-382d-11e8-94f6-daa863593fe4.png)

If you scroll down, you can see the residual plot of the two-way ANOVA shown above. The residuals should not show any pattern, indicating the linear relationship between independent variables. If this is not the case, the results of two-way ANOVA should not be trusted:

![04_explore_27](https://user-images.githubusercontent.com/14832460/38313775-76d5e650-382d-11e8-8a95-82e795896863.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

### 6. CORRELATIONS
This tab is used to check whether and how strongly your selected dependent variables (phenotypes) are related by creating a correlation matrix of the selected variable pairs. Correlation coefficients and p-values are provided for each variable pair. 

#### Select the dataset
First of all, select the dataset you would like to use to perform the correlation analysis. If you did not perform outlier removal or curve fitting, the "outliers removed" and "r2 fitted curves curated data" will not work properly, so please do not select them. 

Note: be aware of the outliers in your selected data. While statistically there’s no harm if the data contains outliers, they can significantly skew the correlation coefficient and make it inaccurate. You can spot them visually from the scatterplot or use the outlier removal feature from Data Curation tab and select the outlier removed data for your correlation analysis.

If you want to include / exclude some of the Dependent Variables from your data, you can do so by selecting or deselecting them from the "Choose from Dependent Variables to be plotted" window. 

![05_correlate_01](https://user-images.githubusercontent.com/14832460/38465501-31d36488-3b25-11e8-9bde-1828dc9a508b.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Select the correlation method

There are two methods you can choose from to calculate the correlation coefficients. The default Pearson r correlation is the most widely used correlation statistic to measure the degree of the relationship between linearly related variables. It assumes that your dataset to be correlated approximate the normal distribution and follow a linear relationship.(https://en.wikipedia.org/wiki/Pearson_correlation_coefficient).Alternatively, you can use Spearman correlation which assess statistical associations based on the ranks of the variables instead of the variables themselves, and it does not hold any assumptions about the distributions of your data. [Spearman correlation](https://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient).

![05_correlate_01a](https://user-images.githubusercontent.com/14832460/38466385-00cc8886-3b31-11e8-85af-2ec805db265f.png)

If you scroll down, you will find a table containing the [coefficient of determination](https://en.wikipedia.org/wiki/Coefficient_of_determination) (R2) and the p-value for the goodness of fit:

![picture1b](https://user-images.githubusercontent.com/14832460/38466384-008d4cf2-3b31-11e8-8570-5c780cc7d5c8.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Correlation for subsetted data

The default is to perform correlation analysis across all dependent variables (phenotypes) across all independent variables. You can also choose to use a subset of your data (for examples, phenotypes under a certain treatment, or from a certain day) to examine the correlation. 

Tick the checkbox “Subset your data for correlation analysis” and choose the specific subset to display the correlation for from the dropdown menu:

![05_correlate_02a](https://user-images.githubusercontent.com/14832460/38466387-01117586-3b31-11e8-8643-3e745649a4e6.png)

Once you select to subset your correlation, you will see a message displaying the top 5 most variable correlation pairs. Those pairs of Dependent Variables are determined by examining the variance in R2 of the correlation between the individual subsets of the Independent Variable. The pairs are NOT selected based on the p-values of the correlation, so the variance in correlation should be examined in more details, before making any conclusions:

![05_correlate_02b](https://user-images.githubusercontent.com/14832460/38466386-00ed6678-3b31-11e8-8489-5b81c5be2607.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Customize the correlation plot

You can choose the plotting method. The default method is "circle" where the correlation strength between individual Dependent Variables is represented by the size and color of the circle:

![05_correlate_02](https://user-images.githubusercontent.com/14832460/38465502-34eb9e06-3b25-11e8-8360-003d3595437f.png)

Some of the correlation plot method only represent the correlation strength with the color - such as "number" method, where the correlation coefficient values are refelcted in different levels in the color scale:

![05_correlate_03](https://user-images.githubusercontent.com/14832460/38465503-350b8144-3b25-11e8-85a3-927c1e7cf10b.png)

You can change the plot type, and plot the correlations between individual traits with full square matrix, or using lower or upper portion of the correlation matrix:

![05_correlate_04](https://user-images.githubusercontent.com/14832460/38465504-352a2c20-3b25-11e8-9b4c-3080a5fa432f.png)

You can also indicate the non-significant correlation with a cross, by ticking the box "indicate non-significant correlation", located lower in the sidebar panel and set the p-value threshold:

![05_correlate_05](https://user-images.githubusercontent.com/14832460/38465505-354cc23a-3b25-11e8-9acc-c3b7e41ce4a9.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Scatterplots

To examine the correlation between selected Dependent Variables in more details, you can use scatterplot. The data used for this graph is exactly the same data you chose in the "correlation plot" tab. From the sidebar panel, choose two Dependent Variable that you wish to plot on x- and y-axis respectively, and the Independent Variable that you would like to use to color-code the graph:

![05_correlate_06](https://user-images.githubusercontent.com/14832460/38465506-356c867e-3b25-11e8-9a8e-22218f0181c4.png)

You can choose to further subset your data by ticking the checkbox "Subset the data?" and selecting an Independent Variable for which you wish to subset:

![05_correlate_07](https://user-images.githubusercontent.com/14832460/38465507-358c1048-3b25-11e8-95fa-598b37cf05e4.png)

By scrolling with your pointer through the graph, you will get a specific information of the samples represented by individual data points. The sample identifier is representing GENOTYPE, Independent Variable, Timepoint and Sample ID (selected in "Data upload" tab). The R2 and p-value for individual correlations are reported in the figure legend, that you can view by selecting "Show the figure legend" checkbox:

![05_correlate_08](https://user-images.githubusercontent.com/14832460/38465508-35ad3d4a-3b25-11e8-8bf9-3e0f837420c8.png)

The scatterplot is interactive, so you can select the specific subsets indicated by individual colors to be hidden from the graph. Please NOTE that this will not affect the R2 and p-value presented in the figure legend since all the values used for the original graph will still be considered for those calculations :

![05_correlate_09](https://user-images.githubusercontent.com/14832460/38465509-35cf6424-3b25-11e8-9f83-2c7dc46fc7d0.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

### 7. PRINCIPAL COMPONENT ANALYSIS

Principal component analysis [(PCA)](https://en.wikipedia.org/wiki/Principal_component_analysis) is often used to simplify data into fewer dimensions and to check which traits explain the majority of the variation found in the population studied. However, PCA is often not explored to its full potential. You can, for example, run PCA on data subsetted by an Independent Variable (e.g.: treatment or a specific timepoint) or run PCA separately on those subsets to see how much each of your Dependent Variables (traits) contribute to explaining the observed variation. MVApp allows you to do all this!

#### Select data, subsets, and Dependent Variables
Select the dataset you would like to analyse from the dropdown menu at the top of the side panel. If you have not performed outlier removal or curve fitting, the "outliers removed" and "r2 fitted curves curated data" will not work properly, so please do not select them.

![06_pca_01](https://user-images.githubusercontent.com/14832460/38466629-ff660ec8-3b34-11e8-8417-67b89ab68b29.png)

Subsequently, select which Dependent Variables you would like to use for PCA:

![06_pca_02](https://user-images.githubusercontent.com/14832460/38466630-ff87d7a6-3b34-11e8-8cf2-3ca17b0a2ab9.png)

You can additionally select if you would like to scale the data (recommended if the values of individual Dependent Variables are differing in their scale), and run PCA on a specific subset of your data. After selecting all of the above, click "Unleash the PCA monster". 

![06_pca_03](https://user-images.githubusercontent.com/14832460/38466631-ffa9d9e6-3b34-11e8-870a-6796c7c05c45.png)

You can view the selected dataset in the first tab called "Selected dataset": 

![06_pca_04](https://user-images.githubusercontent.com/14832460/38466632-ffcc271c-3b34-11e8-80eb-c63ddbe68a5b.png)

The specific subset (scaled or non-scaled) can be viewed in the tab "Final data for PCA":

![06_pca_05](https://user-images.githubusercontent.com/14832460/38466633-ffee0d82-3b34-11e8-97b6-833ed54f1eae.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Visualize the principal components

In the sub-tab called ["Eigenvalues"](https://en.wikipedia.org/wiki/Eigenvalues_and_eigenvectors), you will find the [scree plot](http://www.improvedoutcomes.com/docs/WebSiteDocs/PCA/Creating_a_Scree_Plot.htm) showing the main principal components generated from the PCA. The principal components are shown in descending order based on the percentage variance that is explained by each component. You can download the plot as ".pdf" by clicking on "Download plot" button above the graph:

![06_pca_06](https://user-images.githubusercontent.com/14832460/38466634-000eb2ee-3b35-11e8-9c14-e4012a692641.png)

Below the graph, you can view a default figure legend:

![06_pca_07](https://user-images.githubusercontent.com/14832460/38466635-002f2c4a-3b35-11e8-92b5-87573a4baf8c.png)

If you scroll down in the main window, you will find a table summarizing the eigenvalues of each principal component (comp), their percentage of variance explained and the cumulative percentages of all the components that add up to 100%. The table can be downloaded as a ".csv" file, by clicking on the "Download table" button:

![06_pca_08](https://user-images.githubusercontent.com/14832460/38466636-00517b1a-3b35-11e8-9aa6-e01c9a731469.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Visualize the contribution of each Dependent Variable to the principal components

In the sub-tab 'Contribution per variable', you can visualize the Dependent Variable contribution of each selected principal component. Select the principal components to be plotted on x- and y-axis from the drop-down menu below the graph. The values between brackets on the x- and y-axis indicate the percentage of the variance explained:

![06_pca_09](https://user-images.githubusercontent.com/14832460/38466637-0071b92a-3b35-11e8-9a0c-02a962390515.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### What are the principal component coordinates for individual samples?

By scrolling down, you can find the PC coordinates of each sample represented as a scatter plot. The x- and y-axis are controlled by the same dropdown menu as the contribution plots. You can color the plot by any of the Independent Variable that you select from the dropdown menu. 

![06_pca_10](https://user-images.githubusercontent.com/14832460/38466638-0093d690-3b35-11e8-9741-227ef33dce3b.png)

You can check if there is a separation in the PC coordinates among different genotypes / treatments / timepoints in your samples by changing the color-coding of the graph on the left panel:

![06_pca_11](https://user-images.githubusercontent.com/14832460/38466639-00d17f7c-3b35-11e8-9ef1-3b0925d0fb88.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Explain principal components by examining the contribution of Dependent Variables

In the sub-tab 'Contribution per PC', the contribution of the Dependent Variables to each PC is displayed. You can download the associated graphs by clicking the "Download plot" button:

![06_pca_12](https://user-images.githubusercontent.com/14832460/38466640-00f29428-3b35-11e8-89d3-9dc7060667dd.png)

If you scroll down, you will find a table summarizing the contribution of the Dependent Variables to each PC/dimension (Dim). You can download the table containing the percentange contribution data per PC as a ".csv" file by clicking the "Download the data" button:

![06_pca_14](https://user-images.githubusercontent.com/14832460/38466641-0112713a-3b35-11e8-8158-815a2296d09e.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

### 8. MULTIDIMENSIONAL SCALING

Multidimensional scaling [(MDS)](https://en.wikipedia.org/wiki/Multidimensional_scaling) is a multivariate data analysis approach that is used to visualize the similarity/dissimilarity between samples by plotting points in two dimensions. The input data for MDS is a dissimilarity matrix representing the distances among pairs of objects. MDS is mathematically and conceptually similar to PCA and factor analysis, but PCA is more focused on the dimensions themselves and seeks to identify the traits that explain the most variance, whereas MDS is more focused on the relationships found between the scaled objects.

#### Select data, subsets, and dependent variables
Select the dataset you would like to analyze from the dropdown menu at the top of the side panel. If you have not performed outlier removal or curve fitting, the "outliers removed" and "r2 fitted curves curated data" will not work properly, so please do not select them.

![07_mds_01](https://user-images.githubusercontent.com/14832460/38466843-e4d96c64-3b37-11e8-9837-659e387e5e5c.png)

Subsequently, select which Dependent Variables you would like to use in MDS:

![07_mds_02](https://user-images.githubusercontent.com/14832460/38466844-e4fac292-3b37-11e8-989b-5cf638d872c9.png)

You can additionally select whether you would like to scale the data (recommended if the values of individual Dependent Variables are differing in their scale), and run MDS on a specific subset of your data. If you would like to segregate your scaled samples into a number of clusters, you can select "Cluster samples using k-means" checkbox and choose the number of clusters you would like to use. 

After selecting all the above options, you can click on "Unleash the power of MDS". You will then view the selected dataset in the first tab called "Selected dataset": 

![07_mds_03](https://user-images.githubusercontent.com/14832460/38466845-e51f2b96-3b37-11e8-85b4-47f6d607bd02.png)

The specific subset (scaled or non-scaled) can be found in the tab "Final data for MDS":

![07_mds_04](https://user-images.githubusercontent.com/14832460/38466846-e53ffae2-3b37-11e8-84c2-1630bcc32ac0.png)


[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Multidimensional scaling of individual samples

In the sub-tab "MDS of the samples", you can view a scatter plot showing the two dimensions resulting from the MDS. If the k-means clustering option was selected, the individual samples would be displayed in colors corresponding to different color-coded clusters. This plot can be downloaded as a ".pdf" file, by clicking the "Download plot" button"

![07_mds_05](https://user-images.githubusercontent.com/14832460/38466847-e560bc5a-3b37-11e8-8549-370054552454.png)

By scrolling with your pointer through the graph, you will find specific information regarding your samples. The sample identifier is representing Genotype, Independent Variable, Time/Gradient and Sample ID (selected in "Data upload" tab). 

![07_mds_06](https://user-images.githubusercontent.com/14832460/38466848-e583c628-3b37-11e8-9d60-f29dfeba58fa.png)

If you scroll down, you will see the table summarizing the coordinates of individual samples as calculated with MDS, including the K-means clusters if the option for "k-mean clustering" was chosen. The table can be downloaded as a ".csv" file, by clicking on the "Download table" button.

![07_mds_07](https://user-images.githubusercontent.com/14832460/38466849-e5a53934-3b37-11e8-90f0-8e3f2ea2eb37.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Multidimensional scaling of the selected Dependent Variables

In the sub-tab "Scaling of traits", you can find an MDS performed on the selected Dependent Variables. The plot showing the coordinates of each Dependent Variable is displayed and color-coded by cluster number if that option was included. This kind of plot can provide you with an insight into the relationships between the measured traits:

![07_mds_08](https://user-images.githubusercontent.com/14832460/38466850-e5c56b0a-3b37-11e8-8012-7125123afd68.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

### 9. HIERARCHICAL CLUSTER ANALYSIS

Hierarchical cluster analysis is an algorithmic approach to find discrete groups with varying degrees of (dis)similarity. The samples are hierarchically organised depending on the selected method and may be presented as a dendrogram. Hierarchical clustering is commonly used in discretising largely continuous ecological phenomena to aid structure detection and hypothesis generation. For example, if data were collected along a gradient, cluster analysis may help to identify distinct regions therein which may correspond to an ecologically meaningful grouping. Similarly, the hierarchical cluster analysis can be used in phenotype analysis from the experiment performed with and without stress, and group different genotypes into groups that show similar responses to stress conditions. 

The hierarchical clustering approach was previously used for clustering the Arabidopsis accessions based on their [root](http://www.plantphysiol.org/content/early/2014/09/30/pp.114.248963) and [shoot](https://academic.oup.com/jxb/article-lookup/doi/10.1093/jxb/erw015) responses to salt stress. Now you can perform this analysis within minutes on your own data using MVApp.

#### Selecting the data

Select the dataset to analyse from the dropdown menu at the top of the side panel. If you did not perform outlier removal or curve fitting, the "outliers removed" and "r2 fitted curves curated data" will not work properly, so please do not select them. 

Subsequently, select which Dependent Variables you want to use in the Hierarchical clustering. Please be aware that unlike PCA, the greater number of traits will result in larger number of groups identified. Therefore - we advise to limit the number of dependent variables used as an input for Hierarchical Clustering Analysis:

![08_hclust_02](https://user-images.githubusercontent.com/14832460/38467540-c424dc6e-3b42-11e8-87d6-faad73f502bc.png)

You can additionally select whether you would like to scale the data (recommended if the values of individual dependent variables are differing in their scale), perform the Hierarchical Clustering on the mean data (means are calculated per Genotype, Independent Variable and Time points selected in "Data upload" tab), or run Hierarchical Clustering on a specific subset of your data. At this point you should also select method for clustering the samples. 

You can chose between the following:
* [Ward’s minimum variance method](https://en.wikipedia.org/wiki/Ward%27s_method)
* [Complete linkage](https://en.wikipedia.org/wiki/Complete-linkage_clustering)
* [Single linkage](https://en.wikipedia.org/wiki/Single-linkage_clustering)
* [Average linkage](http://www.saedsayad.com/clustering_hierarchical.htm)
* [Mcquitty - also known as Weighted Pair Group Method with Arithmetic Mean](https://en.wikipedia.org/wiki/WPGMA)
* Median clustering or Weighted Pair Group Method with Centroid Averaging (WPGMCA)
* [Centroid clustering](https://nlp.stanford.edu/IR-book/html/htmledition/centroid-clustering-1.html)

![08_hclust_03](https://user-images.githubusercontent.com/14832460/38467541-c445d892-3b42-11e8-948c-a20e99eccf37.png)

After selecting all of the above you can click "Unleash cluster analysis". You can view the selected dataset in the first tab called "Selected dataset", while the specific subset (scaled or non-scaled) used for the Hierarchical Clustering is displayed in the tab "Final data used for HClust":

![08_hclust_04](https://user-images.githubusercontent.com/14832460/38467542-c4660d74-3b42-11e8-99ad-1e87f2f68b7f.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### View the clusters and select the similarity distance for cluster separation

The relationship between the accessions is established depending on the selected dependent variables and the method. In the sub-tab "Clustering your HOT HOT data" you can view a heatmap of the selected dependent variables displayed as separate rows, while the individual (or mean) values corresponding to individual samples will be displayed in separate columns. 

![08_hclust_05](https://user-images.githubusercontent.com/14832460/38467543-c4880096-3b42-11e8-9c4d-46ae129f6b86.png)

If you scroll down, you will find a dendrogram representing individual samples that are clustered as in the heatmap above, but now you will be able to see the (dis)similarity distance between the samples. Enter the distance at which you wish to separate the data into the clusters in the window "Separate clusters at:". 

![08_hclust_06](https://user-images.githubusercontent.com/14832460/38467544-c4aa656e-3b42-11e8-9da8-d8dc2c9ee2ce.png)

As soon as you enter a value, the message above the dendrogram will change, displaying the number of clusters. Please be aware that having too many clusters might not be informative and will significantly slow, or even crash, the cluster validation step.

![08_hclust_07](https://user-images.githubusercontent.com/14832460/38467545-c4cbb872-3b42-11e8-92b2-23bbc971bf4f.png)

If you scroll even further down, you will find a table containing the cluster ID for your specific samples.

![08_hclust_08](https://user-images.githubusercontent.com/14832460/38467546-c4eec966-3b42-11e8-8309-172747cdc16f.png)

#### Cluster Validation

In the sub-tab "Cluster validation" you will find a message box displaying all the dependent variables for which ANOVA found significant effect of the clusters. In the graph below the message box you can find a box-plot representing individual clusters and the letters above the graph display significant groups calculated using Tukey.HSD test for pairwise comparison.

![08_hclust_10](https://user-images.githubusercontent.com/14832460/38467547-c511dfa0-3b42-11e8-8f4d-265696cfcb6c.png)

You can view individual dependent variables by selecting them from a drop-down menu "View" above the box-plot.

![08_hclust_11](https://user-images.githubusercontent.com/14832460/38467548-c5341b9c-3b42-11e8-98d2-78e5378d97ac.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

### 10. K-MEANS CLUSTER ANALYSIS

K-means clustering is often used to find groups a data set, when categories or groups in the data are unknown. The K-means algorithm assigns the individuals to a number of centroids, defined by the user. The Euclidean distance between the individual and the cluster mean is computed and the individual is assigned to the closest centroid, so that the samples within the same cluster (K) are as similar as possible. This analysis is useful to confirm user's hypotheses about the existance of possible groups or to detect unidentified groups in complex datasets.

#### Selecting the data

Select the dataset to analyse from the dropdown menu at the top of the side panel. If you did not perform outlier removal or curve fitting, the "outliers removed" and "r2 fitted curves curated data" will not work properly, so please do not select them. 

![09_kmclust_01](https://user-images.githubusercontent.com/14832460/38468250-f03fac4e-3b4b-11e8-82a6-41e5a7f8e94f.png)

Subsequently, select which Dependent Variables you want to use in the K-means clustering. We recommend that you only include non-redundant, informative traits, which can be selected by visualizing the correlations between all traits. The use of highly correlated traits could increase the importance of these specific traits and skew the clustering. Thus, excluding redundant traits is advised.

You can additionally select whether you would like to scale the data (recommended if the values of individual Dependent Variables are in different units), perform the K-means Clustering on the mean data (means are calculated per Genotype, Independent Variable and Time points selected in "Data upload" tab), or run K-means Clustering on a specific subset of your data. 

![09_kmclust_02](https://user-images.githubusercontent.com/14832460/38468251-f063153a-3b4b-11e8-96be-274830255685.png)

You can view the selected dataset in the first tab called "Selected dataset", while the specific subset (scaled or non-scaled) used for the K-means clustering is displayed in the tab "Final data used for K-means":

![09_kmclust_03](https://user-images.githubusercontent.com/14832460/38468252-f0860d7e-3b4b-11e8-91ee-fbdddbba3e94.png)

#### Optimal cluster number estimation

The number of clusters or centroids (K) must be defined by the user. If you do not know what is the best number of clusters for your data, you can run a preliminary analysis in the sub-tab "Optimal number of clusters" by clicking "Unleash optimal cluster number estimation" button in the sidepanel.

It will take some time for this analysis to finish - so please be patient...

![09_kmclust_04](https://user-images.githubusercontent.com/14832460/38468253-f0aa5a3a-3b4b-11e8-8794-29edc80a566f.png)

After the estimation step finishes running, the graphs will appear in the main windown in the sub-tab "Optimal number of clusters". The graphs represent a graphical methods for cluster estimation. 

The first graph visualize the [elbow method](https://bl.ocks.org/rpgove/0060ff3b656618e9136b). You can identify the optimal number of clusters by identifying the point at which the line is making the sharpest turn, so called "elbow".

![09_kmclust_05](https://user-images.githubusercontent.com/14832460/38468254-f0cccd2c-3b4b-11e8-9630-a905e453ec69.png)

If you scroll down, you will see the graph representing the ["silhouette method"](https://kapilddatascience.wordpress.com/2015/11/10/using-silhouette-analysis-for-selecting-the-number-of-cluster-for-k-means-clustering/), where the optimal number of clusters is indicated by dashed line.

![09_kmclust_06](https://user-images.githubusercontent.com/14832460/38468255-f0efd560-3b4b-11e8-9a76-6a5d9674fe4d.png)

Scrolling even lower, you will find a message box displaying the results of the cummulative tests, using 30 different indeces, indicating the best cluster number according to the majority rule. 

![09_kmclust_07](https://user-images.githubusercontent.com/14832460/38468256-f114adae-3b4b-11e8-903a-e4bd7de7867d.png)

Below the message box, you will find the graphical representation of the cummulative cluster number indicated by different methods. 

![09_kmclust_08](https://user-images.githubusercontent.com/14832460/38468257-f137e33c-3b4b-11e8-80c3-1a28d8efaa5e.png)

#### Performing k-means clustering 

Once you decided on the number of clusters to be used for k-means clustering, you should enter the cluster number in the "Cluster number" box and click "Unleash cluster analysis" on the side-panel.

![09_kmclust_09](https://user-images.githubusercontent.com/14832460/38468258-f15b8738-3b4b-11e8-8e75-d40d9d4c3b82.png)

The results of the k-means clustering will be displayed in sub-tab "K-means clustering plots", where you can view the individual samples plotted in the order of the selected Dependent Variable from the drop-down menu "Variable to plot" in the main window. The colors represent the individual clusters.

![09_kmclust_10](https://user-images.githubusercontent.com/14832460/38468259-f17ee200-3b4b-11e8-835b-f5df3eea9ee2.png)

You can split the graph by selecting "Split the graph" checkbox and the Independent Variable to split by, as well as modify the appearance of the graph modifying the boxes on the right-hand side above the graph. 

![09_kmclust_12](https://user-images.githubusercontent.com/14832460/38468260-f1c3596c-3b4b-11e8-80ef-9c702161faf7.png)

In the sub-tab "K-means clustering scatter plots", you can plot the corelation between two selected Independent Variables, selected from the drop-down menus in the upper left corner above the plot. 

![09_kmclust_13](https://user-images.githubusercontent.com/14832460/38468261-f1e54d24-3b4b-11e8-9b47-b9a66dc0d7b5.png)

You can split the graph by selecting "Split the graph" checkbox and the Independent Variable to split by, as well as modify the appearance of the graph modifying the boxes on the righ-hand side above the graph. 

![09_kmclust_14](https://user-images.githubusercontent.com/14832460/38468262-f215c76a-3b4b-11e8-8872-1a8e0cc5cd55.png)

Finally, in the sub-tab "K-means clustering data table" you can view the table and which of your samples belong to which clusters. The columns with cluster identity is all the way at the right side end of the table. You can download the table as a ".csv" file by clicking on the button "Download data". 

![09_kmclust_15](https://user-images.githubusercontent.com/14832460/38468263-f2382594-3b4b-11e8-8db6-4f1272c9deea.png)


### 11. HERITABILITY

Heritability is the proportion of the phenotypic variance that can be attributed to genetic variance. This statistic is important in the fields of genetics in order to assess if a trait is heritable (genetically controlled). MVapp allows you to calculate the [broad-sense heritability](https://www.ncbi.nlm.nih.gov/books/NBK21866/), which is the ratio of total genetic variance to total phenotypic variance. 

NOTE! Please, be aware that in order to estimate heritability, you should have at least 5 different genotypes. 

#### Selecting the data

Select the dataset to analyse from the dropdown menu at the top of the side panel. If you did not perform outlier removal or curve fitting, the "outliers removed" and "r2 fitted curves curated data" will not work properly, so please do not select them. 

![10_heritability_01](https://user-images.githubusercontent.com/14832460/38468477-440a82e2-3b4f-11e8-97a6-f5ea31a908d7.png)

If you performed your experiment across different years or experimental batches, please select the column indicating the year / experimental batch from the drop-down menu "Select column containing experimental batch / year". If you do not have this information, or your data was collected from one experiment, select "none". The model will still be able to run.

The same applies for the drop-down menu "Select column containing location".

![10_heritability_02](https://user-images.githubusercontent.com/14832460/38468478-442f697c-3b4f-11e8-84cd-fb848ec15104.png)

Subsequently, enter the number of the replications per location and per year. 

![10_heritability_03](https://user-images.githubusercontent.com/14832460/38468479-44544ac6-3b4f-11e8-8405-18875d13b8fd.png)

If your data contains different treatments, you can split your data by selecting "Split the data?" checkbox and selecting an Independent Variable from the drop-down menu for which you wish to split. 

![10_heritability_04](https://user-images.githubusercontent.com/14832460/38468480-44787720-3b4f-11e8-85ff-dfe62594e84e.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Estimated broad-sense heritability 

In the main window, the message box will give a summary of information entered (number of replications, number of years/locations and unique values per year/location) and the summary of the model used to calculate heritability.

![10_heritability_05](https://user-images.githubusercontent.com/14832460/38468481-449d3f7e-3b4f-11e8-9494-ba761b7884f4.png)

In case you wish to subset your data even further, you can do it by selecting "Subset the data?" checkbox and selecting an Independent Variable from the drop-down menu for which you wish to subset, as well as specific subset to be displayed. As soon as you do that, the estimated broad-sense heritability values will adjust.

![10_heritability_06](https://user-images.githubusercontent.com/14832460/38468482-44bf3c46-3b4f-11e8-8389-fe9b2ab26dd9.png)

You can compare the heritability values between individual subset by changing them in the drop-down menu "Use subset"

![10_heritability_07](https://user-images.githubusercontent.com/14832460/38468483-44e117ee-3b4f-11e8-960b-4b9ec4f98d6a.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

### 12. QUANTILE REGRESSION

[Quantile regression](https://en.wikipedia.org/wiki/Quantile_regression) is a way to estimate the conditional quantiles of a response variable distribution in the linear model that provides a complete view of possible causal relationships between variables. Quantile regression minimizes absolute errors and can provide a more comprehensive analysis of the effect of the predictors on the response variable than mean regression. Linear quantile regression is related to linear least-squares regression as both try to study the relationship between the predictor variables and the response variable, the only difference being that least-squares involves modeling the conditional mean of the response variable, whereas, quantile regression models the conditional quantile of response. It is especially useful in applications where extremes are important, such as environmental studies where upper quantiles of yield are critical.

#### When should you use it?

Quantile regression estimates are more robust against outliers in the response, so if your response variable has potential outliers or extreme data, then ordinary least squares (OLS) regression is more effected as mean is more effected by outliers, you can use median regression as a substitute. If your errors are non-normal then OLS is inefficient, but quantile regression is robust. If your data fails to satisfy the assumption of homoscedatcity of the error terms, then you can use this technique, as there is no such assumption required here. Beyond that,  quantile regression also provides a richer characterization of the data, allowing us to consider the impact of an explantory variable on the entire distribution of response, not merely its conditional mean.

#### Select the dataset

Select which dataset you would like to use to perform quantile regression from the drop-down menu at the top of the side panel. If you did not perform outlier removal or curve fitting, the "outliers removed" and "r2 fitted curves curated data" will not work properly, so please do not select them. 

![11_qr_01](https://user-images.githubusercontent.com/14832460/38468577-16e393c4-3b51-11e8-83c7-c660bebf5a61.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Select reponse, explanatory variable, subsets

Select the phenotype you want as response of your quantile regression, you can only choose one variable. 

![11_qr_02](https://user-images.githubusercontent.com/14832460/38468578-1707cdd4-3b51-11e8-86f6-d803c3379ef2.png)

Select the independent varaibles to subset the data, you can choose a maximum of two variables. 

![11_qr_03](https://user-images.githubusercontent.com/14832460/38468579-172b55f6-3b51-11e8-9310-2c032ee209a3.png)

Then choose the explanatory variables of your quantile regression model, you can choose any number of explanatory variables. 

![11_qr_04](https://user-images.githubusercontent.com/14832460/38468580-174f54c4-3b51-11e8-8c21-dee3a91879df.png)

You can also choose a p-value threshold to test the significance of the explantory variables. You have the option to scale the data which might be useful if your variables are in different units. After you select all of the neccessary parameters you can click on "Unleash the power of Quantile Regression" button. The selected dataset will be displayed in "Selected Dataset" sub-tab in the main window.

![11_qr_04a](https://user-images.githubusercontent.com/14832460/38468628-bb6263e4-3b51-11e8-9fcc-05b964e0654e.png)

The specific subset, selected for the quantile regression with / without scaling is displayed in "Final data for analysis" sub-tab.

![11_qr_05](https://user-images.githubusercontent.com/14832460/38468581-17758978-3b51-11e8-8221-dc83ea4cc6a0.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Results of quantile regression

The result of the quantile regression model can be seen in the sub-tab 'Modelled data'. You can chose a specific subset to view from the drop-down menu "Use subset" above the message box.

![11_qr_06](https://user-images.githubusercontent.com/14832460/38468582-1798d072-3b51-11e8-8114-ea64d9fd1abb.png)

The message box displays the significant phenotypes for lower, median and upper quantiles of the response for the particular subset chosen from the drop down list. You can choose the subset whose result you want to see in the message box.

The results from all the quantile regression models for different subsets are tabulated. The table can be downloaded as a ".csv" file by clicking the button "Download modelled data" containing all the results. 

![11_qr_07](https://user-images.githubusercontent.com/14832460/38468583-17bdc396-3b51-11e8-90b7-c64cedecef02.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Visualize the quantile regression results

The plots of the regression models are displayed in the sub-tab 'Quantile plots'. You can choose the independent variable by which you want to group your plot. If you have chosen two independent variables to subset your data, then you can also choose the value of your another subset variable whose result you want to see. 

If you view a single plot, then choose the particular phenotype you want to view. 

The coefficients of the phenotype are plotted against the quantile level. The colored dots represent that the variable is significant for the particular quantile level and the cross sign represent that it is not significant. The different colors represent the different unique realizations of the grouping variable. The different lines can be used to compare the behavior of phenotypes in different conditions or different days, depending on the grouping variable. The plot can be downloade by using the "Download plot" button above the plots.

![11_qr_08](https://user-images.githubusercontent.com/14832460/38468584-17e1fa36-3b51-11e8-8f2c-adbc673c8730.png)

If you want to view the results of all your phenotypes the you can choose 'multiple plots' from "View plots as:". The panel displaying the plots will update automatically. 

![11_qr_09](https://user-images.githubusercontent.com/14832460/38468585-1805e824-3b51-11e8-8654-d40c19fafae7.png)

You can view the contribution of different Dependent Variables by using a scroll bar "Show plot of variables starting from..."

![11_qr_10](https://user-images.githubusercontent.com/14832460/38468586-1829ad90-3b51-11e8-8f80-8d0765ed17be.png)

[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

#### Quantile plots

If you choose to view your plot as single plot, the quantile plot of the phenotype chosen will be displayed. The coefficients of the phenotype are plotted against the quantile level. The colored dots represent that the variable is significant for the particular quantile level and the cross sign represent that it is not significant. The different colors represent the different unique realizations of the grouping variable. The different lines can be used to compare the behavior of phenotypes in different conditions or different days, depending on the grouping variable. The plot can be downloade by using the "Download plot" button at the bottom.

![single plot](https://user-images.githubusercontent.com/34129392/35572608-0852d162-05e6-11e8-82b9-98582869480c.png)

If you choose to view your plots as multiple plots, the quantile plots of all the phenotypes will be displayed. You can download these plots using the "Download plot" button at the bottom.

![multiple plots](https://user-images.githubusercontent.com/34129392/35572607-0832032e-05e6-11e8-884f-b708e1aad2b7.png)

If you have more than four explanatory variables, then you can use the slider to view more plots.

![multiple plots slider](https://user-images.githubusercontent.com/34129392/35572605-080e38b8-05e6-11e8-9160-c9302cd56ac6.png)


[GO BACK TO TABLE OF CONTENTS](#table-of-contents)

