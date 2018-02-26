# Introduction

Many thanks for considering to contribute to the MVApp. It is people like you that help our community grow and develop new angles to transparent data curation and enriching data analysis. You are a STAR and your opinion is of great value to us. 

This document aims to give an overview of how to contribute to MVApp. We encourage third-party contributions in a variety of forms. There are a few guidelines that we need contributors to follow so that we can keep things working for all users.

### How can you contribute?

* Report bugs
* Request features
* Join the discussion of open issues, features, and bugs
* Revise existing code
* Add new features to the code
* Add or revise documentation

### We are not accepting contributions concerning:

* storing the data on the server
* temporarily modifying the data by hand during the session

If you need any help or have more questions, please contact us at magdalena.m.julkowska@gmail.com

Following these guidelines helps to communicate that you respect the time of the developers managing and developing this open source project. In return, they should reciprocate that respect in addressing your issue, assessing changes, and helping you finalize your pull requests.
 
# Ground Rules

1. Please make sure that the communications with the developers managing and maintaining MVApp are within the [code of conduct](https://github.com/mmjulkowska/MVApp/blob/master/CODE_OF_CONDUCT.md)
2. Make sure you have a github account - annonymous contributions are discouraged
3. If you contribute new feature, you are responsible for creating a README part for the feature, as well as the maintenance of it and resolving the bugs that are related to this feature

# Contributions

### Report a bug:

* Search GitHub and Google to see if your issue has already been reported
* If the issue was not reported previously - please [create a new issue](https://github.com/mmjulkowska/MVApp/issues)
* Clearly describe the issue including:
  * What version of the MVApp were you using and whether you used local (run from your R/Rstudio) or webbrowser version
  * What operating system / browser you are using 
  * What are the steps you did?
  * What did you expect to see?
  * What did you see instead? - attach screenshots if possible
* The bugs are discussed in a weekly meeting of the developers

### Suggest a new feature:

* If you have a particular analysis that you would like to have included in the MVApp, please [create a new issue](https://github.com/mmjulkowska/MVApp/issues)
* In your issue, please make sure to include following information:
  * What kind of data will this feature be applicable to? (numeric / categorical / ordinal / count)
  * Would it fit in any of the existing tabs?
  * How could this new feature be used bo other people in the community?
* In case you do have an R-code that you usually use for performing the new suggested feature - please add it to your issue and make sure to add informative comments using "#" to ensure that the code will be clearly readable for anyone else
* Also - please indicate in your issue if you would like to be involved in contributing to the development of this feature and what is your desired level of contribution / commitment:
  * Do you want to code the feature yourself? 
  * Do you require any help in coding the feature?
  * IF NO SHINY CODING SKILLS: Do you want to be involved in design of the user interface? 
  * Can you provide an example dataset that we can use to develop this feature? (the dataset will be kept confidential)
  * Are you willing to write a README
* The suggestions of new features are discussed in a weekly meeting of the developers. 
* Suggestions for new features can be considered as significant intellectual contributions to the MVApp, and their authors might be included as contributors in the new MVApp release, depending on their level of commitment. 

### Contribute a new feature:

* When you contribute new feature - please add it as an [issue](https://github.com/mmjulkowska/MVApp/issues) first, to allow others to comment and give feedback. Please follow the guidelines for "Suggesting new feature" as described above.
* When you want to create a new feature or non-trivial change to existing code, create a ['pull request'](https://help.github.com/articles/creating-a-pull-request/) to your own account or request to be a contributor to the project at magdalena.m.julkowska@gmail.com and pull your branch to work on the project.
* Always work in your branch rather than on the master branch. Branches focus on fixing or adding a single feature or set of closely related features because this will make it easier to review and merge your contributions. Since multiple people work on the same repository, make sure to keep your master branch in sync with the master of the mmjulkowska/MVApp repository. The master branch is the central repository for stable releases, browser-version of the MVApp and the latest code.
* Add descriptive comments using "#" to your feature and clearly mark it, so that it will be easy to find among many lines of code. Do it in the ui.R as well as in the server.R files.
* Test your new features in the local R/Rstudio environment on your own machine prior to commiting the changes to your branch. 
* Add documentation for your new feature: 
    * A new documentation consists of the entry into the README file.
    * The README is written using the markdown langage which is very easy to follow - the syntax is described [here](https://guides.github.com/features/mastering-markdown/)
    * The screenshots to the README can be added by creating the image link code in an "issue" - please include "README screenshot ..." in the issue title, so it would be obvious for the developers team that this is not an issue that requires solving. 
    * For uploading the tutorial video to our [Youtube channel](https://www.youtube.com/channel/UCeTCqj3dHWbjIbt9cXVjHMQ) please contact magdalena.m.julkowska@gmail.com. Tutorial video shows how to use your feature from data-upload to the analysis part. 
* After you finished the contribution please create a merge request. The merge request will be resolved by the developing team during a weekly meeting.
* Succesful contributions to the project will be acknowledged by adding their developer into the list of contributors in the next release of the MVApp.

### Small contributions:

* Small contributions such as fixing spelling errors, where the content is small enough to not be considered intellectual property, can be submitted by a contributor as an ["issue"](https://github.com/mmjulkowska/MVApp/issues)
* As a rule of thumb, changes are obvious fixes if they do not introduce any new functionality or creative thinking. As long as the change does not affect functionality, some likely examples include the following:
  * Spelling / grammar fixes
  * Typo correction, white space and formatting changes
  * Comment clean up
  * Bug fixes that change default return values or error codes stored in constants
  * Adding logging messages or debugging output
  
