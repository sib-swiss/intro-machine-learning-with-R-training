# Introduction to Machine Learning (with R)

This repository regroups works on the "introduction to machine learning" course of the SIB. 
This is the R implementation of this course. We propose a separate [python implementation](https://github.com/sib-swiss/intro-machine-learning-training).


## pre-requisites

The course is targeted to life scientists who are already familiar with the R programming language and who have a basic knowledge of statistics.

In order to follow the course, you need to have installed R and Rstudio.

You will also need to have installed the following packages:

**from CRAN** (install with `install.packages()`):
 
 * BiocManager
 * dendextend
 * dplyr
 * ggplot2
 * GGally
 * ggbiplot
 * gplots
 * gridExtra
 * important
 * pheatmap
 * ranger
 * RColorBrewer
 * stringr
 * tidyr
 * tidymodels
 * recipes
 * rpart.plot
 * themis
 * vip
 * umap

**from Bioconductor** (install with `BiocManager::install`)

 * bluster


This can be done by running the code:

```r
install.packages(c(
  "BiocManager", "dendextend", "dplyr", "ggplot2", "GGally", "ggbiplot", "gplots",
  "gridExtra", "important", "pheatmap", "ranger", "RColorBrewer", "stringr", "tidyr", 
  "tidymodels", "recipes", "rpart.plot", "themis", "vip", "umap"))
BiocManager::install("bluster")
```

## course organization

The course is organized in several numbered Rmarkdown documents, each corresponding to a chapter which interleaves theory, code demo, and exercises.

    Chapter1 : Exploratory analysis
    Chapter2 : Unsupervised learning
    Chapter3 : Supervised learning and Machine Learning routine
    Chapter4 : Distance-based model for classification
    Chapter5 : Decision trees for classification
    Chapter6 : Machine Learning for regression


Solutions to each practical can be found in the solutions/ folder.


## directory structure

 * data : contains the datasets
 * exam : contains the data and instructions of an optional exam
 * images : images generated or used in the notebooks
 * R_notebooks : folder containing the Rmarkdown chapters

