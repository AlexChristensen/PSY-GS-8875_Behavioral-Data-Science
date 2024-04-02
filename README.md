# PSY-GS-8875 | Behavioral Data Science | Spring 2024

### Instructor
Alexander P. Christensen

Assistant Professor

Hobbs Hall, Room 221

[alexander.christensen@vanderbilt.edu](mailto:alexander.christensen@vanderbilt.edu)

Office Hours: by email

### R packages

```
# CRAN packages
install.packages(
  pkgs = c(
    "remotes", "reticulate", "tictoc",
    "ggplot2", "caret", "glmnet", "GGally",
    "MASS", "ggpubr", "keras", "tensorflow", 
    "ranger", "cluster", "faraway",
    "NbClust", "factoextra", "aricode", 
    "EGAnet", "textdata", "tm", "tidyverse"
  )
)

# GitHub packages
remotes::install_github("AlexChristensen/toolchainR")
remotes::install_github("AlexChristensen/latentFactoR")
remotes::install_github("atomashevic/transforEmotion")
```

### [Course Outline](https://docs.google.com/spreadsheets/d/1h7zq3rfj3JwyiW6lUcTt4Uid7i0BFrt1/edit?usp=sharing&ouid=113368369928842739072&rtpof=true&sd=true)

Week 1: Introduction and Retrieval-augmented Generation

Week 2: Regression and Classification (with gradient descent)

+ (linear) regression

+ classification (logistic regression)

+ gradient descent

Week 3: Generalizability

+ data splitting (training/testing)

+ bootstrap

+ (k-folds) cross-validation

Week 4: Ridge and LASSO Regularization

+ ridge ($\ell_2$-norm) regression

+ lasso ($\ell_1$-norm) regression

Week 5-6: (Artificial) Neural Networks

+ artificial neural networks

+ activation functions

+ backpropagation

+ training tips and tricks

Week 7-8: Trees and Forests

+ classification and regression trees (CART)

+ bootstrap aggregation (bagging)

+ random forests

Week 9-10: $k$-means and Hierarchical Clustering

+ distances

+ $k$-means/mediods

+ hierarchical clustering

Week 11-12: Exploratory Graph Analysis (EGA)

+ (psychometric) networks 

+ network estimation

+ community detection

+ exploratory graph analysis

+ unidimensionality

Week 12-13: EGA Framework

+ bootstrap EGA

+ local dependence detection (Unique Variable Analysis)

+ (network) loadings and scores

+ (metric) invariance

+ hierarchical EGA

Week 14: Final Project

### Data

#### Social Sciences

[Inter-university Consortium for Political and Social Research](https://www.openicpsr.org/openicpsr/)

[Open Psychometrics](https://openpsychometrics.org/_rawdata/)

[Journal of Open Psychology Data](https://openpsychologydata.metajnl.com/)

[Open Science Framework](https://osf.io/search/)

#### General

[Kaggle](https://www.kaggle.com/datasets)

[UCI Machine Learning Repository](https://archive-beta.ics.uci.edu/ml/datasets)
