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
```

### [Course Outline](https://docs.google.com/spreadsheets/d/1h7zq3rfj3JwyiW6lUcTt4Uid7i0BFrt1/edit?usp=sharing&ouid=113368369928842739072&rtpof=true&sd=true)

Week 1: Introduction and Retrieval-augmented Generation

Week 2: Regression and Classification (with gradient descent)

Week 3: Generalizability

Week 4: Ridge and LASSO Regularization

Week 5: (Artificial) Neural Networks

Week 6: Trees and Forests

Week 7: $k$-means and Hierarchical Clustering

Week 8: Psychometric Networks and Exploratory Graph Analysis

Week 9: Robustness of Exploratory Graph Analysis

Week 10-11: Dynamic Psychometric Networks

Week 12-13: Natural Language Processing with Transformers

Week 14: Final Project
