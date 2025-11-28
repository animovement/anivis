# Visualize the distribution of confidence values for each keypoint

This function generates histograms showing the distribution of
confidence values for each keypoint in the dataset.

## Usage

``` r
# S3 method for class 'plot_check_confidence'
plot(x, ...)
```

## Arguments

- x:

  A data frame containing at least the columns `keypoint` and
  `confidence`.

- ...:

  Additional arguments for ggplot2

## Value

A ggplot boxplot of the confidence per keypoint.
