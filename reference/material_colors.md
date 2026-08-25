# Material design colours

Extract one or more colours from the Material design palette. Vendored
from the see package.

## Usage

``` r
material_colors(...)
```

## Arguments

- ...:

  Character names of colours to extract. If none are given, the full
  palette is returned.

## Value

A named character vector of hex colours.

## Examples

``` r
material_colors()
#>         red        pink      purple deep purple      indigo        blue 
#>   "#f44336"   "#E91E63"   "#9C27B0"   "#673AB7"   "#3F51B5"   "#2196F3" 
#>  light blue        cyan        teal       green light green        lime 
#>   "#03A9F4"   "#00BCD4"   "#009688"   "#4CAF50"   "#8BC34A"   "#CDDC39" 
#>      yellow       amber      orange deep orange       brown        grey 
#>   "#FFEB3B"   "#FFC107"   "#FF9800"   "#FF5722"   "#795548"   "#9E9E9E" 
#>   blue grey 
#>   "#607D8B" 
material_colors("red", "blue")
#>       red      blue 
#> "#f44336" "#2196F3" 
```
