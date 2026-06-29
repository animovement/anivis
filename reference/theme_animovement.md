# Animovement ggplot2 Theme

The default animovement theme, with light and dark variants. It builds
on
[`ggplot2::theme_linedraw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
— fine black panel border, thin gridlines — but swaps linedraw's black
facet strips for the usual light-grey strips with dark text, and adds
the plain enlarged title, generous axis-title margins and title
positioning of `see::theme_modern()`. Further themes with their own
names may be added over time.

## Usage

``` r
theme_animovement(mode = c("light", "dark"), base_size = 11, base_family = "")

theme_animovement_light(base_size = 11, base_family = "")

theme_animovement_dark(base_size = 11, base_family = "")
```

## Arguments

- mode:

  Either "light" (default) or "dark".

- base_size:

  Base font size.

- base_family:

  Base font family.

## Value

A ggplot2 theme object.
