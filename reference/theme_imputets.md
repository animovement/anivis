# imputeTS-style Theme for Check Plots

A larger-font variant of
[`theme_animovement()`](https://animovement.dev/anivis/reference/theme_animovement.md)
tuned for the diagnostic check plots
([`plot.anivis_check_na_timing()`](https://animovement.dev/anivis/reference/plot.anivis_check_na_timing.md),
[`plot.anivis_check_na_gapsize()`](https://animovement.dev/anivis/reference/plot.anivis_check_na_gapsize.md),
[`plot.anivis_check_confidence()`](https://animovement.dev/anivis/reference/plot.anivis_check_confidence.md)),
styled after the imputeTS package figures they are adapted from.
Compared with
[`theme_animovement()`](https://animovement.dev/anivis/reference/theme_animovement.md)
it uses a larger base size, places the title directly above the panel
(not over the axis), renders the subtitle as markdown via marquee — so
words like NA / non-NA can be coloured inline with the `.na` / `.nona`
classes — and puts the legend at the bottom.

## Usage

``` r
theme_imputets(mode = c("light", "dark"), base_size = 13)
```

## Arguments

- mode:

  Either `"light"` (default) or `"dark"`.

- base_size:

  Base font size. Default `13`.

## Value

A ggplot2 theme object.

## See also

[`theme_animovement()`](https://animovement.dev/anivis/reference/theme_animovement.md)

## Examples

``` r
library(ggplot2)
ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  theme_imputets()

```
