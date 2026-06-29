# Plot an anievent Object

Creates a visualization of state and point events from an
[`aniframe::anievent()`](http://animovement.dev/aniframe/reference/anievent.md)
using
[`geom_event_state()`](http://animovement.dev/anivis/reference/geom_event_state.md)
and
[`geom_event_point()`](http://animovement.dev/anivis/reference/geom_event_point.md).
Returns a patchwork object that can be combined with additional plots.
The drawing itself is done by
[`plot_events()`](http://animovement.dev/anivis/reference/plot_events.md).

## Usage

``` r
# S3 method for class 'anievent'
plot(x, ..., mode = c("light", "dark"))
```

## Arguments

- x:

  An anievent object.

- ...:

  Additional arguments passed to underlying plot functions.

- mode:

  Either `"light"` (default) or `"dark"`; passed to
  [`theme_animovement()`](http://animovement.dev/anivis/reference/theme_animovement.md).

## Value

A patchwork object.

## See also

[`plot_events()`](http://animovement.dev/anivis/reference/plot_events.md)
