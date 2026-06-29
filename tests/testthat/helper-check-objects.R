# Build genuine `check_*` objects for the plot.check_* / as_plot_data tests.
#
# The `check_*()` compute functions are destined for the anicheck package and
# are not a dependency of anivis, so they are vendored here (verbatim from
# anicheck_files/) to produce real objects through the real computation path —
# far less fragile than hand-assembling the data frame + attributes a method
# reads. Everything is wrapped in a single local() so the internal helper names
# (group_key, aniframe_group_cols, ...) cannot collide with anivis internals;
# only the three make_check_*() builders are exported into the test environment.

.anivis_check_builders <- local({
  # --- shared helpers (check_helpers.R) --------------------------------------

  group_key <- function(tbl, group_cols) {
    n <- nrow(tbl)
    if (!n) {
      return(character(0))
    }
    if (!length(group_cols)) {
      return(rep("all", n))
    }
    do.call(
      paste,
      c(lapply(group_cols, function(col) as.character(tbl[[col]])), sep = "\t")
    )
  }

  split_by_group_cols <- function(df, group_cols) {
    key <- group_key(df, group_cols)
    split(df, factor(key, levels = unique(key)))
  }

  aniframe_group_cols <- function(data) {
    meta <- aniframe::get_metadata(data)
    what <- intersect(meta$variables_what, names(data))
    when <- setdiff(intersect(meta$variables_when, names(data)), "time")
    unique(c(what, when))
  }

  check_na_variable <- function(data, variable) {
    if (is.null(variable) || !length(variable)) {
      cli::cli_abort("{.arg variable} must name at least one column.")
    }
    if (!is.character(variable)) {
      cli::cli_abort(
        "{.arg variable} must be a character vector of column names."
      )
    }
    unknown <- setdiff(variable, names(data))
    if (length(unknown)) {
      cli::cli_abort(
        "{.arg variable} names unknown column{?s}: {.val {unknown}}."
      )
    }
    variable
  }

  group_totals <- function(df, group_cols) {
    parts <- split_by_group_cols(df, group_cols)
    out <- do.call(
      rbind,
      lapply(parts, function(d) {
        row <- data.frame(
          n_frames = nrow(d),
          n_missing = sum(d$.missing),
          time_min = min(d$time),
          time_max = max(d$time)
        )
        for (col in group_cols) {
          row[[col]] <- d[[col]][1]
        }
        row[c(group_cols, "n_frames", "n_missing", "time_min", "time_max")]
      })
    )
    rownames(out) <- NULL
    out
  }

  distribution_summary <- function(df, group_cols, value_col) {
    parts <- split_by_group_cols(df, group_cols)
    out <- do.call(
      rbind,
      lapply(parts, function(d) {
        v <- d[[value_col]][!is.na(d[[value_col]])]
        q <- if (length(v)) {
          stats::quantile(v, c(0, 0.25, 0.5, 0.75, 1), names = FALSE)
        } else {
          rep(NA_real_, 5)
        }
        row <- data.frame(
          n = length(v),
          mean = if (length(v)) mean(v) else NA_real_,
          sd = if (length(v) > 1L) stats::sd(v) else NA_real_,
          min = q[1],
          q25 = q[2],
          median = q[3],
          q75 = q[4],
          max = q[5]
        )
        for (col in group_cols) {
          row[[col]] <- d[[col]][1]
        }
        row[c(
          group_cols,
          "n",
          "mean",
          "sd",
          "min",
          "q25",
          "median",
          "q75",
          "max"
        )]
      })
    )
    rownames(out) <- NULL
    out
  }

  # --- check_na_timing -------------------------------------------------------

  na_timing_runs <- function(d, group_cols) {
    d <- d[order(d$time), , drop = FALSE]
    runs <- rle(d$.missing)
    ends <- cumsum(runs$lengths)
    starts <- ends - runs$lengths + 1L
    keep <- which(runs$values)
    if (!length(keep)) {
      return(NULL)
    }
    out <- data.frame(
      start = d$time[starts[keep]],
      stop = d$time[ends[keep]],
      length = runs$lengths[keep]
    )
    for (col in group_cols) {
      out[[col]] <- d[[col]][1]
    }
    out[c(group_cols, "start", "stop", "length")]
  }

  na_timing_totals <- function(d, group_cols) {
    out <- data.frame(
      n_frames = nrow(d),
      n_missing = sum(d$.missing),
      time_min = min(d$time),
      time_max = max(d$time)
    )
    for (col in group_cols) {
      out[[col]] <- d[[col]][1]
    }
    out[c(group_cols, "n_frames", "n_missing", "time_min", "time_max")]
  }

  na_timing_step <- function(parts) {
    steps <- unlist(
      lapply(parts, function(d) diff(sort(d$time))),
      use.names = FALSE
    )
    if (!length(steps)) {
      return(1)
    }
    step <- stats::median(steps)
    if (is.na(step) || step <= 0) 1 else step
  }

  empty_segments <- function(group_cols) {
    cols <- c(
      stats::setNames(rep(list(character(0)), length(group_cols)), group_cols),
      list(start = numeric(0), stop = numeric(0), length = integer(0))
    )
    do.call(data.frame, c(cols, stringsAsFactors = FALSE))
  }

  new_check_na_timing <- function(
    x,
    variable,
    unit_time,
    group_cols,
    groups,
    time_step,
    time_range
  ) {
    class(x) <- c(
      "check_na_timing",
      "anivis_check_na_timing",
      "tbl_df",
      "tbl",
      "data.frame"
    )
    attr(x, "variable") <- variable
    attr(x, "unit_time") <- unit_time
    attr(x, "group_cols") <- group_cols
    attr(x, "groups") <- groups
    attr(x, "time_step") <- time_step
    attr(x, "time_range") <- time_range
    x
  }

  build_na_timing <- function(data, variable = "x") {
    variable <- check_na_variable(data, variable)
    meta <- aniframe::get_metadata(data)
    group_cols <- aniframe_group_cols(data)

    df <- as.data.frame(data)
    df$.missing <- Reduce(`|`, lapply(variable, function(v) is.na(df[[v]])))

    parts <- split_by_group_cols(df, group_cols)
    segments <- do.call(
      rbind,
      lapply(parts, na_timing_runs, group_cols = group_cols)
    )
    if (is.null(segments)) {
      segments <- empty_segments(group_cols)
    }
    rownames(segments) <- NULL

    groups <- do.call(
      rbind,
      lapply(parts, na_timing_totals, group_cols = group_cols)
    )
    rownames(groups) <- NULL

    new_check_na_timing(
      segments,
      variable = variable,
      unit_time = if (!is.null(meta$unit_time)) {
        as.character(meta$unit_time)
      } else {
        NA_character_
      },
      group_cols = group_cols,
      groups = groups,
      time_step = na_timing_step(parts),
      time_range = c(min(df$time), max(df$time))
    )
  }

  # --- check_na_gapsize ------------------------------------------------------

  na_gapsize_tabulate <- function(d, group_cols) {
    runs <- rle(d$.missing[order(d$time)])
    lengths <- runs$lengths[runs$values]
    if (!length(lengths)) {
      return(NULL)
    }
    tab <- table(lengths)
    out <- data.frame(
      gap_size = as.integer(names(tab)),
      n_gaps = as.integer(tab)
    )
    out$n_na <- out$gap_size * out$n_gaps
    for (col in group_cols) {
      out[[col]] <- d[[col]][1]
    }
    out[c(group_cols, "gap_size", "n_gaps", "n_na")]
  }

  empty_gapsize <- function(group_cols) {
    cols <- c(
      stats::setNames(rep(list(character(0)), length(group_cols)), group_cols),
      list(gap_size = integer(0), n_gaps = integer(0), n_na = integer(0))
    )
    do.call(data.frame, c(cols, stringsAsFactors = FALSE))
  }

  new_check_na_gapsize <- function(x, variable, group_cols, groups) {
    class(x) <- c(
      "check_na_gapsize",
      "anivis_check_na_gapsize",
      "tbl_df",
      "tbl",
      "data.frame"
    )
    attr(x, "variable") <- variable
    attr(x, "group_cols") <- group_cols
    attr(x, "groups") <- groups
    x
  }

  build_na_gapsize <- function(data, variable = "x") {
    variable <- check_na_variable(data, variable)
    group_cols <- aniframe_group_cols(data)

    df <- as.data.frame(data)
    df$.missing <- Reduce(`|`, lapply(variable, function(v) is.na(df[[v]])))

    parts <- split_by_group_cols(df, group_cols)
    out <- do.call(
      rbind,
      lapply(parts, na_gapsize_tabulate, group_cols = group_cols)
    )
    if (is.null(out)) {
      out <- empty_gapsize(group_cols)
    }
    rownames(out) <- NULL

    new_check_na_gapsize(
      out,
      variable = variable,
      group_cols = group_cols,
      groups = group_totals(df, group_cols)
    )
  }

  # --- check_confidence ------------------------------------------------------

  confidence_density <- function(d, group_cols, n) {
    v <- d$confidence[!is.na(d$confidence)]
    if (length(v) >= 2L && diff(range(v)) > 0) {
      dens <- stats::density(v, from = min(v), to = max(v), n = n)
      out <- data.frame(value = dens$x, density = dens$y)
    } else {
      val <- if (length(v)) v[1] else NA_real_
      out <- data.frame(value = c(val, val), density = c(0, 1))
    }
    for (col in group_cols) {
      out[[col]] <- d[[col]][1]
    }
    out[c(group_cols, "value", "density")]
  }

  new_check_confidence <- function(x, group_cols, groups) {
    class(x) <- c(
      "check_confidence",
      "anivis_check_confidence",
      "tbl_df",
      "tbl",
      "data.frame"
    )
    attr(x, "group_cols") <- group_cols
    attr(x, "groups") <- groups
    x
  }

  build_confidence <- function(data, n = 256) {
    if (!("confidence" %in% names(data))) {
      cli::cli_abort(
        "{.fun check_confidence} needs a {.field confidence} column."
      )
    }
    group_cols <- aniframe_group_cols(data)

    df <- as.data.frame(data)
    parts <- split_by_group_cols(df, group_cols)
    grid <- do.call(
      rbind,
      lapply(parts, confidence_density, group_cols = group_cols, n = n)
    )
    rownames(grid) <- NULL

    new_check_confidence(
      grid,
      group_cols = group_cols,
      groups = distribution_summary(df, group_cols, "confidence")
    )
  }

  list(
    na_timing = build_na_timing,
    na_gapsize = build_na_gapsize,
    confidence = build_confidence
  )
})

make_check_na_timing <- .anivis_check_builders$na_timing
make_check_na_gapsize <- .anivis_check_builders$na_gapsize
make_check_confidence <- .anivis_check_builders$confidence
