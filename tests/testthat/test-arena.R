# Tests for arenas: the presets, arena_transform(), geom_arena() and drawing
# them under plot_trajectory().

all_arenas <- function() {
  list(
    rect = arena_rect(60, 45, centre_zone = 0.5),
    circle = arena_circle(75, platform = c(25, 25, 6), quadrants = TRUE),
    ymaze = arena_ymaze(40, 8),
    tmaze = arena_tmaze(50, 40, 10),
    plusmaze = arena_plusmaze(35, 5),
    radial = arena_radial(8, 60, 10),
    radial_hub = arena_radial(8, 60, 10, hub_radius = 20),
    football = arena_football(),
    track = arena_track(hurdles = "400m"),
    shot_put = arena_throw("shot_put"),
    discus = arena_throw("discus"),
    hammer = arena_throw("hammer"),
    javelin = arena_throw("javelin"),
    long_jump = arena_jump("long_jump"),
    triple_jump = arena_jump("triple_jump"),
    high_jump = arena_jump("high_jump"),
    pole_vault = arena_jump("pole_vault")
  )
}

paths_of <- function(arena, kind) {
  s <- arena$shapes[arena$shapes$kind == kind, ]
  split(s, s$path)
}

is_closed <- function(p) {
  isTRUE(all.equal(p$x[1], p$x[nrow(p)])) &&
    isTRUE(all.equal(p$y[1], p$y[nrow(p)]))
}

test_that("every preset is an arena with finite shapes of known kinds", {
  for (a in all_arenas()) {
    expect_s3_class(a, "anivis_arena")
    expect_true(all(a$shapes$kind %in% arena_kinds()))
    expect_true(all(is.finite(a$shapes$x)))
    expect_true(all(is.finite(a$shapes$y)))
  }
})

test_that("every wall is a closed outline", {
  for (a in all_arenas()) {
    for (p in paths_of(a, "wall")) {
      expect_true(is_closed(p))
    }
  }
})

test_that("football rectangles are drawn with all four sides", {
  # Regression: rectangles drawn as paths lost their closing side, which
  # dropped the left goal line and the inner edges of the right penalty and
  # goal areas.
  pitch <- arena_football()
  lines <- paths_of(pitch, "line")
  for (nm in c(
    "boundary/1",
    "penalty_area_left/1",
    "penalty_area_right/1",
    "goal_area_left/1",
    "goal_area_right/1",
    "goal_left/1",
    "goal_right/1",
    "centre_circle/1"
  )) {
    expect_true(is_closed(lines[[nm]]), info = nm)
  }
  boundary <- lines[["boundary/1"]]
  expect_equal(range(boundary$x), c(-52.5, 52.5))
  expect_equal(range(boundary$y), c(-34, 34))
  expect_equal(range(lines[["penalty_area_right/1"]]$x), c(52.5 - 16.5, 52.5))
  expect_equal(range(lines[["goal_area_left/1"]]$x), c(-52.5, -52.5 + 5.5))
})

test_that("football markings follow the Laws of the Game", {
  pitch <- arena_football(length = 100, width = 64)
  s <- pitch$shapes
  spots <- s[s$kind == "spot", ]
  expect_setequal(spots$x, c(0, -50 + 11, 50 - 11))
  circle <- s[s$element == "centre_circle", ]
  expect_equal(max(sqrt(circle$x^2 + circle$y^2)), 9.15)
  # The penalty arc ends on the edge of the penalty area
  arc <- s[s$element == "penalty_arc_right", ]
  expect_equal(max(arc$x), 50 - 16.5, tolerance = 1e-8)
  # Corner arcs stay inside the field
  corners <- s[startsWith(s$element, "corner_arc"), ]
  expect_true(all(abs(corners$x) <= 50 + 1e-8))
  expect_true(all(abs(corners$y) <= 32 + 1e-8))
  expect_error(arena_football(60, 70), "longer")
})

test_that("maze arms keep their width up to the centre", {
  ymaze <- arena_ymaze(40, 8, labels = NULL)
  wall <- ymaze$shapes
  r <- sqrt(wall$x^2 + wall$y^2)
  # Inner corners sit half an arm width from both neighbouring arm axes
  inner <- r[r < 10]
  expect_length(unique(round(inner, 8)), 1)
  expect_equal(inner[1] * sin(pi / 3), 4)
  # Arm ends are at the arm length, half a width either side of the axis
  expect_equal(max(wall$y), 40)

  hub <- arena_radial(8, 60, 10, hub_radius = 20)
  hub_r <- sqrt(hub$shapes$x^2 + hub$shapes$y^2)
  # Arms leave the hub's edges; its corners lie between them
  expect_equal(min(hub_r), sqrt((20 * cos(pi / 8))^2 + 5^2), tolerance = 1e-8)
  expect_equal(max(hub_r[hub_r < 30]), 20, tolerance = 1e-8)
  expect_error(arena_radial(8, 60, 30, hub_radius = 20), "too wide")
})

test_that("plus maze closed arms are zones, named by their labels", {
  pm <- arena_plusmaze(35, 5, closed = c("W", "E"))
  zones <- unique(pm$shapes$element[pm$shapes$kind == "zone"])
  expect_setequal(zones, c("closed_W", "closed_E"))
  expect_error(arena_plusmaze(35, 5, closed = "X"), "not an arm")
})

test_that("presets reject impossible dimensions", {
  expect_error(arena_rect(-1, 10), "positive")
  expect_error(arena_ymaze("40", 8), "positive")
  expect_error(arena_rect(10, 10, centre_zone = 2), "fraction")
  expect_error(arena_circle(10, platform = c(1, 2)), "platform")
  expect_error(arena_radial(2, 10, 2), "at least 3")
})

test_that("the track measures 400m in lane 1, with staggered starts", {
  t <- track_geometry(8)
  expect_equal(track_lane(t, 1)$length, 400, tolerance = 1e-4)
  track <- arena_track()
  s <- track$shapes
  start_400 <- s[s$element == "start_400m", ]
  # Lane 1 starts on the finish line; lane 8 is on the bend, ahead of it
  lane_1 <- start_400[start_400$path == "start_400m/1", ]
  # Lane 1 measures 400.001m, so its start sits 1mm onto the bend
  expect_true(all(abs(lane_1$x - 84.39 / 2) < 0.002))
  lane_8 <- start_400[start_400$path == "start_400m/8", ]
  stagger <- 2 * pi * (36.5 + 7 * 1.22 + 0.20 - (36.5 + 0.30))
  angle <- atan2(lane_8$y[1], lane_8$x[1] - 84.39 / 2)
  expect_equal(
    angle,
    -pi / 2 + stagger / (36.5 + 7 * 1.22 + 0.20),
    tolerance = 1e-4
  )
  # The 200m in lane 1 starts where the back straight meets the near bend
  start_200 <- s[s$path == "start_200m/1", ]
  expect_true(all(abs(start_200$x + 84.39 / 2) < 0.002))
  expect_true(all(start_200$y > 0))
  expect_equal(range(s$x[s$element == "start_100m"]), rep(84.39 / 2 - 100, 2))
})

test_that("hurdles follow Rule 22", {
  h110 <- arena_track(lanes = 2, hurdles = "110m")$shapes
  marks <- h110[h110$element == "hurdles" & h110$y == -36.5, ]
  expect_equal(sort(unique(marks$x)), 84.39 / 2 - 110 + 13.72 + 9.14 * 0:9)
  h400 <- arena_track(lanes = 8, hurdles = "400m")$shapes
  expect_length(unique(h400$path[h400$element == "hurdles"]), 80)
  expect_error(arena_track(hurdles = "200m"))
})

test_that("throwing circles and sectors follow the rules", {
  sector_angle <- function(arena) {
    lines <- arena$shapes[arena$shapes$element == "sector_lines", ]
    ends <- lines[!duplicated(lines$path, fromLast = TRUE), ]
    diff(sort(atan2(ends$y, ends$x))) * 180 / pi
  }
  rim_diameter <- function(arena) {
    rim <- arena$shapes[arena$shapes$element == "rim", ]
    2 * max(sqrt(rim$x^2 + rim$y^2))
  }
  expect_equal(rim_diameter(arena_throw("shot_put")), 2.135)
  expect_equal(rim_diameter(arena_throw("hammer")), 2.135)
  expect_equal(rim_diameter(arena_throw("discus")), 2.5)
  expect_equal(sector_angle(arena_throw("discus")), 34.92, tolerance = 1e-6)
  # The javelin sector lines run from the arc's ends through its centre
  expect_equal(sector_angle(arena_throw("javelin")), 28.96, tolerance = 1e-3)
  board <- arena_throw("shot_put")$shapes
  board <- board[board$element == "stop_board", ]
  expect_equal(
    diff(range(board$y[abs(sqrt(board$x^2 + board$y^2) - 2.135 / 2) < 1e-9])),
    1.21
  )
  long <- arena_throw("shot_put", sector_length = 30)$shapes
  expect_equal(
    max(long$x[long$element == "sector_lines"]),
    30 * cos(34.92 / 2 * pi / 180)
  )
  # The venue sits on a grass apron reaching past the sector
  expect_equal(long$material[1], "grass")
  expect_gt(max(long$x), 30)
})

# The sector lines as rays: where each starts, and its direction in degrees.
sector_rays <- function(arena) {
  lines <- arena$shapes[arena$shapes$element == "sector_lines", ]
  lapply(split(lines, lines$path), function(p) {
    list(
      start = c(p$x[1], p$y[1]),
      angle = atan2(diff(p$y), diff(p$x)) * 180 / pi,
      # Distance from the origin to the line through the ray
      offset = abs(p$x[1] * p$y[2] - p$y[1] * p$x[2]) /
        sqrt(diff(p$x)^2 + diff(p$y)^2)
    )
  })
}

test_that("circle throw sectors follow Rule 32", {
  for (event in c("shot_put", "discus", "hammer")) {
    rays <- sector_rays(arena_throw(event))
    angles <- vapply(rays, function(r) r$angle, numeric(1), USE.NAMES = FALSE)
    # 34.92 degrees, symmetric about the throwing direction
    expect_equal(diff(sort(angles)), 34.92, tolerance = 1e-10, info = event)
    expect_equal(sum(angles), 0, tolerance = 1e-10, info = event)
    # The lines, extended, pass through the centre of the circle
    for (r in rays) {
      expect_lt(r$offset, 1e-9)
    }
    # The rule's own check: 12m +/- 0.05m apart at 20m from the centre
    chord <- 2 * 20 * sin(max(angles) * pi / 180)
    expect_lt(abs(chord - 12), 0.05)
  }

  # The 0.75m lines either side lie on the line through the centre at right
  # angles to the sector
  shapes <- arena_throw("shot_put")$shapes
  rim_lines <- shapes[shapes$element == "rim_lines", ]
  expect_equal(unique(rim_lines$x), 0)
  lengths <- tapply(rim_lines$y, rim_lines$path, function(y) diff(range(y)))
  expect_equal(as.vector(lengths), c(0.75, 0.75))
})

test_that("the javelin sector follows Rule 32", {
  rays <- sector_rays(arena_throw("javelin"))
  angles <- vapply(rays, function(r) r$angle, numeric(1), USE.NAMES = FALSE)
  # Built from the arc and runway lines, so exact; the rules round it
  expect_equal(
    diff(sort(angles)),
    2 * asin(2 / 8) * 180 / pi,
    tolerance = 1e-10
  )
  expect_equal(round(diff(sort(angles)), 2), 28.96)
  for (r in rays) {
    # Each line starts where the 8m arc meets a runway line, 4m apart...
    expect_equal(sqrt(sum(r$start^2)), 8, tolerance = 1e-10)
    expect_equal(abs(r$start[2]), 2, tolerance = 1e-10)
    # ...and passes through the centre of the arc
    expect_lt(r$offset, 1e-9)
  }
})

test_that("jumps follow the rules", {
  lj <- arena_jump("long_jump")$shapes
  pit <- lj[lj$element == "pit", ]
  expect_equal(diff(range(pit$y)), 2.75)
  expect_equal(range(pit$x), c(2, 11))
  board <- lj[lj$element == "take_off_board", ]
  expect_equal(range(board$x), c(-0.2, 0))
  expect_equal(diff(range(board$y)), 1.22)
  tj <- arena_jump("triple_jump", take_off = 11)$shapes
  expect_equal(min(tj$x[tj$element == "pit"]), 11)
  hj <- arena_jump("high_jump")$shapes
  expect_equal(diff(range(hj$y[hj$element == "uprights"])), 4.04)
  pv <- arena_jump("pole_vault")$shapes
  box <- pv[pv$element == "box", ]
  expect_equal(range(box$x), c(-1, 0))
  expect_equal(diff(range(box$y[box$x == -1])), 0.6)
  expect_equal(diff(range(box$y[box$x == 0])), 0.15)
})

test_that("surfaces are coloured by material, in order", {
  track <- arena_track()
  layers <- geom_arena(track)
  fills <- unlist(lapply(layers, function(l) l$aes_params$fill))
  # The infield grass is drawn after the track it sits inside
  expect_lt(match("#C99C8C", fills), match("#AFC0A2", fills))
  bad <- data.frame(
    element = "s",
    kind = "surface",
    x = 0:2,
    y = c(0, 1, 0),
    material = "lava"
  )
  expect_error(as_arena(bad, "m"), "lava")
})

test_that("axes can be left out", {
  p <- plot(arena_ymaze(40, 8), axes = FALSE)
  expect_s3_class(p$theme$axis.text, "element_blank")
  af <- data.frame(time = 1:3, x = 1:3, y = 1:3) |>
    anicore::as_anipoint(metadata = list(unit_space = "cm"))
  expect_s3_class(
    plot_trajectory(af, axes = FALSE)$theme$axis.title,
    "element_blank"
  )
  expect_false(inherits(plot_trajectory(af)$theme$axis.title, "element_blank"))
})

test_that("as_arena() checks its shapes", {
  ok <- data.frame(
    element = "wall",
    kind = "wall",
    x = c(0, 1, 1),
    y = c(0, 0, 1)
  )
  a <- as_arena(ok, unit = "cm")
  expect_s3_class(a, "anivis_arena")
  expect_equal(unique(a$shapes$path), "wall/1")
  expect_error(as_arena(ok[, -1], "cm"), "element")
  bad <- ok
  bad$kind <- "fence"
  expect_error(as_arena(bad, "cm"), "fence")
  expect_error(as_arena(ok, c("cm", "m")), "single string")
  expect_output(print(a), "in cm, lab style")
})

test_that("arena_transform() converts units, scales, rotates, flips and shifts", {
  a <- as_arena(
    data.frame(element = "p", kind = "spot", x = 1, y = 0),
    unit = "cm"
  )

  mm <- arena_transform(a, unit = "mm")
  expect_equal(mm$unit, "mm")
  expect_equal(mm$shapes$x, 10)

  yd <- arena_transform(as_arena(a$shapes, "yd"), unit = "m")
  expect_equal(yd$shapes$x, 0.9144)

  turned <- arena_transform(a, rotate = 90)
  expect_equal(c(turned$shapes$x, turned$shapes$y), c(0, 1), tolerance = 1e-12)

  flipped <- arena_transform(a, rotate = 90, flip_y = TRUE, origin = c(5, 5))
  expect_equal(
    c(flipped$shapes$x, flipped$shapes$y),
    c(5, 4),
    tolerance = 1e-12
  )

  px <- arena_transform(a, unit = "px", scale = c(12, 3))
  expect_equal(px$unit, "px")
  expect_equal(px$shapes$x, 12)

  expect_error(
    arena_transform(as_arena(a$shapes, "furlong"), unit = "m"),
    "Cannot convert"
  )
  expect_error(arena_transform(list()), "must be an arena")
})

test_that("geom_arena() layers ignore the plot's aesthetics", {
  layers <- geom_arena(arena_ymaze(40, 8))
  expect_true(all(vapply(
    layers,
    function(l) isFALSE(l$inherit.aes),
    logical(1)
  )))

  # Regression for the approach this replaced: surface layers that inherit a
  # global mapping fail on columns they do not have.
  df <- data.frame(id = rep(c("a", "b"), each = 3), x = 1:6, y = 1:6)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, linetype = id)) +
    geom_arena(arena_football()) +
    ggplot2::geom_path()
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("geom_arena() colours follow the surface, mode and palette", {
  pitch <- arena_football()
  fills <- function(layers) {
    unlist(lapply(layers, function(l) l$aes_params$fill))
  }
  expect_true("#AFC0A2" %in% fills(geom_arena(pitch)))
  expect_true("#27352C" %in% fills(geom_arena(pitch, mode = "dark")))
  # "plain" draws no surface or stripes
  plain <- geom_arena(pitch, palette = "plain")
  expect_false(any(c("#AFC0A2", "#FFFFFF") %in% fills(plain)))
  expect_true(
    "#123456" %in% fills(geom_arena(pitch, palette = list(grass = "#123456")))
  )
  expect_error(geom_arena(pitch, palette = "neon"), "must be one of")
  expect_error(geom_arena(pitch, palette = list(turf = "red")), "unknown")
  expect_length(
    Filter(
      function(l) inherits(l$geom, "GeomText"),
      geom_arena(arena_ymaze(40, 8), labels = FALSE)
    ),
    0
  )
})

test_that("plot() draws every arena on its own", {
  for (a in all_arenas()) {
    for (mode in c("light", "dark")) {
      p <- plot(a, mode = mode)
      expect_s3_class(p, "ggplot")
      expect_no_error(ggplot2::ggplot_build(p))
    }
  }
})

test_that("plot_trajectory() draws the arena under the paths, in the data's unit", {
  af <- data.frame(
    time = 1:5,
    x = c(0, 0, 0, 200, 0),
    y = c(0, 300, 0, -100, 0)
  ) |>
    anicore::as_anipoint(metadata = list(unit_space = "mm"))
  p <- plot_trajectory(af, arena = arena_ymaze(40, 8))
  expect_no_error(ggplot2::ggplot_build(p))

  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  first_path <- which(
    geoms == "GeomPath" &
      !vapply(
        p$layers,
        function(l) isFALSE(l$inherit.aes),
        logical(1)
      )
  )[1]
  arena_rows <- which(vapply(
    p$layers,
    function(l) isFALSE(l$inherit.aes) && "path" %in% names(l$data),
    logical(1)
  ))
  expect_true(max(arena_rows) < first_path)

  # The cm maze was converted to mm
  wall <- p$layers[[arena_rows[1]]]$data
  expect_equal(max(wall$y), 400)
  expect_s3_class(p$theme$panel.grid.major, "element_blank")
})

test_that("plot_trajectory() refuses to match an arena to uncalibrated data", {
  af <- data.frame(time = 1:3, x = 1:3, y = 1:3) |>
    anicore::as_anipoint(metadata = list(unit_space = "px"))
  expect_error(plot_trajectory(af, arena = arena_ymaze(40, 8)), "Calibrate")
  placed <- arena_transform(arena_ymaze(40, 8), unit = "px", scale = 10)
  expect_no_error(ggplot2::ggplot_build(plot_trajectory(af, arena = placed)))
  expect_error(plot_trajectory(af, arena = "ymaze"), "must be an arena")
})
