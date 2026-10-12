#' Athletics Arenas
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Athletics venues drawn to the World Athletics Technical Rules (Book C2.1),
#' in metres, to draw under tracking data with [plot_trajectory()] or
#' [geom_arena()]. They are coloured in the `"sport"` palette of
#' [geom_arena()].
#'
#' * `arena_track()`: the standard 400m track, two bends of 36.50m radius
#'   joined by straights of 84.39m, centred on the middle of the infield, with
#'   athletes running counter-clockwise and the finish at the end of the home
#'   straight (along the bottom). Lanes are 1.22m wide. The home straight runs
#'   on past both bends for the 100m and 110m starts and the run-out. Start
#'   lines and hurdles are placed along each lane's measurement line, 0.30m
#'   from the kerb in lane 1 and 0.20m from the inner line in the others, so
#'   lane 1 measures 400.00m and the staggers follow.
#' * `arena_throw()`: a throwing circle or the javelin runway, with its landing
#'   sector, thrown along x from the origin. The circle is 2.135m across for
#'   the shot put and hammer and 2.50m for the discus; the sector is 34.92
#'   degrees. The javelin is thrown from behind an arc of 8m radius at the end
#'   of a 4m wide runway, into a sector of 28.96 degrees, and the origin is the
#'   centre of the arc.
#' * `arena_jump()`: a jumping venue, run along x. For the long and triple jump
#'   the take-off line is at the origin, with the board behind it and the sand
#'   pit ahead. For the high jump the origin is midway between the uprights,
#'   with a fan-shaped run-up behind and the landing area ahead. For the pole
#'   vault the origin is the zero line at the back of the box.
#'
#' Rules fix the markings, not every size: the length of the landing sectors
#' and the run-ups are drawn long enough for elite performances and can be
#' changed.
#'
#' @param lanes Number of lanes (default `8`).
#' @param starts Start lines to draw (default all): any of `"100m"`, `"110m"`
#'   (on the home straight), `"200m"` and `"400m"` (staggered).
#' @param hurdles Hurdle positions to mark (default `NULL`, none): one of
#'   `"100m"`, `"110m"` or `"400m"`.
#' @param event The event. For `arena_throw()`: `"shot_put"`, `"discus"`,
#'   `"hammer"` or `"javelin"`. For `arena_jump()`: `"long_jump"`,
#'   `"triple_jump"`, `"high_jump"` or `"pole_vault"`.
#' @param sector_length Length of the landing sector from the centre of the
#'   circle or arc (default `NULL`: 25m for the shot put, 80m for the discus,
#'   90m for the hammer and 100m for the javelin).
#' @param runway_length Length of the run-up behind the take-off (default
#'   `NULL`: 36.5m for the javelin, 40m for the horizontal jumps and the pole
#'   vault, and a fan of 15m radius for the high jump).
#' @param take_off Distance from the take-off line to the near end of the pit
#'   in the long and triple jump (default `NULL`: 2m for the long jump, within
#'   the 1m to 3m the rules allow, and 13m for the triple jump, the men's
#'   minimum; 11m is the women's).
#'
#' @return An arena; see [as_arena()].
#'
#' @family arenas
#' @examples
#' plot(arena_track())
#' plot(arena_track(hurdles = "400m"), mode = "dark", axes = FALSE)
#' plot(arena_throw("discus", sector_length = 30))
#' plot(arena_throw("javelin", sector_length = 30))
#' plot(arena_jump("triple_jump"))
#' plot(arena_jump("high_jump"))
#' @name athletics_arenas
NULL

#' @rdname athletics_arenas
#' @export
arena_track <- function(
  lanes = 8,
  starts = c("100m", "110m", "200m", "400m"),
  hurdles = NULL
) {
  check_dimensions(lanes = lanes)
  starts <- rlang::arg_match(
    starts,
    c("100m", "110m", "200m", "400m"),
    multiple = TRUE
  )
  if (!is.null(hurdles)) {
    hurdles <- rlang::arg_match(hurdles, c("100m", "110m", "400m"))
  }
  t <- track_geometry(lanes)
  outer <- t$kerb + lanes * t$lane
  back <- t$straight / 2 - 110 - 5
  run_out <- t$straight / 2 + 17

  parts <- list(
    arena_xy("track", "surface", track_oval_xy(t, outer), material = "track"),
    arena_xy(
      "sprint_straight",
      "surface",
      arena_rect_xy(back, run_out, -outer, -t$kerb),
      material = "track"
    ),
    arena_xy(
      "infield",
      "surface",
      track_oval_xy(t, t$kerb),
      material = "grass"
    ),
    arena_xy("kerb", "line", track_oval_xy(t, t$kerb))
  )
  for (k in seq_len(lanes)) {
    r <- t$kerb + k * t$lane
    parts <- c(
      parts,
      list(arena_xy("lane_line", "line", track_oval_xy(t, r), path = k))
    )
  }
  # The home straight carries on past both bends
  for (k in 0:lanes) {
    y <- -(t$kerb + k * t$lane)
    parts <- c(
      parts,
      list(
        arena_shape(
          "straight_line",
          "line",
          c(back, -t$straight / 2),
          c(y, y),
          path = 2 * k
        ),
        arena_shape(
          "straight_line",
          "line",
          c(t$straight / 2, run_out),
          c(y, y),
          path = 2 * k + 1
        )
      )
    )
  }
  parts <- c(
    parts,
    list(arena_shape(
      "finish_line",
      "line",
      c(t$straight / 2, t$straight / 2),
      c(-t$kerb, -outer)
    ))
  )
  for (race in intersect(starts, c("100m", "110m"))) {
    x <- t$straight / 2 - as.numeric(sub("m", "", race))
    parts <- c(
      parts,
      list(arena_shape(
        paste0("start_", race),
        "line",
        c(x, x),
        c(-t$kerb, -outer)
      ))
    )
  }
  for (race in intersect(starts, c("200m", "400m"))) {
    d <- as.numeric(sub("m", "", race))
    parts <- c(
      parts,
      lapply(seq_len(lanes), function(i) {
        lane <- track_lane(t, i)
        xy <- track_across(t, lane, lane$length - d)
        arena_shape(paste0("start_", race), "line", xy$x, xy$y, path = i)
      })
    )
  }
  if (!is.null(hurdles)) {
    parts <- c(parts, track_hurdles(t, lanes, hurdles))
  }
  as_arena(do.call(rbind, parts), "m", "400m track", style = "sport")
}

#' @rdname athletics_arenas
#' @export
arena_throw <- function(
  event = c("shot_put", "discus", "hammer", "javelin"),
  sector_length = NULL,
  runway_length = NULL
) {
  event <- rlang::arg_match(event)
  if (event == "javelin") {
    return(arena_javelin(sector_length %||% 100, runway_length %||% 36.5))
  }
  radius <- if (event == "discus") 1.25 else 2.135 / 2
  sector_length <- sector_length %||%
    c(shot_put = 25, discus = 80, hammer = 90)[[event]]
  check_dimensions(sector_length = sector_length)
  half <- 34.92 / 2 * pi / 180

  parts <- list(
    arena_xy(
      "sector",
      "surface",
      arena_close(list(
        x = c(
          radius * cos(half),
          sector_length * cos(half),
          sector_length * cos(-half),
          radius * cos(-half)
        ),
        y = c(
          radius * sin(half),
          sector_length * sin(half),
          sector_length * sin(-half),
          radius * sin(-half)
        )
      )),
      material = "grass"
    ),
    arena_xy(
      "circle",
      "surface",
      arena_arc_xy(0, 0, radius),
      material = "concrete"
    ),
    arena_xy("rim", "line", arena_arc_xy(0, 0, radius))
  )
  for (side in c(-1, 1)) {
    parts <- c(
      parts,
      list(
        # 0.75m lines either side, through the centre, square to the sector
        arena_shape(
          "rim_lines",
          "line",
          c(0, 0),
          side * c(radius, radius + 0.75),
          path = (side + 3) / 2
        ),
        arena_shape(
          "sector_lines",
          "line",
          c(radius, sector_length) * cos(side * half),
          c(radius, sector_length) * sin(side * half),
          path = (side + 3) / 2
        )
      )
    )
  }
  if (event == "shot_put") {
    # Stop board: an arc with a chord of 1.21m, 0.112m wide, outside the rim
    a <- asin(0.605 / radius)
    inner <- arena_arc_xy(0, 0, radius, -a, a, n = 20)
    outer <- arena_arc_xy(0, 0, radius + 0.112, a, -a, n = 20)
    parts <- c(
      parts,
      list(arena_xy(
        "stop_board",
        "line",
        arena_close(list(x = c(inner$x, outer$x), y = c(inner$y, outer$y)))
      ))
    )
  }
  name <- c(shot_put = "shot put", discus = "discus", hammer = "hammer")[[
    event
  ]]
  as_arena(with_apron(parts), "m", paste(name, "circle"), style = "sport")
}

#' @rdname athletics_arenas
#' @export
arena_jump <- function(
  event = c("long_jump", "triple_jump", "high_jump", "pole_vault"),
  runway_length = NULL,
  take_off = NULL
) {
  event <- rlang::arg_match(event)
  switch(
    event,
    long_jump = arena_horizontal_jump(
      runway_length %||% 40,
      take_off %||% 2,
      pit_end = 11,
      name = "long jump"
    ),
    triple_jump = arena_horizontal_jump(
      runway_length %||% 40,
      take_off %||% 13,
      pit_end = (take_off %||% 13) + 8,
      name = "triple jump"
    ),
    high_jump = arena_high_jump(runway_length %||% 15),
    pole_vault = arena_pole_vault(runway_length %||% 40)
  )
}

# Internal ---------------------------------------------------------------

# Field events sit in the grass of the infield, which also keeps their white
# lines visible.
with_apron <- function(parts, margin = 2) {
  shapes <- do.call(rbind, parts)
  apron <- arena_xy(
    "infield",
    "surface",
    arena_rect_xy(
      min(shapes$x) - margin,
      max(shapes$x) + margin,
      min(shapes$y) - margin,
      max(shapes$y) + margin
    ),
    material = "grass"
  )
  rbind(apron, shapes)
}

track_geometry <- function(lanes) {
  list(lanes = lanes, kerb = 36.5, straight = 84.39, lane = 1.22)
}

# The track's outline at radius r: the home straight along the bottom, then
# counter-clockwise round the far bend, the back straight and the near bend.
track_oval_xy <- function(t, r, n = 90) {
  h <- t$straight / 2
  far <- arena_arc_xy(h, 0, r, -pi / 2, pi / 2, n)
  near <- arena_arc_xy(-h, 0, r, pi / 2, 3 * pi / 2, n)
  arena_close(list(x = c(-h, far$x, near$x), y = c(-r, far$y, near$y)))
}

# Lane i: its edges and the line it is measured along.
track_lane <- function(t, i) {
  inner <- t$kerb + (i - 1) * t$lane
  measured <- if (i == 1) t$kerb + 0.30 else inner + 0.20
  list(
    inner = inner,
    outer = inner + t$lane,
    measured = measured,
    length = 2 * t$straight + 2 * pi * measured
  )
}

# A line across a lane, s metres along its measurement line from the finish,
# counter-clockwise.
track_across <- function(t, lane, s) {
  h <- t$straight / 2
  r <- lane$measured
  s <- s %% lane$length
  edges <- c(lane$inner, lane$outer)
  bend <- function(cx, a) list(x = cx + edges * cos(a), y = edges * sin(a))
  if (s < pi * r) {
    return(bend(h, -pi / 2 + s / r))
  }
  s <- s - pi * r
  if (s < t$straight) {
    return(list(x = c(h - s, h - s), y = edges))
  }
  s <- s - t$straight
  if (s < pi * r) {
    return(bend(-h, pi / 2 + s / r))
  }
  s <- s - pi * r
  list(x = c(-h + s, -h + s), y = -edges)
}

# Hurdle marks across each lane: the distance from the start to the first
# hurdle, between hurdles, with ten flights (Rule 22).
track_hurdles <- function(t, lanes, race) {
  spec <- list(
    `100m` = c(13, 8.5),
    `110m` = c(13.72, 9.14),
    `400m` = c(45, 35)
  )[[race]]
  distance <- as.numeric(sub("m", "", race))
  at <- spec[1] + spec[2] * 0:9
  marks <- list()
  for (i in seq_len(lanes)) {
    lane <- track_lane(t, i)
    for (j in seq_along(at)) {
      s <- if (race == "400m") {
        lane$length - distance + at[j]
      } else {
        # On the home straight, measured back from the finish
        -(distance - at[j])
      }
      xy <- if (race == "400m") {
        track_across(t, lane, s)
      } else {
        x <- t$straight / 2 + s
        list(x = c(x, x), y = -c(lane$inner, lane$outer))
      }
      marks[[length(marks) + 1]] <- arena_shape(
        "hurdles",
        "line",
        xy$x,
        xy$y,
        path = (i - 1) * 10 + j
      )
    }
  }
  marks
}

arena_javelin <- function(sector_length, runway_length) {
  check_dimensions(sector_length = sector_length, runway_length = runway_length)
  r <- 8
  a <- asin(2 / r)
  arc_x <- r * cos(a)
  half <- 28.96 / 2 * pi / 180
  arc <- arena_arc_xy(0, 0, r, -a, a, n = 30)
  parts <- list(
    arena_xy(
      "sector",
      "surface",
      arena_close(list(
        x = c(arc$x, rev(sector_length * cos(seq(-a, a, length.out = 30)))),
        y = c(arc$y, rev(sector_length * sin(seq(-a, a, length.out = 30))))
      )),
      material = "grass"
    ),
    arena_xy(
      "runway",
      "surface",
      arena_close(list(
        x = c(arc_x - runway_length, arc$x, arc_x - runway_length),
        y = c(-2, arc$y, 2)
      )),
      material = "track"
    ),
    arena_xy("arc", "line", arc)
  )
  for (side in c(-1, 1)) {
    parts <- c(
      parts,
      list(
        arena_shape(
          "runway_lines",
          "line",
          c(arc_x - runway_length, arc_x),
          c(2, 2) * side,
          path = (side + 3) / 2
        ),
        arena_shape(
          "arc_ends",
          "line",
          c(arc_x, arc_x),
          side * c(2, 2.75),
          path = (side + 3) / 2
        ),
        arena_shape(
          "sector_lines",
          "line",
          c(arc_x, sector_length * cos(side * a)),
          c(2 * side, sector_length * sin(side * a)),
          path = (side + 3) / 2
        )
      )
    )
  }
  as_arena(with_apron(parts), "m", "javelin runway", style = "sport")
}

arena_horizontal_jump <- function(runway_length, take_off, pit_end, name) {
  check_dimensions(runway_length = runway_length, take_off = take_off)
  w <- 1.22 / 2
  pit <- 2.75 / 2
  parts <- list(
    arena_xy(
      "runway",
      "surface",
      arena_rect_xy(-runway_length, take_off, -w, w),
      material = "track"
    ),
    arena_xy(
      "pit",
      "surface",
      arena_rect_xy(take_off, pit_end, -pit, pit),
      material = "sand"
    ),
    arena_shape(
      "runway_lines",
      "line",
      c(-runway_length, take_off),
      c(-w, -w),
      path = 1L
    ),
    arena_shape(
      "runway_lines",
      "line",
      c(-runway_length, take_off),
      c(w, w),
      path = 2L
    ),
    # The board's edge nearer the pit is the take-off line, at the origin
    arena_xy("take_off_board", "line", arena_rect_xy(-0.20, 0, -w, w))
  )
  as_arena(with_apron(parts), "m", name, style = "sport")
}

arena_high_jump <- function(runway_radius) {
  check_dimensions(runway_length = runway_radius)
  fan <- arena_arc_xy(0, 0, runway_radius, pi / 2, 3 * pi / 2, n = 60)
  parts <- list(
    arena_xy("runway", "surface", arena_close(fan), material = "track"),
    arena_xy(
      "landing_area",
      "surface",
      arena_rect_xy(0, 4, -3, 3),
      material = "mat"
    ),
    arena_shape("crossbar", "line", c(0, 0), c(-2.02, 2.02)),
    arena_shape("uprights", "spot", c(0, 0), c(-2.02, 2.02))
  )
  as_arena(with_apron(parts), "m", "high jump", style = "sport")
}

arena_pole_vault <- function(runway_length) {
  check_dimensions(runway_length = runway_length)
  w <- 1.22 / 2
  # Front pieces reach 2m forward either side of the box, 0.125m clear of it
  front <- function(side) {
    arena_close(list(
      x = c(-2, -1, 0, 0, -2),
      y = side * c(0.425, 0.425, 0.2, 3, 3)
    ))
  }
  parts <- list(
    arena_xy(
      "runway",
      "surface",
      arena_rect_xy(-runway_length, 0, -w, w),
      material = "track"
    ),
    arena_xy(
      "landing_area",
      "surface",
      arena_rect_xy(0, 6, -3, 3),
      material = "mat"
    ),
    arena_xy("landing_area", "surface", front(1), path = 2L, material = "mat"),
    arena_xy("landing_area", "surface", front(-1), path = 3L, material = "mat"),
    arena_shape(
      "runway_lines",
      "line",
      c(-runway_length, -2),
      c(-w, -w),
      path = 1L
    ),
    arena_shape(
      "runway_lines",
      "line",
      c(-runway_length, -2),
      c(w, w),
      path = 2L
    ),
    # Box: 1m long, 0.60m wide at the front, 0.15m at the stop board
    arena_xy(
      "box",
      "line",
      arena_close(list(x = c(-1, 0, 0, -1), y = c(-0.30, -0.075, 0.075, 0.30)))
    ),
    arena_shape("zero_line", "line", c(0, 0), c(-2.2, 2.2)),
    arena_shape("uprights", "spot", c(0, 0), c(-2.165, 2.165))
  )
  as_arena(with_apron(parts), "m", "pole vault", style = "sport")
}
