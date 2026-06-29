# plot.check_pose() -- DEFERRED until aniframe provides a centroid helper.
#
# `check_pose` diagnoses keypoint spread: for every frame it needs the centroid
# of the tracked keypoints and each keypoint's distance to it, then the
# distribution of those distances per keypoint. The centroid maths must NOT be
# hand-rolled -- it will come from an aniframe helper (e.g. add_centroid() /
# calculate_distance_to_centroid()). Once that lands, this check is essentially
# a re-skinned check_confidence (a per-keypoint distribution), so reuse that
# machinery rather than starting from scratch:
#
# 1. anicheck_files/check_pose.R (compute half, bound for the anicheck package):
#    - check_pose.aniframe(data): require a `keypoint` column with >= 2 keypoints;
#      use the aniframe centroid helper to get each keypoint's per-frame distance
#      to the centroid (do NOT compute the centroid manually here).
#    - Reduce to a COMPACT object, exactly like check_confidence: a per-group
#      kernel-density grid of the distances (cf. confidence_density()) plus a
#      five-number summary attribute (distribution_summary()). Group by
#      aniframe_group_cols(). Class c("check_pose","tbl_df","tbl","data.frame").
#    - summary.check_pose() / print.check_pose() mirroring check_confidence.
#    - Tests in anicheck_files/test-check_pose.R, mirroring test-check_confidence.
#
# 2. plot.check_pose() + as_plot_data.check_pose() (this file, anivis):
#    structurally identical to plot.check_confidence() / as_plot_data.check_
#    confidence(). Reuse them -- ideally factor the shared violin builder
#    (density grid -> clipped horizontal mirrored polygons + median/IQR overlay,
#    theme_imputets(), vertical-only gridlines, facets stacked as rows, the
#    "single varying group on the axis, the rest facet" rule) into one internal
#    helper called by both, instead of duplicating it. Differences for pose:
#      - x label "distance to centroid (<unit_space>)", title "Keypoint Spread
#        Around the Centroid";
#      - distances are >= 0 and unbounded (not [0, 1] like confidence), so build
#        the density over [0, max] rather than [min, max].
#
# See git history for the earlier hand-rolled-centroid draft that was reverted,
# and the other check_*/plot.check_* pairs for the established split.
