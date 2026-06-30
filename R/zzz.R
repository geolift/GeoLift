# Copyright (c) Meta Platforms, Inc. and its affiliates.
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

.onLoad <- function(libname, pkgname) {
  # Patch augsynth::treated_table to handle multiple treated units.
  #
  # Bug in augsynth 0.2.0: when trt_index has length > 1, `t(df[trt_index, ])`
  # produces an n_time x n_units matrix, and `as.numeric()` on that gives
  # n_time * n_units elements — but `time` only has n_time elements.
  # tibble >= 3.0 rejects the size mismatch that older tibble silently recycled.
  #
  # Fix: use colMeans() across treated units, which yields a proper n_time vector
  # for both single and multiple treated unit cases.
  patched_treated_table <- function(augsynth) {
    if (inherits(augsynth, "summary.augsynth")) {
      return(augsynth$treated_table)
    }
    trt_index <- which(augsynth$data$trt == 1)
    df <- dplyr::bind_cols(augsynth$data$X, augsynth$data$y)
    synth_unit <- predict(augsynth)
    average_unit <- colMeans(df[-trt_index, , drop = FALSE])
    treated_unit <- colMeans(df[trt_index, , drop = FALSE])
    lvls <- tibble::tibble(
      time        = as.numeric(colnames(df)),
      Yobs        = as.numeric(treated_unit),
      Yhat        = as.numeric(synth_unit),
      raw_average = as.numeric(average_unit)
    )
    t0           <- ncol(augsynth$data$X)
    tpost        <- ncol(augsynth$data$y)
    lvls$tx      <- rep(c(0, 1), c(t0, tpost))
    lvls$ATT     <- lvls$Yobs - lvls$Yhat
    lvls$rstat   <- lvls$ATT / sqrt(mean(lvls$ATT[lvls$tx == 0]^2))
    lvls <- dplyr::relocate(lvls, time, tx, Yobs, Yhat, raw_average, ATT, rstat)
    return(lvls)
  }

  tryCatch(
    utils::assignInNamespace("treated_table", patched_treated_table, ns = "augsynth"),
    error = function(e) {
      packageStartupMessage(
        "GeoLift: could not patch augsynth::treated_table. ",
        "GeoLift() with multiple treatment locations may fail with tibble >= 3.0. ",
        "Consider updating augsynth."
      )
    }
  )
}
