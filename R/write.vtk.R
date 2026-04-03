#' Convert the FLAMINGO predicted 3D genome structure into .vtk format for visualization
#' @param points 3D coordiantes predicted by FLAMINGO in x,y,z format.
#' @param lookup_table The annotation for each point, could be labels or scores, i.e. TAD annotations.
#' @param name output file name
#' @param opt_path output file path
#' @keywords write.vtk
#' @return Write out a .vtk file for visualization using Paraview
#' @export
write.vtk <- function(points,lookup_table,name,opt_path){
  points <- as.matrix(points)
  if (!is.numeric(points)) {
    stop("`points` must be numeric and contain x, y, z coordinates.")
  }
  if (ncol(points) != 3) {
    stop("`points` must have exactly 3 columns (x, y, z).")
  }

  n_points <- nrow(points)

  if (length(lookup_table) != n_points) {
    stop("`lookup_table` length must match the number of rows in `points`.")
  }

  if (is.numeric(lookup_table)) {
    scalar_type <- "float"
    lookup_values <- lookup_table
  } else {
    warning("`lookup_table` is not numeric; converting labels to integer ids for VTK output.")
    lookup_values <- as.integer(as.factor(lookup_table))
    scalar_type <- "int"
  }

  con <- file(opt_path, open = "wt")
  on.exit(close(con), add = TRUE)

  writeLines(c(
    "# vtk DataFile Version 1.0",
    as.character(name),
    "ASCII",
    "DATASET POLYDATA",
    paste("POINTS", n_points, "float")
  ), con)

  writeLines(apply(points, 1, function(x) paste(x, collapse = " ")), con)

  writeLines(c(
    paste("LINES 1", n_points + 1),
    paste(c(n_points, 0:(n_points - 1)), collapse = " "),
    paste("POINT_DATA", n_points),
    paste("SCALARS volume", scalar_type),
    "LOOKUP_TABLE default"
  ), con)

  writeLines(as.character(lookup_values), con)
}
