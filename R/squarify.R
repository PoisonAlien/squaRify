#This code is a direct translation of [squarify](https://github.com/laserson/squarify) by [Uri Laserson](https://github.com/laserson)
#See https://github.com/laserson/squarify for more details

# Squarified Treemap Layout
# Implements algorithm from Bruls, Huizing, van Wijk, "Squarified Treemaps"

pad_rectangle = function(rect) {
  if (rect$dx > 2) {
    rect$x = rect$x + 1
    rect$dx = rect$dx - 2
  }
  if (rect$dy > 2) {
    rect$y = rect$y + 1
    rect$dy = rect$dy - 2
  }
}

layoutrow = function(sizes, x, y, dx, dy) {
  covered_area = sum(sizes)
  width = covered_area / dy
  rects = list()
  for (size in sizes) {
    rects = c(rects, list(list(x = x, y = y, dx = width, dy = size / width)))
    y = y + size / width
  }
  return(rects)
}

layoutcol = function(sizes, x, y, dx, dy) {
  covered_area = sum(sizes)
  height = covered_area / dx
  rects = list()
  for (size in sizes) {
    rects = c(rects, list(list(x = x, y = y, dx = size / height, dy = height)))
    x = x + size / height
  }
  return(rects)
}

layoutMain = function(sizes, x, y, dx, dy) {
  if (dx >= dy) {
    return(layoutrow(sizes, x, y, dx, dy))
  } else {
    return(layoutcol(sizes, x, y, dx, dy))
  }
}

leftoverrow = function(sizes, x, y, dx, dy) {
  covered_area = sum(sizes)
  width = covered_area / dy
  leftover_x = x + width
  leftover_y = y
  leftover_dx = dx - width
  leftover_dy = dy
  return(list(leftover_x, leftover_y, leftover_dx, leftover_dy))
}

leftovercol = function(sizes, x, y, dx, dy) {
  covered_area = sum(sizes)
  height = covered_area / dx
  leftover_x = x
  leftover_y = y + height
  leftover_dx = dx
  leftover_dy = dy - height
  return(list(leftover_x, leftover_y, leftover_dx, leftover_dy))
}

leftover = function(sizes, x, y, dx, dy) {
  if (dx >= dy) {
    return(leftoverrow(sizes, x, y, dx, dy))
  } else {
    return(leftovercol(sizes, x, y, dx, dy))
  }
}

worst_ratio = function(sizes, x, y, dx, dy) {
  rects = layoutMain(sizes, x, y, dx, dy)
  return(max(sapply(rects, function(rect) {
    max(rect$dx / rect$dy, rect$dy / rect$dx)
  })))
}

squarify_main = function(sizes, x, y, dx, dy) {
  sizes = as.numeric(sizes)

  if (length(sizes) == 0) {
    return(list())
  }

  if (length(sizes) == 1) {
    return(layoutMain(sizes, x, y, dx, dy))
  }

  i = 1
  while (i < length(sizes) && worst_ratio(sizes[1:i], x, y, dx, dy) >= worst_ratio(sizes[1:(i + 1)], x, y, dx, dy)) {
    i = i + 1
  }
  current = sizes[1:i]
  remaining = sizes[(i + 1):length(sizes)]

  leftover_info = leftover(current, x, y, dx, dy)
  leftover_x = leftover_info[[1]]
  leftover_y = leftover_info[[2]]
  leftover_dx = leftover_info[[3]]
  leftover_dy = leftover_info[[4]]

  return(c(layoutMain(current, x, y, dx, dy), squarify_main(remaining, leftover_x, leftover_y, leftover_dx, leftover_dy)))
}

padded_squarify = function(sizes, x, y, dx, dy) {
  rects = squarify(sizes, x, y, dx, dy)
  for (rect in rects) {
    pad_rectangle(rect)
  }
  return(rects)
}

normalize_sizes = function(sizes, dx, dy) {
  total_size = sum(sizes)
  total_area = dx * dy
  sizes = sizes * (total_area / total_size)
  return(sizes)
}

#' Treemap in base R
#' @details Plot treemaps in base R
#' This function is a direct transpilation of python [squarify](https://github.com/laserson/squarify) package by [Uri Laserson](https://github.com/laserson)
#' @param X numeric vectors of values
#' @param labels a character vector specifying the text to be written for each value in X. Default NULL.
#' @param sub_labels a character vector specifying the sub-text to be written for each value in X. Default NULL.
#' @param fontSize numeric character expansion factor. Default 1
#' @param col colors. If colors is longer than x, the coordinates are recycled to the length of labels.
#' @param alpha Default 1
#' @param show_val Default TRUE.
#' @param show_pct Default FALSE.
#' @param text_col Default #2c3e50
#' @param sub_text_col Default #34495e
#' @param borderCol Default NULL
#' @param text_font Font type. Default 1. Can be 1, 2 (bold), 3 (italic), 4 (bold-italic)
#' @param sub_text_font Font type. Default 1. Can be 1, 2 (bold), 3 (italic), 4 (bold-italic)
#' @export
#' @importFrom graphics rect text
#' @importFrom grDevices hcl.colors
#' @examples
#' gdp <- system.file("extdata", "G7_vs_BRCIS_GDP.tsv", package = "squaRify")
#' gdp = read.delim(file = gdp)
#' g7 = subset(gdp, consortium == "G7")
#' squarify(X = g7$GDP_T, labels = g7$country)

squarify = function(X, labels = NULL, sub_labels = NULL, fontSize = 1, col = NULL, alpha = 1, show_val = TRUE,
                    show_pct = FALSE, text_col = "#2c3e50", sub_text_col = "#34495e",
                    borderCol = NULL, text_font = 1, sub_text_font = 1){
  x <- 0  # X-coordinate of the origin
  y <- 0  # Y-coordinate of the origin
  dx <- 100  # Full width of the treemap
  dy <- 100  # Full height of the treemap

  sizes_norm <- X / sum(X) * dx * dy  # Normalize the sizes

  if(is.null(col)){
    col = hcl.colors(n = length(X), palette = "Dark2")
  }else if(length(col) != length(X)){
    col = rep(col, length(X))
  }

  if(length(text_col) != length(X)){
    text_col = rep(text_col, length(X))
  }

  if(length(sub_text_col) != length(X)){
    sub_text_col = rep(sub_text_col, length(X))
  }

  col = grDevices::adjustcolor(col = col, alpha.f = alpha)

  # Compute the treemap rectangles
  rectangles <- squarify_main(sizes_norm, x, y, dx, dy)


  # Function to draw a rectangle
  draw_rect <- function(x, y, dx, dy, col, ...) {
    rect(x, y, x + dx, y + dy, col = col, ...)
  }

  # Function to draw text inside the rectangle
  draw_text <- function(x, y, dx, dy, label, fs = 1, ...) {
    text(x + dx / 2, y + dy / 2, label, cex = fs, xpd = TRUE, ...)
  }

  draw_text_sub <- function(x, y, dx, dy, label, fs = 1, ...) {
    text(x + dx/2, y + dy / 2, label, cex = fs * 0.8, pos = 1, xpd = TRUE, ...)
  }

  if(is.null(labels)){
    labels = seq_along(X)
  }

  plot(NA, xlim = c(0, dx), ylim = c(0, dy), xlab = "", ylab = "", xaxt = "n", yaxt = "n", frame.plot = FALSE)

  if(is.null(sub_labels)){
    sub_text = paste0(X)
  }else{
    sub_text = sub_labels
  }

  if(show_pct){
    sub_text = paste0(sub_text,  " [", round(X/sum(X) * 100, 2), "%]")
  }

  #print(rectangles)
  for (i in seq_along(rectangles)) {
    rect_info <- rectangles[[i]]
    draw_rect(rect_info$x, rect_info$y, rect_info$dx, rect_info$dy, col[i], border = borderCol)
    draw_text(rect_info$x, rect_info$y, rect_info$dx, rect_info$dy, labels[i], fs = fontSize, col = text_col[i], font = text_font) # Use original sizes
    if(show_val){
      draw_text_sub(rect_info$x, rect_info$y, rect_info$dx, rect_info$dy, sub_text[i], fs = fontSize, col = sub_text_col[i], font = sub_text_font) # Use original sizes
    }
  }
}
