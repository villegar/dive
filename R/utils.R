#' Arrange files containing raw sequence filenames and their corresponding
#' MD5 hash. This files should be sorted as two column table.
#' MD5SUM FILENAME
#'
#' @param extension preferred file extension, default: md5.txt
#' @param subdirectory subdirectory containing the files, within the current
#' working directory.
#'
#' @export
raw_md5 <- function(extension = "md5.txt", subdirectory = "") {
  extension <- paste0("*", extension, "$") # Adding regex pattern
  files <- list.files(here::here(subdirectory), pattern = extension)
  raw_sequences_md5 <- data.frame() # Create empty data frame
  for(f in files) { # Read each file containing the MD5SUM and RAW SEQUENCE NAMES
    tmp <- read.table(f, col.names = c("MD5SUM", "RAW_SEQUENCE_NAME"), stringsAsFactors = FALSE)
    raw_sequences_md5 <- rbind(raw_sequences_md5, tmp) # Cat by rows
  }
  
  # Export as CSV the combined data
  write.csv(raw_sequences_md5, here::here("raw_sequences_md5.csv"))
}

#' Compute MD5 hashes recursively for files matching an extension within a 
#' subdirectory in the current path.
#'
#' @param extension preferred file extension, default: fasta
#' @param subdirectory subdirectory containing the files, within the current
#' 
#' @export
get_md5 <- function(extension = "fasta", subdirectory = "") {
  # List subdirectories inside the current working directory
  dirs <- unique(c("", list.dirs(here::here(subdirectory),
                                 full.names = FALSE, recursive = FALSE)))
  # Loop through the subdirectories
  for(d in dirs) {
    # List files inside the current subdirectory, that match a extension
    files <- list.files(here::here(d), pattern = paste0(".", extension, "$"))
    md5 <- data.frame()
    for(f in files) {
      fullpath <- here::here(d, f) # Full path to current element
      if (file.exists(fullpath) && !dir.exists(fullpath)) {
        # Compute MD5 hash
        tmp <- data.frame(MD5 = tools::md5sum(fullpath)[[1]], FILENAME = f)
        md5 <- rbind(md5, tmp)
      }
    }
    if (nrow(md5) > 0) {
      # Store the results as a table file: MD5 FILENAME
      output_name <- here::here(paste0(ifelse(d == "", "top", d), ".md5.txt"))
      colnames(md5) <- NULL
      write.table(md5, output_name, row.names = FALSE, quote = FALSE)
    }
  }
}

#' Create hexagonal logo for the package
#'
#' @param subplot image to use as the main logo
#' @param dpi plot resolution (dots-per-inch)
#' @param h_color color for hexagon border
#' @param h_fill color to fill hexagon
#' @param output output file (hexagonal logo)
#' @param package title for logo (package name)
#' @param p_color color for package name
#' @param url URL for package repository or website
#' @param u_size text size for URL
#'
#' @return hexagonal logo
#' @export
#'
#' @examples
#' hex_logo()
#' \dontrun{
#' hex_logo("inst/images/diving.png", output = "inst/images/logo.png")
#' }
hex_logo <- function(subplot = system.file("images/diving.png", package = "dive"),
                     dpi = 600,
                     h_color = "#000000",
                     h_fill = "#FFFFFF",
                     output = system.file("images/logo.png", package = "dive"),
                     package = "DIVe",
                     p_color = "#000000",
                     url = "https://github.com/villegar/dive",
                     u_size = 1.55) {
  hexSticker::sticker(subplot = subplot, package = package,
                      h_color = h_color,  h_fill = h_fill,
                      dpi = dpi,
                      s_x = 1.0, s_y = .85, s_width = .5,
                      p_x = 1.0, p_y = 1.52, p_size = 6, p_color = p_color,
                      url = url,
                      u_angle = 30, u_color = p_color, u_size = u_size,
                      filename = output)
}
