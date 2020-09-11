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

#' Generates DIVe hexagon logo for the repository
#'
#' @export
#'
#' @examples
#' hex_logo()
hex_logo <- function() {
  hexSticker::sticker("images/diving.png", package="DIVe", 
                      h_color = "black",  h_fill = "white",
                      l_x = 1, l_y = 1, spotlight = FALSE, 
                      s_x = 1.15, s_y = 1.0, s_width = .6, 
                      p_x = .75, p_y = 1.5, p_size = 8, p_color = "black",
                      filename="images/logo.png")
}
