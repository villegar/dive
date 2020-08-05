#' Arrange files containing raw sequence filenames and their corresponding
#' MD5 hash. This files should be sorted as two column table.
#' MD5SUM FILENAME
#'
#' @param extension preferred file extension, default: md5.txt
#' @param subdirectory subdirectory containing the files, within the current
#' working directory.
#'
#' @export
#'
# @examples
raw_md5 <- function(extension = "md5.txt", subdirectory = "") {
  files <- list.files(here::here(subdirectory), pattern = paste0("*", extension))
  raw_sequences_md5 <- data.frame() # Create empty data frame
  for(f in files){ # Read each file containing the MD5SUM and RAW SEQUENCE NAMES
    tmp <- read.table(f, col.names = c("MD5SUM","RAW_SEQUENCE_NAME"), stringsAsFactors = FALSE)
    raw_sequences_md5 <- rbind(raw_sequences_md5,tmp) # Cat by rows
  }
  
  # Export as CSV the combined data
  write.csv(raw_sequences_md5, here::here("raw_sequences_md5.csv"))
}