#Those functions from Claude to decode compressed map data with base R only
# ewe_map_decode.R: decode compressed Ecospace map strings ("gzip:<base64>") from EwE models.
# Base R only (needs R >= 3.3.0). In a package run roxygen2::roxygenise(); as a script, source() it.

#' Decode base64 text to a raw vector (base R only)
#' @param x A single base64 string; padding and whitespace are ignored.
#' @return A raw vector.
#' @noRd
b64_to_raw <- function(x) {
  x <- gsub("[^A-Za-z0-9+/]", "", x)                    # drops '=', whitespace and any non-base64 character
  lut <- integer(256)
  lut[as.integer(charToRaw(paste(c(LETTERS, letters, 0:9, "+", "/"),
                                 collapse = ""))) + 1L] <- 0:63
  v <- lut[as.integer(charToRaw(x)) + 1L]               # 6-bit values
  n <- (length(v) * 3) %/% 4                            # number of real output bytes
  v <- c(v, rep(0L, (4L - length(v) %% 4L) %% 4L))      # pad to a multiple of 4
  q <- matrix(v, nrow = 4L)
  out <- rbind(bitwOr(bitwShiftL(q[1, ], 2L), bitwShiftR(q[2, ], 4L)),
               bitwOr(bitwShiftL(bitwAnd(q[2, ], 15L), 4L), bitwShiftR(q[3, ], 2L)),
               bitwOr(bitwShiftL(bitwAnd(q[3, ], 3L), 6L), q[4, ]))
  as.raw(out[seq_len(n)])
}

#' Gunzip a raw vector, stopping if the data is truncated or corrupt
#' @param raw A raw vector holding one complete gzip stream.
#' @return A raw vector of decompressed bytes.
#' @noRd
gunzip_raw <- function(raw) {
  con <- gzcon(rawConnection(raw))    # not memDecompress(): it can exhaust memory on truncated input
  on.exit(close(con))
  chunks <- list()
  repeat {
    x <- readBin(con, "raw", 65536L)
    if (length(x) == 0L) break
    chunks[[length(chunks) + 1L]] <- x
  }
  out <- unlist(chunks)
  isize <- sum(as.numeric(as.integer(raw[length(raw) - 3:0])) * 256^(0:3))  # size in gzip trailer (mod 2^32)
  if (length(out) != isize)
    stop("gzip data looks truncated or corrupt: read ", length(out),
         " bytes but the trailer says ", isize)
  out
}

#' Decode an Ecospace map string from an EwE model
#'
#' Converts a map layer stored as text in an EwE model, either plain or as
#' \code{"gzip:<base64>"}, into a list of numeric vectors, one per map row.
#' Rows are returned as stored and can differ in length; nothing is padded.
#'
#' @param s A single string: \code{"gzip:"} plus base64 text, or plain text
#'   with commas between values and semicolons between rows.
#' @param nodata Value marking cells without data; these become \code{NA}.
#' @return A list of numeric vectors, one per map row, in the order stored.
#' @note The format was inferred from example strings, not an EwE specification.
#'   How short rows align with map columns, and which edge row 1 is, are unverified.
#' @examples
#' x <- paste0("gzip:", "H4sIAAAAAAACA9O1BAIdXXTSGsI2NDLQM9UxNjFAETUyNNUxsTDQMzJF",
#'             "EcYgAbGwk5RcAAAA")
#' rows <- decode_ewe_map(x)
#' do.call(rbind, rows)         # a matrix, when all rows have the same length
#' decode_ewe_map("1,2,3;4,5")  # unequal rows are kept as stored
#' @noRd
decode_ewe_map <- function(s, nodata = -9999) {
  if (startsWith(s, "gzip:"))
    s <- rawToChar(gunzip_raw(b64_to_raw(substring(s, 6L, nchar(s)))))
  rows <- lapply(strsplit(strsplit(s, ";", fixed = TRUE)[[1]], ",", fixed = TRUE),
                 function(r) { r <- as.numeric(r); r[r == nodata] <- NA; r })
  if (length(rows) == 0L) stop("no map values found in the string")
  rows
}
