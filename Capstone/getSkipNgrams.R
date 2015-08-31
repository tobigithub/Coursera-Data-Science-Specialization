# Skip-n-grams aus einem character vector erstellen, in dem ein Element ein
# Dokument ist.
# Die Funktion trennt Sätze anhand von Interpunktion oder Zahlen. Alles Weitere
# sollte in den Inputdaten schon gesäubert sein.
getSkipNgrams <- function(text, n = 3){
      if (n < 3) stop("n < 3 and skip == TRUE is not meaningful")
      require(data.table)
      text <- (lapply(lapply(text,
                                   strsplit,
                                   split = "[[:space:]]+"),
                            unlist))
      text <- sapply(text, FUN = function(x){
            x <- unlist(strsplit(x, split = " "))
            if(length(x) >= n){
                  lx <- seq_along(x)
                  lx <- head(lx, n = length(lx) - n + 1)
                  sapply(lx, FUN = function(y){
                        return(c(x[y], x[y + n - 1]))
                  })
            }
      })
      names(text) <- NULL
      # Drop list elements that are NULL
      text <- text[!unlist(lapply(text, is.null))]
      text <- data.frame(text)
      colnames(text) <- NULL
      text <- sapply(text, paste, collapse = " ")
      text <- data.table(ngram = text, count = 1)
      return(text[, lapply(.SD, sum), by = ngram])
}
