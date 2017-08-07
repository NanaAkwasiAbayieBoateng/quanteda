#' bootstrap a corpus or character object by resampling sentences
#' 
#' Bootstrap a corpus or character object by resampling from sentences, with
#' replacement.
#' @param x a \link{corpus} or character object
#' @param n number of resamples
#' @param verbose if \code{TRUE} print status messages
#' @details Function produces multiple, resampled character or \link{corpus} or
#'   objects, based on resampling sentences (wth replacement) from each
#'   document, recombining these into new "documents" and computing a dfm for
#'   each. Resampling of sentences is done strictly within document, so that
#'   every resampled document will contain at least some of its original tokens.
#' @return A named list of character or \link{corpus} objects, where the first,
#'   the object name followed by \code{_0}, is the original item before
#'   resampling, and the subsequent elements are the sentence-resampled text
#'   objects.
#' @author Kenneth Benoit
#' @export
#' @keywords corpus experimental bootstrap
#' @examples 
#' set.seed(5)
#' # bootstrapping from a character
#' txt <- c(textone = "This is a sentence.  Another sentence.  Yet another.", 
#'          texttwo = "Premiere phrase.  Deuxieme phrase.")
#' bootstrap_character(txt, n = 3)
#' 
#' # bootstrapping from a corpus 
#' corp <- corpus(txt)
#' lapply(bootstrap_corpus(corp, n = 3), texts)
bootstrap_corpus <- function(x, n = 10, verbose = quanteda_options("verbose")) {
    UseMethod("bootstrap_corpus")
}

#' @rdname bootstrap_corpus
#' @export
bootstrap_corpus.corpus <- function(x, n = 10, verbose = quanteda_options("verbose")) {
    if (verbose) 
        message("Segmenting the ", 
                stringi::stri_replace_all_fixed(as.character(sys.calls()[2][[1]])[1], "bootstrap_dfm.", ""),
                " into sentences...", appendLF = FALSE)
    corp_sentences <- corpus_reshape(x, to = "sentences")
    if (verbose) message("done.")
    
    if (verbose) {
        message("Bootstrapping the documents to create multiple resampled objects...")
        message("   ...resampling: 0", appendLF = FALSE)
    }
    
    result <- list()
    result[['resample_0']] <- x     # the original corpus
    for (i in seq_len(n)) {
        if (verbose) message(", ", i, appendLF = FALSE)
        result[[paste0("resample_", i)]] <- 
            corpus_reshape(corpus_sample(corp_sentences, by = "_docid", replace = TRUE), 
                           to = "documents")
    }
    
    if (verbose) 
        message("\n   ...complete.\n")
    
    class(result) <- c("corpus_bootstrap")
    return(result)
}

#' @rdname bootstrap_corpus
#' @export
bootstrap_character <- function(x, n = 10, verbose = quanteda_options("verbose")) {
    UseMethod("bootstrap_character")
}

#' @rdname bootstrap_corpus
#' @export
bootstrap_character.character <- function(x, n = 10, verbose = quanteda_options("verbose")) {
    result <- bootstrap_corpus(corpus(x), n = n, verbose = verbose)
    result <- lapply(result, texts)
    class(result) <- c("character_bootstrap")
    result
}
