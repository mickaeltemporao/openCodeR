#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     qprep.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2017-09-14 11:59:28
# Modified:     2017-09-14 12:04:33
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2017 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
qprep <-
function(text.var, rm.dash = TRUE, bracket = "all", missing = NULL,
    names = FALSE, abbreviation = qdapDictionaries::abbreviations,
    replace = NULL, ignore.case = TRUE, num.paste = TRUE, ...) {

    if (!is.null(bracket)) {
        text.var <- bracketX(clean(text.var), bracket = bracket,
            missing = missing, names = names)
    }
    if (!is.null(abbreviation)) {
        text.var <- replace_abbreviation(text.var, abbreviation = abbreviation,
            replace = replace, ignore.case = ignore.case)
    }
    if (!is.null(num.paste)) {
        text.var <- replace_number(text.var, num.paste = num.paste)
    }
    if (rm.dash) {
        text.var <- gsub("-", " ", text.var)
    }
    Trim(scrubber(replace_symbol(text.var, ...)))
}

bracketX <-
function (text.var, bracket = "all", missing = NULL, names = FALSE,
    fix.space = TRUE, scrub = fix.space) {
    lside <- rside <- ""
    if (fix.space) {
        lside <- rside <- "[ ]*"
        text.var <- mgsub(c("(", ")","[", "]", "{", "}", "<", ">"),
            c(" (", ") "," [", "] ", " {", "} ", " <", "> "), text.var)
    }
    FUN <- function(bracket, text.var, missing, names) {
        X <- switch(bracket,
            html = sapply(text.var, function(x) gsub(paste0(lside, "<.*?>", rside), "", x)),
            angle = sapply(text.var, function(x) gsub(paste0(lside, "<.*?>", rside), "", x)),
            square = sapply(text.var, function(x) gsub(paste0(lside, "\\[.*?\\]", rside), "", x)),
            round = sapply(text.var, function(x) gsub(paste0(lside, "\\(.*?\\)", rside), "", x)),
            curly = sapply(text.var, function(x) gsub(paste0(lside, "\\{.*?\\}", rside), "", x)),
            all = {
                P1 <- sapply(text.var, function(x) gsub(paste0(lside, "\\[.*?\\]", rside), "", x))
                P1 <- sapply(P1, function(x) gsub(paste0(lside, "\\(.*?\\)", rside), "", x))
                P1 <- sapply(P1, function(x) gsub(paste0(lside, "<.*?>", rside), "", x))
                sapply(P1, function(x) gsub(paste0(lside, "\\{.*?\\}", rside), "", x))
            }
        )
        if (scrub) {
            X <- scrubber(gsub(" +", " ", X), fix.space = FALSE)
        }
        if (!is.null(missing)) {
            X[X == ""] <- missing
        }
        if (!names) names(X) <- NULL
        X
    }

    invisible(lapply(bracket, function(x) {
        text.var <<- FUN(x, text.var = text.var,
            missing = missing, names = names)
    }))
    text.var
}



#' Multiple gsub
#'
#' \code{multigsub} - A wrapper for \code{\link[base]{gsub}} that takes a vector
#' of search terms and a vector or single value of replacements.
#'
#' @param pattern Character string to be matched in the given character vector.
#' @param replacement Character string equal in length to pattern or of length
#' one which are  a replacement for matched pattern.
#' @param text.var The text variable.
#' @param leadspace logical.  If \code{TRUE} inserts a leading space in the
#' replacements.
#' @param trailspace logical.  If \code{TRUE} inserts a trailing space in the
#' replacements.
#' @param fixed logical. If \code{TRUE}, pattern is a string to be matched as is.
#' Overrides all conflicting arguments.
#' @param trim logical.  If \code{TRUE} leading and trailing white spaces are
#' removed and multiple white spaces are reduced to a single white space.
#' @param order.pattern logical.  If \code{TRUE} and \code{fixed = TRUE}, the
#' \code{pattern} string is sorted by number of characters to prevent substrings
#' replacing meta strings (e.g., \code{pattern = c("the", "then")} resorts to
#' search for "then" first).
#' @param \dots Additional arguments passed to \code{\link[base]{gsub}}.
#' @rdname multigsub
#' @return \code{multigsub} - Returns a vector with the pattern replaced.
#' @seealso \code{\link[base]{gsub}}
#' @export
#' @examples
#' \dontrun{
#' ## ======================
#' ##    `mgsub` Function
#' ## ======================
#'
#' multigsub(c("it's", "I'm"), c("it is", "I am"), DATA$state)
#' mgsub(c("it's", "I'm"), c("it is", "I am"), DATA$state)
#' mgsub("[[:punct:]]", "PUNC", DATA$state, fixed = FALSE)
#'
#' ## ======================
#' ## `sub_holder` Function
#' ## ======================
#'
#' ## `alpha.type` as TRUE
#' (fake_dat <- paste(emoticon[1:11,2], DATA$state))
#' (m <- sub_holder(emoticon[,2], fake_dat))
#' m$unhold(strip(m$output))
#' # With Stemming
#' m$unhold(stemmer(strip(m$output), capitalize = FALSE))
#'
#' ## `alpha.type` as FALSE (numeric keys)
#' vowels <- LETTERS[c(1, 5, 9, 15, 21)]
#' (m2 <- sub_holder(vowels, toupper(DATA$state), alpha.type = FALSE))
#' m2$unhold(gsub("[^0-9]", "", m2$output))
#' mtabulate(strsplit(m2$unhold(gsub("[^0-9]", "", m2$output)), ""))
#' }
multigsub <-
function (pattern, replacement, text.var, leadspace = FALSE,
    trailspace = FALSE, fixed = TRUE, trim = TRUE, order.pattern = fixed,
    ...) {

    if (leadspace | trailspace) replacement <- spaste(replacement, trailing = trailspace, leading = leadspace)

    if (fixed && order.pattern) {
        ord <- rev(order(nchar(pattern)))
        pattern <- pattern[ord]
        if (length(replacement) != 1) replacement <- replacement[ord]
    }
    if (length(replacement) == 1) replacement <- rep(replacement, length(pattern))

    for (i in seq_along(pattern)){
        text.var <- gsub(pattern[i], replacement[i], text.var, fixed = fixed, ...)
    }

    if (trim) text.var <- gsub("\\s+", " ", gsub("^\\s+|\\s+$", "", text.var, perl=TRUE), perl=TRUE)
    text.var
}

#' @rdname multigsub
#' @export
mgsub <- multigsub


#' Multiple gsub
#'
#' \code{sub_holder} - This function holds the place for particular character
#' values, allowing the user to manipulate the vector and then revert the place
#' holders back to the original values.
#'
#' @param alpha.type logical.  If \code{TRUE} alpha (lower case letters) are
#' used for the key.  If \code{FALSE} numbers are used as the key.
#' @return \code{sub_holder} - Returns a list with the following:
#' \item{output}{keyed place holder character vector}
#' \item{unhold}{A function used to revert back to the original values}
#' @rdname multigsub
#' @note The \code{unhold} function for \code{sub_holder} will only work on keys
#' that have not been disturbed by subsequent alterations.  The key follows the
#' pattern of `qdapplaceholder` followed by lower case letter keys followed by
#' `qdap`.
#' @export
sub_holder <- function(pattern, text.var, alpha.type = TRUE, ...) {

    if (!is.character(pattern)) pattern <- as.character(pattern)
    x2 <- x <- length(pattern)

    if (alpha.type) {
        counter <- 0
        while(x > 26) {
            x <- x/26
            counter <- counter + 1
        }
        if (x > 0) counter + 1
        keys <- paste2(expand.grid(lapply(1:counter, function(i) letters)), sep="")
        reps <- paste0("qdapplaceholder", keys, "qdap")
    } else {
        keys <- reps <- 1:x
    }

    output <- mgsub(pattern, reps, text.var, ...)


    FUN <- function(text.var, ...) {
        mgsub(reps, pattern, text.var)
    }

    out <- list(output = output, unhold = FUN)

    attributes(out) <- list(
        class = c("sub_holder", "list"),
        names = names(out),
        pattern = pattern,
        keys = keys,
        len = x2
    )
    out

}


#' Prints a sub_holder object
#'
#' Prints a sub_holder object
#'
#' @param x The sub_holder object
#' @param \ldots ignored
#' @export
#' @method print sub_holder
print.sub_holder <-
function(x, ...) {
    print(x[["output"]])
}


#' Remove Escaped Characters
#'
#' Preprocess data to remove escaped characters
#'
#' @param text.var The text variable
#' @return Returns a vector of character strings with escaped characters removed.
#' @keywords escaped character
#' @export
#' @examples
#' \dontrun{
#' x <- "I go \\r
#'     to the \\tnext line"
#' x
#' clean(x)
#' }
clean <-
function(text.var) {
    gsub("\\s+", " ", gsub("\\\\r|\\\\n|\\n|\\\\t", " ", text.var))
}


#' Clean Imported Text
#'
#' Use to clean text variables when importing a new data set.  Removes extra
#' white spaces other textual anomalies that may cause errors.
#'
#' @param text.var The text variable.
#' @param num2word logical If \code{TRUE} replaces a numbers with text
#' representations.
#' @param fix.comma logical If \code{TRUE} removes any spaces before a comma.
#' @param fix.space logical.  If \code{TRUE} extra spaces before endmarks are
#' removed.
#' @param rm.quote  logical If \code{TRUE} removes any \code{\"}.
#' @param \ldots Other arguments passed to \code{\link[qdap]{replace_number}}.
#' @return Returns a parsed character vector.
#' @seealso \code{\link[qdap]{strip}}
#' @keywords parse, clean
#' @export
#' @examples
#' \dontrun{
#' x <- c("I like 456 dogs\t  , don't you?", 'The end"')
#' scrubber(x)
#' scrubber(x, TRUE)
#' }
scrubber <-
function(text.var, num2word = FALSE, rm.quote = TRUE, fix.comma = TRUE,
    fix.space = TRUE, ...){
    x <- reducer(Trim(clean(text.var)))
    if (rm.quote) {
        x  <- gsub('\"', "", x)
    }
    if (fix.comma) {
        x <- gsub(" ,", ",", x)
    }
    ncx <- nchar(x)
    if (fix.space) {
        x <- Trim(gsub("(\\s+)([.?|!,*]+)$", "\\2", x))
    }
    x[is.na(text.var)] <- NA
    if (num2word) {
        x <- replace_number(x, ...)
    }
    x
}

#internal not exported
reducer <-
function(x) gsub("\\s+", " ", x)


#' Remove Leading/Trailing White Space
#'
#' Remove leading/trailing white space.
#'
#' @param x The text variable.
#' @return Returns a vector with the leading/trailing white spaces removed.
#' @export
#' @examples
#' \dontrun{
#' (x <- c("  talkstats.com ", "   really? ", " yeah"))
#' Trim(x)
#' }
Trim <-
function (x) gsub("^\\s+|\\s+$", "", x)


#' Replace Abbreviations
#'
#' This function replaces abbreviations with long form.
#'
#' @param text.var  The text variable.
#' @param abbreviation A two column key of abbreviations (column 1) and long
#' form replacements (column 2) or a vector of abbreviations.  Default is to use
#' qdapDictionaries's \code{\link[qdapDictionaries]{abbreviations}} data set.
#' @param replace A vector of long form replacements if a data frame is not
#' supplied to the abbreviation argument.
#' @param ignore.case logical.  If \code{TRUE} replaces without regard to
#' capitalization.
#' @return Returns a vector with abbreviations replaced.
#' @keywords abbreviation
#' @seealso
#' \code{\link[qdap]{bracketX}},
#' \code{\link[qdap]{qprep}},
#' \code{\link[qdap]{replace_contraction}},
#' \code{\link[qdap]{replace_number}},
#' \code{\link[qdap]{replace_symbol}}
#' @export
#' @examples
#' \dontrun{
#' x <- c("Mr. Jones is here at 7:30 p.m.",
#'     "Check it out at www.github.com/trinker/qdap",
#'     "i.e. He's a sr. dr.; the best in 2012 A.D.",
#'     "the robot at t.s. is 10ft. 3in.")
#'
#' replace_abbreviation(x)
#'
#' #create abbreviation and replacement vectors
#' abv <- c("in.", "ft.", "t.s.")
#' repl <- c("inch", "feet", "talkstats")
#'
#' replace_abbreviation(x, abv, repl)
#'
#' (KEY <- rbind(abbreviations, data.frame(abv = abv, rep = repl)))
#' replace_abbreviation(x, KEY)
#' }
replace_abbreviation <- function (text.var, abbreviation = qdapDictionaries::abbreviations,
    replace = NULL, ignore.case = TRUE) {

    if (!is.null(replace)) {
        ab <- data.frame(abv = abbreviation, repl = replace, stringsAsFactors = FALSE)
    } else {
        if (is.list(abbreviation)) {
            ab <- data.frame(abv = abbreviation[[1]], repl = abbreviation[[2]], stringsAsFactors = FALSE)
        } else {
            stop("must supply vector of abbreviations and vector of replacements")
        }
    }

    if (ignore.case) {
        ab[, 1] <- tolower(ab[, 1])
        caps <- function(string, all = FALSE) {
            capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))
            if (all) {
                x <- paste(unlist(lapply(strsplit(string, " "), capit)), collapse = " ")
                y <- paste(unlist(lapply(strsplit(x, NULL), capit)), collapse = "")
                x <- c(x, y)
            } else {
                x <- capit(string)
            }
            return(x)
        }
        ab2 <- do.call(rbind, list(ab, ab))
        temp <- unlist(lapply(ab2[, 1], caps, TRUE))
        ab2[, 1] <- temp[1:(length(temp)/2)]
        v <- as.character(ab[, 2])
        ab <- data.frame(rbind(ab, ab2), stringsAsFactors = FALSE)
        ab[, 2] <- c(v, rep(v, each = 2))
        ab[, 2] <- spaste(ab[, 2])
    }

    text.var <- Trim(text.var)
    pn <- which(substring(text.var, nchar(text.var)) == ".")
    text.var <- mgsub(ab[, 1], ab[, 2], text.var)
    x <- Trim(gsub("\\s+", " ", text.var))
    x[pn] <- sapply(x[pn], function(z) {
        if (substring(z, nchar(z)) != ".") {
            paste(z, ".", sep = "")
        } else {
            z
        }
    }, USE.NAMES = FALSE)

    scrubber(x)
}


#' Add Leading/Trailing Spaces
#'
#' Adds trailing and/or leading spaces to a vector of terms.
#'
#' @param terms A character vector of terms to insert trailing and/or leading
#' spaces.
#' @param leading logical.  If \code{TRUE} inserts a leading space in the terms.
#' @param trailing logical.  If \code{TRUE} inserts a trailing space in the
#' terms.
#' @return Returns a character vector with trailing and/or leading spaces.
#' @export
#' @examples
#' \dontrun{
#' spaste(Top25Words)
#' spaste(Top25Words, FALSE)
#' spaste(Top25Words, trailing = TRUE, leading = FALSE) #or
#' spaste(Top25Words, , FALSE)
#' }
spaste <-
function(terms, trailing = TRUE, leading = TRUE){
    if (leading) {
        s1 <- " "
    } else {
        s1 <- ""
    }
    if (trailing) {
        s2 <- " "
    } else {
        s2 <- ""
    }
    pas <- function(x) paste0(s1, x, s2)
    if (is.list(terms)) {
        z <- lapply(terms, pas)
    } else {
        z <- pas(terms)
    }
    return(z)
}


#' Replace Numbers With Text Representation
#'
#' Replaces numeric represented numbers with words (e.g., 1001 becomes one
#' thousand one).
#'
#' @param text.var  The text variable.
#' @param num.paste logical.  If \code{TRUE} a the elements of larger numbers are
#' separated with spaces.  If \code{FALSE} the elements will be joined without
#' spaces.
#' @param remove logical.  If \code{TRUE} numbers are removed from the text.
#' @return Returns a vector with abbreviations replaced.
#' @references Fox, J. (2005). Programmer's niche: How do you spell that number?
#' R News. Vol. 5(1), pp. 51-55.
#' @note The user may want to use \code{\link[qdap]{replace_ordinal}} first to
#' remove ordinal number notation.  For example \code{\link[qdap]{replace_number}}
#' would turn "21st" into "twenty onest", whereas \code{\link[qdap]{replace_ordinal}}
#' would generate "twenty first".
#' @keywords number-to-word
#' @seealso
#' \code{\link[qdap]{bracketX}},
#' \code{\link[qdap]{qprep}},
#' \code{\link[qdap]{replace_abbreviation}},
#' \code{\link[qdap]{replace_contraction}},
#' \code{\link[qdap]{replace_symbol}},
#' \code{\link[qdap]{replace_ordinal}}
#' @export
#' @examples
#' \dontrun{
#' x <- c("I like 346,457 ice cream cones.", "They are 99 percent good")
#' y <- c("I like 346457 ice cream cones.", "They are 99 percent good")
#' replace_number(x)
#' replace_number(y)
#' replace_number(x, FALSE)
#' replace_number(x, remove=TRUE)
#' }
replace_number  <-
function(text.var, num.paste = TRUE, remove = FALSE) {

    if (remove) return(gsub("[0-9]", "", text.var))

    ones <- c("zero", "one", "two", "three", "four", "five", "six", "seven",
        "eight", "nine")

    num.paste <- ifelse(num.paste, "separate", "combine")

    unlist(lapply(lapply(gsub(",([0-9])", "\\1", text.var), function(x) {
            if (!is.na(x) & length(unlist(strsplit(x,
                "([0-9])", perl = TRUE))) > 1) {
                num_sub(x, num.paste = num.paste)
            } else {
                x
            }
        }
    ), function(x) mgsub(0:9, ones, x)))

}

## Helper function to convert numbers
numb2word <- function(x){
    helper <- function(x){
        digits <- rev(strsplit(as.character(x), "")[[1]])
        nDigits <- length(digits)
        if (nDigits == 1) as.vector(ones[digits])
        else if (nDigits == 2)
            if (x <= 19) as.vector(teens[digits[1]])
                else trim(paste(tens[digits[2]],
    Recall(as.numeric(digits[1]))))
        else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred",
            Recall(makeNumber(digits[2:1]))))
        else {
            nSuffix <- ((nDigits + 2) %/% 3) - 1
            if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
            trim(paste(Recall(makeNumber(digits[
                nDigits:(3*nSuffix + 1)])),
                suffixes[nSuffix],
                Recall(makeNumber(digits[(3*nSuffix):1]))))
            }
        }
    trim <- function(text){
        gsub("^\ ", "", gsub("\ *$", "", text))
        }
    makeNumber <- function(...) as.numeric(paste(..., collapse=""))
    opts <- options(scipen=100)
    on.exit(options(opts))
    ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
        "eight", "nine")
    names(ones) <- 0:9
    teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
        "sixteen", " seventeen", "eighteen", "nineteen")
    names(teens) <- 0:9
    tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
        "ninety")
    names(tens) <- 2:9
    x <- round(x)
    suffixes <- c("thousand", "million", "billion", "trillion", "quadrillion",
        "quintillion", "sextillion", "septillion", "octillion", "nonillion",
        "decillion", "undecillion", "duodecillion", "tredecillion",
        "quattuordecillion")
    if (length(x) > 1) return(sapply(x, helper))
    helper(x)
}

## Helper function to sub out numbers
num_sub <- function(x, num.paste) {
    len <- attributes(gregexpr("[[:digit:]]+", x)[[1]])$match.length
    pos <- c(gregexpr("[[:digit:]]+", x)[[1]])
    values <- substring(x, pos, pos + len - 1)
    pos.end <- pos + len - 1
    replacements <- sapply(values, function(x) numb2word(as.numeric(x)))
    replacements <- switch(num.paste,
        separate = replacements,
        combine =  sapply(replacements, function(x)gsub(" ", "", x)),
        stop("Invalid num.paste argument"))
    numDF <- unique(data.frame(symbol = names(replacements),
        text = replacements))
    rownames(numDF) <- 1:nrow(numDF)
    pat <- paste(numDF[, "symbol"], collapse = "|")
    repeat {
        m <- regexpr(pat, x)
        if (m == -1)
            break
        sym <- regmatches(x, m)
        regmatches(x, m) <- numDF[match(sym, numDF[, "symbol"]),
            "text"]
    }
    return(x)
}



#' Replace Symbols With Word Equivalents
#'
#' This function replaces symbols with word equivalents (e.g., \code{@@} becomes
#' \code{"at"}.
#'
#' @param text.var  The text variable.
#' @param dollar logical.  If \code{TRUE} replaces dollar sign ($) with
#' \code{"dollar"}.
#' @param percent logical.  If \code{TRUE} replaces percent sign (\%) with
#' \code{"percent"}.
#' @param pound logical.  If \code{TRUE} replaces pound sign (#) with
#' \code{"number"}.
#' @param at logical.  If \code{TRUE} replaces at sign (@@) with \code{"at"}.
#' @param and logical.  If \code{TRUE} replaces and sign (&) with \code{"and"}.
#' @param with  logical.  If \code{TRUE} replaces with sign (w/) with
#' \code{"with"}.
#' @return Returns a character vector with symbols replaced..
#' @keywords symbol-replace
#' @seealso
#' \code{\link[qdap]{bracketX}},
#' \code{\link[qdap]{qprep}},
#' \code{\link[qdap]{replace_abbreviation}},
#' \code{\link[qdap]{replace_contraction}},
#' \code{\link[qdap]{replace_number}},
#' @export
#' @examples
#' \dontrun{
#' x <- c("I am @@ Jon's & Jim's w/ Marry",
#'     "I owe $41 for food",
#'     "two is 10% of a #")
#' replace_symbol(x)
#' }
replace_symbol <-
function(text.var, dollar = TRUE, percent = TRUE,
         pound = TRUE, at = TRUE, and = TRUE, with = TRUE) {
  x <- c(dollar, percent, pound, at, and, with, with)
  scrubber(mgsub(pattern = c("%", "$", "#", "&", "@", "w/o", "w/")[x],
        replacement = spaste(c("percent", "dollar", "number", "and", "at",
        "without", "with")[x]), text.var = text.var, fixed = TRUE,
         leadspace = FALSE, trailspace = FALSE))
}
