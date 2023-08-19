## borrow unexported functions from 'tools' needed to avoid using ':::'

## tools:::.Rd_get_argument_names - this is called directly
.Rd_get_argument_names <- function (x) 
{
    x <- .Rd_get_section(x, "arguments")
    if (!length(x)) 
        return(character())
    txt <- .Rd_get_item_tags(x)
    txt <- unlist(strsplit(txt, ", *"))
    txt <- gsub("\\\\l?dots", "...", txt)
    txt <- gsub("\\_", "_", txt, fixed = TRUE)
    trimws(txt)
}

## the functions below are called (directly or indirectly) by .Rd_get_argument_names

## tools:::.Rd_get_section
.Rd_get_section <- function (x, which, predefined = TRUE){
    if (predefined) 
        x <- x[RdTags(x) == paste0("\\", which)]
    else {
        x <- x[RdTags(x) == "\\section"]
        if (length(x)) {
            ind <- sapply(x, function(e) .Rd_get_text(e[[1L]])) == 
                which
            x <- lapply(x[ind], `[[`, 2L)
        }
    }
    if (!length(x)) 
        x
    else structure(x[[1L]], class = "Rd")
}

## tools:::.Rd_get_item_tags
.Rd_get_item_tags <- function (x){
    x <- x[RdTags(x) == "\\item"]
    out <- lapply(x[lengths(x) == 2L], function(e) .Rd_deparse(e[[1L]]))
    as.character(unlist(out))
}

## tools:::RdTags
RdTags <- function (Rd) {
    res <- lapply(Rd, attr, "Rd_tag")
    if (length(res)) 
        simplify2array(res, FALSE)
    else character()
}


## tools:::.Rd_deparse
.Rd_deparse <- function (x, tag = TRUE) {
    if (!tag) 
        attr(x, "Rd_tag") <- "Rd"
    paste(tools:::as.character.Rd(x), collapse = "")
}

## tools:::.Rd_get_section
.Rd_get_section <- function (x, which, predefined = TRUE){
    if (predefined) 
        x <- x[RdTags(x) == paste0("\\", which)]
    else {
        x <- x[RdTags(x) == "\\section"]
        if (length(x)) {
            ind <- sapply(x, function(e) .Rd_get_text(e[[1L]])) == 
                which
            x <- lapply(x[ind], `[[`, 2L)
        }
    }
    if (!length(x)) 
        x
    else structure(x[[1L]], class = "Rd")
}

## tools:::.Rd_get_text
.Rd_get_text <- function (x){
    if (is.character(x)) 
        return(c(x))
    rval <- NULL
    file <- textConnection("rval", "w", local = TRUE)
    save <- options(useFancyQuotes = FALSE)
    Rdsave <- Rd2txt_options(underline_titles = FALSE)
    sink(file)
    tryCatch(Rd2txt(x, fragment = TRUE), finally = {
        sink()
        options(save)
        Rd2txt_options(Rdsave)
        close(file)
    })
    if (is.null(rval)) 
        rval <- character()
    else enc2utf8(rval)
}


## the following are for .Rd_deparse
## > tools:::as.character.Rd
## function (x, deparse = FALSE, ...) 
## {
##     ZEROARG <- c("\\cr", "\\dots", "\\ldots", "\\R", "\\tab")
##     MULTIARG <- c("\\section", "\\subsection", "\\item", "\\enc", 
##         "\\method", "\\S3method", "\\S4method", "\\tabular", 
##         "\\if", "\\href", "\\ifelse")
##     USERMACROS <- c("USERMACRO", "\\newcommand", "\\renewcommand")
##     EQN <- c("\\deqn", "\\eqn", "\\figure")
##     modes <- c(RLIKE = 1L, LATEXLIKE = 2L, VERBATIM = 3L, INOPTION = 4L, 
##         COMMENTMODE = 5L, UNKNOWNMODE = 6L)
##     tags <- c(RCODE = 1L, TEXT = 2L, VERB = 3L, COMMENT = 5L, 
##         UNKNOWN = 6L)
##     state <- c(braceDepth = 0L, inRString = 0L)
##     needBraces <- FALSE
##     inEqn <- 0L
##     pr <- function(x, quoteBraces) {
##         tag <- attr(x, "Rd_tag")
##         if (is.null(tag) || tag == "LIST") 
##             tag <- ""
##         if (is.list(x)) {
##             savestate <- state
##             state <<- c(0L, 0L)
##             needBraces <<- FALSE
##             if (tag == "Rd") {
##                 result <- character()
##                 for (i in seq_along(x)) result <- c(result, pr(x[[i]], 
##                   quoteBraces))
##             }
##             else if (startsWith(tag, "#")) {
##                 if (deparse) {
##                   dep <- deparseRdElement(x[[1L]][[1L]], c(state, 
##                     modes["LATEXLIKE"], inEqn, as.integer(quoteBraces)))
##                   result <- c(tag, dep[[1L]])
##                 }
##                 else result <- c(tag, x[[1L]][[1L]])
##                 for (i in seq_along(x[[2L]])) result <- c(result, 
##                   pr(x[[2L]][[i]], quoteBraces))
##                 result <- c(result, "#endif\n")
##             }
##             else if (tag %in% ZEROARG) {
##                 result <- tag
##                 needBraces <<- TRUE
##             }
##             else if (tag %in% MULTIARG) {
##                 result <- tag
##                 for (i in seq_along(x)) result <- c(result, pr(x[[i]], 
##                   quoteBraces))
##             }
##             else if (tag %in% EQN) {
##                 result <- tag
##                 inEqn <<- 1L
##                 result <- c(result, pr(x[[1L]], quoteBraces))
##                 inEqn <<- 0L
##                 if (length(x) > 1L) 
##                   result <- c(result, pr(x[[2L]], quoteBraces))
##             }
##             else {
##                 result <- tag
##                 if (!is.null(option <- attr(x, "Rd_option"))) 
##                   result <- c(result, "[", pr(option, quoteBraces), 
##                     "]")
##                 result <- c(result, "{")
##                 for (i in seq_along(x)) result <- c(result, pr(x[[i]], 
##                   quoteBraces))
##                 result <- c(result, "}")
##             }
##             if (state[1L]) 
##                 result <- pr(x, TRUE)
##             state <<- savestate
##         }
##         else if (tag %in% USERMACROS) {
##             result <- c()
##         }
##         else {
##             if (deparse) {
##                 dep <- deparseRdElement(as.character(x), c(state, 
##                   tags[tag], inEqn, as.integer(quoteBraces)))
##                 result <- dep[[1L]]
##                 state <<- dep[[2L]][1L:2L]
##             }
##             else {
##                 if (inherits(x, "Rd")) 
##                   class(x) <- setdiff(class(x), "Rd")
##                 result <- as.character(x)
##             }
##             if (needBraces) {
##                 if (grepl("^[[:alpha:]]", result)) 
##                   result <- c("{}", result)
##                 needBraces <<- FALSE
##             }
##         }
##         result
##     }
##     if (is.null(attr(x, "Rd_tag"))) 
##         attr(x, "Rd_tag") <- "Rd"
##     pr(x, quoteBraces = FALSE)
## }


## > tools:::deparseRdElement
## function (element, state) 
##     .Call(C_deparseRd, element, state)
