library(purrr)
library(furrr)
library(progressr)
plan(list(tweak(multicore, workers = availableCores()%/%2), tweak(multicore, workers = 1)))


mytxtProgressBar = function(min = 0, max = 1, initial = 0, char = "=", width = NA, 
    title, label, style = 1, file = "") 
{
    if (!identical(file, "") && !(inherits(file, "connection") && 
        isOpen(file))) 
        stop("'file' must be \"\" or an open connection object")
    if (!style %in% 1L:4L) 
        style <- 1
    .val <- initial
    .killed <- FALSE
    .nb <- 0L
    .pc <- -1L
    nw <- nchar(char, "w")
    if (nw == 0) 
        stop("'char' must have a non-zero width")
    if (is.na(width)) {
        width <- getOption("width")
        if (style == 3L || style == 4L) 
            width <- width - 10L
        if (nw > 1) 
            width <- trunc(width/nw)
    }
    if (max <= min) 
        stop("must have 'max' > 'min'")
    up1 <- function(value) {
        if (!is.finite(value) || value < min || value > max) 
            return()
        .val <<- value
        nb <- round(width * (value - min)/(max - min))
        if (.nb < nb) {
            cat(strrep(char, nb - .nb), file = file)
            flush.console()
        }
        else if (.nb > nb) {
            cat("\r", strrep(" ", .nb * nw), "\r", strrep(char, 
                nb), sep = "", file = file)
            flush.console()
        }
        .nb <<- nb
    }
    up2 <- function(value) {
        if (!is.finite(value) || value < min || value > max) 
            return()
        .val <<- value
        nb <- round(width * (value - min)/(max - min))
        if (.nb <= nb) {
            cat("\r", strrep(char, nb), sep = "", file = file)
            flush.console()
        }
        else {
            cat("\r", strrep(" ", .nb * nw), "\r", strrep(char, 
                nb), sep = "", file = file)
            flush.console()
        }
        .nb <<- nb
    }
    up3 <- function(value) {
        if (!is.finite(value) || value < min || value > max) 
            return()
        .val <<- value
        nb <- round(width * (value - min)/(max - min))
        pc <- round(100 * (value - min)/(max - min))
        if (nb == .nb && pc == .pc) 
            return()
        cat(paste0("\r  |", strrep(" ", nw * width + 6)), file = file)
        cat(paste(c("\r  |", rep.int(char, nb), rep.int(" ", 
            nw * (width - nb)), sprintf("| %3d%%", pc)), collapse = ""), 
            file = file)
        flush.console()
        .nb <<- nb
        .pc <<- pc
    }
    up4 <- function(value) {
        if (!is.finite(value) || value < min || value > max) 
            return()
        .val <<- value
        nb <- round(width * (value - min)/(max - min))
        pc <- round(100 * (value - min)/(max - min))
        if (nb == .nb && pc == .pc) 
            return()
        cat(paste0("\r  |", strrep(" ", nw * width + 6)), file = file)
        cat(paste(c("\r  |", rep.int(char, nb), rep.int(" ", 
            nw * (width - nb)), sprintf("| %3d/%d ", value - min, max - min)), collapse = ""), 
            file = file)
        flush.console()
        .nb <<- nb
        .pc <<- pc
    }
    getVal <- function() .val
    kill <- function() if (!.killed) {
        cat("\n", file = file)
        flush.console()
        .killed <<- TRUE
    }
    up <- switch(style, up1, up2, up3, up4)
    up(initial)
    structure(list(getVal = getVal, up = up, kill = kill), class = "txtProgressBar")
}


eraseTxtProgressBar = function(pb) 
{
    if (inherits(pb, "voidProgressBar")) 
        return()
    pb_env <- environment(pb$getVal)
    with(pb_env, {
        if (style == 1L || style == 2L) {
            n <- .nb
        }
        else if (style == 3L || style == 4L) {
            n <- 3L + nw * width + 18L
        }
        cat("\r", strrep(" ", times = n), "\r", sep = "", file = file)
        flush.console()
        .nb <- 0L
        .pc <- -1L
    })
}


redrawTxtProgressBar = function(pb) 
{
    if (inherits(pb, "voidProgressBar")) 
        return()
    setTxtProgressBar(pb, value = pb$getVal())
}


myhandler_txtprogressbar = function(char = "=", style = 3L, file = stderr(), intrusiveness = getOption("progressr.intrusiveness.terminal", 
    1), target = "terminal", ...) 
{
    backend_args <- progressr:::handler_backend_args(char = char, style = style, 
        ...)
    reporter <- local({
        pb <- NULL
        make_pb <- function(max, ...) {
            if (!is.null(pb)) 
                return(pb)
            args <- c(list(max = max, ...), backend_args)
            pb <<- do.call(mytxtProgressBar, args = args)
            pb
        }
        list(reset = function(...) {
            pb <<- NULL
        }, hide = function(...) {
            if (is.null(pb)) return()
            eraseTxtProgressBar(pb)
        }, unhide = function(...) {
            if (is.null(pb)) return()
            redrawTxtProgressBar(pb)
        }, interrupt = function(config, state, progression, ...) {
            if (is.null(pb)) return()
            eraseTxtProgressBar(pb)
            redrawTxtProgressBar(pb)
            msg <- conditionMessage(progression)
            msg <- paste(c("", msg, ""), collapse = "\n")
            cat(msg, file = file)
        }, initiate = function(config, state, progression, ...) {
            if (!state$enabled || config$times == 1L) return()
            progressr:::stop_if_not(is.null(pb))
            make_pb(max = config$max_steps, file = file)
        }, update = function(config, state, progression, ...) {
            if (!state$enabled || config$times == 1L) return()
            make_pb(max = config$max_steps, file = file)
            if (inherits(progression, "sticky")) {
                eraseTxtProgressBar(pb)
                message(paste0(state$message, ""))
                redrawTxtProgressBar(pb)
            }
            if (progression$amount == 0) return()
            setTxtProgressBar(pb, value = state$step)
        }, finish = function(config, state, progression, ...) {
            if (is.null(pb)) return()
            if (!state$enabled) return()
            if (config$clear) {
                eraseTxtProgressBar(pb)
                pb_env <- environment(pb$getVal)
                file <- pb_env$file
                pb_env$file <- tempfile()
                on.exit({
                  if (file_test("-f", pb_env$file)) file.remove(pb_env$file)
                  pb_env$file <- file
                })
            } else {
                setTxtProgressBar(pb, value = config$max_steps)
            }
            close(pb)
            pb <<- NULL
        })
    })
    make_progression_handler("txtprogressbar", reporter, intrusiveness = intrusiveness, 
        target = target, ...)
}


options(progressr.enable = T)
options(cli.progress_handlers = "progressr")
handlers(myhandler_txtprogressbar(style = 4L, file = "", intrusiveness = 1, clear = T))
c1 = furrr_options(chunk_size = 1)


maps = map_vec
map2s = map2_vec
imaps=\(...) list_simplify(imap(...), strict=F)
pmaps = pmap_vec

vmap = \(.x, .f) with_progress(map(.x, .f, .progress = T), delay_stdout = F, delay_terminal = F)
vmaps = \(.x, .f) with_progress(map_vec(.x, .f, .progress = T), delay_stdout = F, delay_terminal = F)
vimap = \(.x, .f) with_progress(map2(.x, purrr:::vec_index(.x), .f, .progress = T), delay_stdout = F, delay_terminal = F)
vimaps = \(.x, .f) with_progress(map2_vec(.x, purrr:::vec_index(.x), .f, .progress = T), delay_stdout = F, delay_terminal = F)
vmap2 = \(.x, .y, .f) with_progress(map2(.x, .y, .f, .progress = T), delay_stdout = F, delay_terminal = F)
vmap2s = \(.x, .y, .f) with_progress(map2_vec(.x, .y, .f, .progress = T), delay_stdout = F, delay_terminal = F)
vpmap = \(.l, .f) with_progress(pmap(.l, .f, .progress = T), delay_stdout = F, delay_terminal = F)
vpmaps = \(.l, .f) with_progress(pmap_vec(.l, .f, .progress = T), delay_stdout = F, delay_terminal = F)


fmap=\(.x, .f) if (length(.x)<256) future_map(.x, .f, .options = c1) else future_map(.x, .f)
fmap2=\(.x, .y, .f) if (length(.x)<256) future_map2(.x, .y,.f, .options = c1) else future_map2(.x, .y, .f)
fimap=\(.x, .f) if (length(.x)<256) future_imap(.x, .f, .options = c1) else future_imap(.x, .f)
fpmap=\(.x, .f) if (length(.x[[1]])<256) future_pmap(.x, .f, .options = c1) else future_pmap(.x, .f)

fmaps=\(...) list_simplify(fmap(...), strict=F)
fmap2s=\(...) list_simplify(fmap2(...), strict=F)
fimaps=\(...) list_simplify(fimap(...), strict=F)
fpmaps=\(...) list_simplify(fpmap(...), strict=F)



fv = \(.x, .f, rp = 1, ff, isp = F) {
    if (isp)
        n = length(.x[[1]]) else n = length(.x)
    if (n < 256) 
        .options = c1 else .options = furrr_options()
    with_progress({
        p = progressor(steps = n%/%rp)
        .f = as_mapper(.f)
        rp1 = 1/rp
        f = \(...) {
            out = .f(...)
            if (rp <= 1 || runif(1) < rp1) 
                p()
            out
        }
        ff(.x, f, .options = .options)
    }, delay_stdout = F, delay_terminal = F)
}


fvmap2 = \(.x, .y, .f, rp = 1, isp = F) {
    if (isp)
        n = length(.x[[1]]) else n = length(.x)
    if (n < 256) 
        .options = furrr_options(chunk_size = 1) else .options = furrr_options()
    with_progress({
        p = progressor(steps = n%/%rp)
        .f = as_mapper(.f)
        rp1 = 1/rp
        f = \(...) {
            out = .f(...)
            if (rp <= 1 || runif(1) < rp1) 
                p()
            out
        }
        future_map2(.x, .y, f, .options = .options)
    }, delay_stdout = F, delay_terminal = F)
}

fvmap = \(...) fv(..., ff = future_map)
fvimap = \(...) fv(..., ff = future_imap)
fvpmap = \(...) fv(..., ff = future_pmap, isp = T)


fvmaps=\(...) list_simplify(fvmap(...), strict=F)
fvmap2s=\(...) list_simplify(fvmap2(...), strict=F)
fvimaps=\(...) list_simplify(fvimap(...), strict=F)
fvpmaps=\(...) list_simplify(fvpmap(...), strict=F)


