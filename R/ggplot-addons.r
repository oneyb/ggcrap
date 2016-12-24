##' A convenience function for labeling facets for publication
##'
##' needs the same \code{data.frame} and facet formula as the plot on top of which it is being layered
##' @title Label facets
##' @param facets A \code{formula}: the same that is supplied to \code{facet_grid}
##' @param data a \code{data.frame}:
##' @param labels labels to plot
##' @param x see \code{?geom_text}
##' @param y see \code{?geom_text}
##' @param hjust see \code{?geom_text}
##' @param vjust see \code{?geom_text}
##' @param size see \code{?geom_text}
##' @param fontface see \code{?geom_text}
##' @param ... extra arguments to \code{geom_text}
##' @return \code{geom_text} object
##' @export
label_facet <- function(facets, data, labels=paste0(LETTERS, ")"), x=-Inf,
                        y=Inf, hjust=-.2, vjust=1.4,
                        size=rel(3.6), fontface="bold", ...)
{
    facets <- eval(parse(text = sub("\ \\.$|^.\ ", "0", deparse(facets))))
    d.f <- ddply(data, facets, nrow)
    d.f$labels <- labels[1:nrow(d.f)]
    geom_text(aes(label = labels), data = d.f, x = x, y = y,
              hjust = hjust, vjust = vjust, size = size, fontface = fontface, ...)
}
##' @rdname label_facet
##' @export
guess_dim_facet <- function(facets, data)
{
    facets <- eval(parse(text = sub("\ \\.$|^.\ ", "0", deparse(facets))))
    ## print(ddply(data, facets, nrow))
    dim(ddply(data, facets, nrow)) - 1
}

##' A brutish fix to allow plotting of time series stacked on top of each
##' other.  It will work for more than just time series.  Probably violates a few principles
##' of the grammar of graphics and usually requires the violation of tidy data
##' principles.
##'
##' takes \code{ggplot} object and changes it to a \code{grob}, mauls it a bit, and
##'     spits out a \code{grob}
##' @title Move facet strip y
##' @param object
##' @param sane.defaults
##' @return \code{grid} object
##' @examples
##' library(reshape2)
##' ## simulate a model study
##' air <- transform(airquality, Ozone.model = Ozone +
##'     rnorm(length(Ozone), 2),
##'                         Wind.model = Wind + rnorm(length(Wind)))
##' ## Add time, see ?air
##' air$dtm <- as.Date(paste("1973", air$Month, air$Day,
##'                                 sep = "-"), format="%Y-%m-%d", tz="EST")
##' d.f <- melt(air, id.vars = "dtm", measure.vars = c("Wind",
##'     "Wind.model", "Ozone", "Ozone.model"))
##' d.f$type <- ifelse(grepl("model", d.f$variable), "Modeled", "Measured")
##' d.f$Variables <- sub("\\.model", "", d.f$variable)
##' ## The problem
##' (box <- ggplot(d.f, aes(dtm, value, color=type)) +
##'     geom_line() +
##'      facet_grid(Variables~., scales="free_y"))
##' ## A solution, please note that \code{ggsave} will no longer work
##' move_facet_strip_y(box)
##'
##' @export
move_facet_strip_y <- function(object, sane.defaults = TRUE)
{
    ## From: http://stackoverflow.com/questions/23497682/alter-facet-grid-output-in-ggplot2
    if (sane.defaults)
    {
        object <- object +
            theme(strip.text.y = element_text(angle=90)) +
            labs(y = "")
    }
    g <- ggplotGrob(object)
    g$widths <- g$widths[-g$layout[g$layout$name == "strip-right", "r"][1]]
    index <- which(unlist(lapply(sapply(g$grobs, "[[", "name"),
                                 function(x)
        any(grepl(pattern="axis.title.y", x)))))
    g$grobs <- g$grobs[-index]
    adjustment.index <- g$layout[, "l"] >=
        g$layout[g$layout$name == "strip-right", "r"][1]
    ## Pull them up
    g$layout[adjustment.index, c("l", "r")] <- g$layout[adjustment.index ,
                                                        c("l", "r")] - 1
    g$layout[g$layout$name == "background", "r"] <-
        g$layout[g$layout$name == "background", "r"] - 1
    ## Find bordering elements
    ## adjustment.index <- g$layout[, "r"] ==
    ##     unique(g$layout[g$layout$name == "strip-right", "r"]) - 1
    ## Pull them over
    ## g$layout[adjustment.index, "r"] <- g$layout[adjustment.index , "r"] + 1
    ## Push panels over
    g$layout[g$layout$name == "strip-right", c("l", "r")] <-
        g$layout[g$layout$name == "ylab", c("l", "r")]
    index <- g$layout[, "z"] > g$layout[g$layout$name == "ylab", "z"]
    g$layout[index, "z"] <- g$layout[index, "z"] - 1
    g$layout <- g$layout[g$layout$name != "ylab", ]
    ## change colors to sane defaults
    if (sane.defaults)
    {
        ## index <- which(unlist(lapply(sapply(g$grobs, "[[", "childrenOrder"),
        ##                              function(x)
        ##     any(grepl(pattern="strip.text.y|strip.background", x)))))
        index <- which(g$layout$name %in% "strip-right")
        ## loop over panels
        for(i in index) {
            place <- grep("strip.background", g$grobs[[i]]$childrenOrder)
            g$grobs[[i]]$children[[place]][["gp"]][["col"]] <- "transparent"
            g$grobs[[i]]$children[[place]][["gp"]][["fill"]] <- "transparent"
        }
    }
    class(g) <- c("ggcrap", class(g))
    g
}

##' @rdname move_facet_strip_y
##' @export
move_facet_strip_x <- function(object, sane.defaults = TRUE)
{
    ## From: http://stackoverflow.com/questions/23497682/alter-facet-grid-output-in-ggplot2
    if (sane.defaults)
    {
        object <- object + labs(x = "")
    }
    g <- ggplotGrob(object)
    ## Find bordering elements
    ## old.layout <- g$layout
    ## g$layout <- old.layout
    g$heights <- g$heights[-3]
    index <- which(unlist(lapply(sapply(g$grobs, "[[", "name"),
                                     function(x)
                any(grepl(pattern="axis.title.x", x)))))
    g$grobs <- g$grobs[-index]
    adjustment.index <- g$layout[, "t"] >= 3
    ## Pull them up
    g$layout[adjustment.index, c("t", "b")] <- g$layout[adjustment.index ,
                                                        c("t", "b")] - 1
    ## Push panels over
    g$layout[g$layout$name == "strip-top", c("t", "b")] <-
        g$layout[g$layout$name == "xlab", c("t", "b")]
    g$layout[g$layout$name == "background", "b"] <-
        g$layout[g$layout$name == "xlab", "b"] + 1
    index <- g$layout[, "z"] > g$layout[g$layout$name == "xlab", "z"]
    g$layout[index, "z"] <- g$layout[index, "z"] - 1
    g$layout <- g$layout[g$layout$name != "xlab", ]
    ## change colors to sane defaults
    if (sane.defaults)
    {
        ## index <- which(unlist(lapply(sapply(g$grobs, "[[", "childrenOrder"),
        ##                              function(x)
        ##     any(grepl(pattern="strip.text.x|strip.background", x)))))
        index <- which(g$layout$name %in% "strip-top")
        ## loop over panels
        for(i in index) {
            place <- grep("strip.background", g$grobs[[i]]$childrenOrder)
            g$grobs[[i]]$children[[place]][["gp"]][["col"]] <- "transparent"
            g$grobs[[i]]$children[[place]][["gp"]][["fill"]] <- "transparent"
        }
    }
    class(g) <- c("ggcrap", class(g))
    g
}
##' S3 method to plot \code{grid} object result from \code{move_facet_strip_y}
##'
##'
##' @param g Result from \code{move_facet_strip_y}
##' @export
print.crap <- function(g) {
    grid.draw(g)
}
##' A more robust label parser
##'
##' a fillin replacement for \code{label_parsed}
##' @param variable variable (ggplot internal)
##' @param value variable (ggplot internal)
##' @return a \code{list} that is useful for panel text (i.e. \code{strip.text.x})
##' @export
label_parser <-  function (variable, value)
{
    llply(as.character(value), function(x){
        if (class(try(output <- parse(text = x), silent=TRUE)) == "try-error")
            if (class(try(output <- parse(text = gsub(" ", "~", x)), silent=TRUE)) == "try-error")
                output <- parse(text = gsub(",", "*','", gsub(" ", "~",x)))
        output
    })
}
