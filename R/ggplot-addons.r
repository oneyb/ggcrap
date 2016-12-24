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
##' From: http://stackoverflow.com/questions/23497682/alter-facet-grid-output-in-ggplot2
##' @title Move facet strip y
##' @param object
##' @param sane.defaults
##' @return \code{grid} object
##' @examples
##' library(reshape2)
##' ## simulate a model study
##' air <- transform(airquality, Ozone.model = Ozone +
##'                                rnorm(length(Ozone), 2),
##'                  Wind.model = Wind + rnorm(length(Wind)))
##' ## Add time, see ?airquality
##' air$dtm <- as.Date(paste("1973", air$Month, air$Day,
##'                          sep = "-"), format="%Y-%m-%d", tz="EST")
##' d.f <- melt(air, id.vars = "dtm", measure.vars = c("Wind", "Wind.model",
##'                                                    "Ozone", "Ozone.model"))
##' d.f$type <- ifelse(grepl("model", d.f$variable), "Modeled", "Measured")
##' d.f$Variables <- sub("\\.model", "", d.f$variable)
##' plot <- ggplot(d.f, aes(dtm, value, color=type)) + geom_line()
##' (box <- plot + facet_grid(Variables~., scales="free_y"))
##' x11()
##' (move_facet_strip_y(box))
##'
##' ## Use fancy units
##' d.f$Variables <- rerevalue(d.f$variable, c("Wind~'('*m~s^{-1}*')'"="Wind.*", "Ozone~'('*mol~mol^{-1}*')'"="Ozone.*"))
##' plot <- ggplot(d.f, aes(dtm, value, color=type)) + geom_line()
##' (box <- plot + facet_grid(Variables~., scales="free_y", labeller=label_parsed))
##' x11()
##' (move_facet_strip_y(box))
##'
##' ## The other way around
##' d.f <- melt(airquality, id.vars = c("Ozone"),
##'             measure.vars = c("Temp", "Solar.R"))
##' ## d.f$Variables <- sub("\\.model", "", d.f$variable)
##' plot <- ggplot(d.f, aes(value, Ozone)) + geom_point()
##' (box <- plot + facet_grid(.~variable, scales="free_x"))
##' x11()
##' (test <- move_facet_strip_x(box))
##'
##' ## please note that \code{ggsave} will no longer work with earlier  versions (<= 2.0.0) of ggplot2
##' ## ggsave("test.pdf", test)
##' ## browseURL("test.pdf")
##' @export
move_facet_strip_y <- function(object, sane.defaults = TRUE)
{
  if (sane.defaults)
  {
    object <- object +
      theme(strip.text.y = element_text(angle=90)) +
      labs(y = "")
  }
  g <- ggplotGrob(object)
  ## remove the widths
  g$widths <- g$widths[-g$layout[grep("strip-r", g$layout$name), "r"]]
  ## remove the y-label grob
  index <- which(unlist(lapply(sapply(g$grobs, "[[", "name"),
                               function(x)
                                 any(grepl(pattern="axis.title.y", x)))))
  axis.title.y <- g$grobs[index]
  g$grobs <- g$grobs[-index]
  ## adjust the whole table
  adjustment.index <- g$layout[, "l"] >=
    g$layout[grep("strip-r", g$layout$name), "r"][1]
  ## Pull them up
  g$layout[adjustment.index, c("l", "r")] <- g$layout[adjustment.index,
                                                      c("l", "r")] - 1
  g$layout[g$layout$name == "background", "r"] <-
    g$layout[g$layout$name == "background", "r"] - 1
  ## Find bordering elements
  ## adjustment.index <- g$layout[, "r"] ==
  ##     unique(g$layout[g$layout$name == "strip-right", "r"]) - 1
  ## Pull them over
  ## g$layout[adjustment.index, "r"] <- g$layout[adjustment.index , "r"] + 1
  ## Push panels over
  g$layout[grepl("strip-r", g$layout$name), c("l", "r")] <-
    g$layout[grep("ylab", g$layout$name)[1], c("l", "r")]
  index <- g$layout[, "z"] > g$layout[grepl("ylab", g$layout$name), "z"][1]
  g$layout[index, "z"] <- g$layout[index, "z"] - 1
  g$layout <- g$layout[-grep("ylab", g$layout$name)[1], ]
  ## change colors to sane defaults
  if (sane.defaults)
  {
    ## index <- which(unlist(lapply(sapply(g$grobs, "[[", "childrenOrder"),
    ##                              function(x)
    ##     any(grepl(pattern="strip.text.y|strip.background", x)))))
    ## browser()
    index <- grep("strip-r", g$layout$name)
    ## loop over panels
    for(i in index) {
      place <- grep("strip.background", names(g$grobs[[i]]$grobs[[1]]$children))
      g$grobs[[i]]$grobs[[1]]$children[[place]][["gp"]][["col"]] <- "transparent"
      g$grobs[[i]]$grobs[[1]]$children[[place]][["gp"]][["fill"]] <- "transparent"
      place2 <- grep("stripGrob", names(g$grobs[[i]]$grobs[[1]]$children))
      g$grobs[[i]]$grobs[[1]]$children[[place2]]$children[[1]]$gp <-
        axis.title.y[[1]]$children[[1]]$gp
      ## place <- grep("strip.background", g$grobs[[i]]$childrenOrder)
      ## g$grobs[[i]]$children[[place]][["gp"]][["col"]] <- "transparent"
      ## g$grobs[[i]]$children[[place]][["gp"]][["fill"]] <- "transparent"
    }
  }
  class(g) <- c("ggcrap", class(g))
  g
}

##' @rdname move_facet_strip_y
##' @export
move_facet_strip_x <- function(object, sane.defaults = TRUE)
{
  if (sane.defaults)
  {
    object <- object + labs(x = "")
  }
  g <- ggplotGrob(object)
  strip.index <- g$layout[grep("strip-t", g$layout$name)[2], "t"]
  g$heights <- g$heights[-strip.index]
  index <- which(unlist(lapply(sapply(g$grobs, "[[", "name"),
                               function(x)
                                 any(grepl(pattern="axis.title.x", x)))))
  axis.title.x <- g$grobs[index]
  g$grobs <- g$grobs[-index]
  adjustment.index <- g$layout[, "t"] >= strip.index
  ## Pull them up
  g$layout[adjustment.index, c("t", "b")] <- g$layout[adjustment.index,
                                                      c("t", "b")] - 1
  ## Push panels over
  g$layout[grep("strip-t", g$layout$name), c("t", "b")] <-
    g$layout[grep("xlab", g$layout$name)[2], c("t", "b")]
  g$layout[g$layout$name == "background", "b"] <-
    g$layout[grep("xlab", g$layout$name)[2], "b"] + 1
  index <- g$layout[, "z"] > g$layout[grep("xlab", g$layout$name)[2], "z"]
  g$layout[index, "z"] <- g$layout[index, "z"] - 1
  g$layout <- g$layout[-grep("xlab", g$layout$name)[2], ]
  ## change colors to sane defaults
  if (sane.defaults)
  {
    ## index <- which(unlist(lapply(sapply(g$grobs, "[[", "childrenOrder"),
    ##                              function(x)
    ##     any(grepl(pattern="strip.text.x|strip.background", x)))))
    index <- grep("strip-t", g$layout$name)
    ## browser()
    ## loop over panels
    for(i in index) {
      place <- grep("strip.background", names(g$grobs[[i]]$grobs[[1]]$children))
      g$grobs[[i]]$grobs[[1]]$children[[place]][["gp"]][["col"]] <- "transparent"
      g$grobs[[i]]$grobs[[1]]$children[[place]][["gp"]][["fill"]] <- "transparent"
      place2 <- grep("stripGrob", names(g$grobs[[i]]$grobs[[1]]$children))
      g$grobs[[i]]$grobs[[1]]$children[[place2]]$children[[1]]$gp <-
        axis.title.x[[1]]$children[[1]]$gp
      ## place <- grep("strip.background", g$grobs[[i]]$childrenOrder)
      ## g$grobs[[i]]$children[[place]][["gp"]][["col"]] <- "transparent"
      ## g$grobs[[i]]$children[[place]][["gp"]][["fill"]] <- "transparent"
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
print.ggcrap <- function(g) {
    grid::grid.draw(g)
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
