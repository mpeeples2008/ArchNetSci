cvd_grid <- function(plot = last_plot(), severity = 1) {
  deut <- function(c)
    deutan(c, severity)
  p1 <- edit_colors(plot, deut)

  prot <- function(c)
    protan(c, severity)
  p2 <- edit_colors(plot, prot)

  trit <- function(c)
    tritan(c, severity)
  p3 <- edit_colors(plot, trit)

  des <- function(c)
    desaturate(c, severity)
  p4 <- edit_colors(plot, des)

  cowplot::plot_grid(
    p1,
    p2,
    p3,
    p4,
    scale = 0.9,
    hjust = 0,
    vjust = 1,
    labels = c("Deutanomaly", "Protanomaly",
               "Tritanomaly", "Desaturated"),
    label_x = 0.01,
    label_y = 0.99,
    label_size = 12,
    label_fontface = "bold"
  )
}

#' Edit colors in existing plot or grid object
#'
#' The function `edit_colors()` can modify colors in existing ggplot2
#' plots, grid objects,
#' or R base plots provided as recorded plots.
#'
#' @param plot The plot or grid graphics object to edit. The
#' function `edit_colors()` can
#'   accept any object that can be handled by [`cowplot::plot_to_gtable()`].
#' @param colfun The function used to edit colors (`col` in `gpar` objects).
#' @param fillfun The function used to edit fill
#' colors (`fill` in `gpar` objects).
#'   By default the same as `colfun`.
#' @param ... Other parameters to be given to functions colfun and fillfun.
#' @examples
#' library(ggplot2)
#' library(colorspace) # for desaturate
#' p <- ggplot(iris, aes(Sepal.Width, fill=Species)) +
#'   geom_density(alpha = 0.7)
#'
#' p2 <- edit_colors(p, deutan)
#' p3 <- edit_colors(p, tritan, sev = 7)
#' p4 <- edit_colors(p, desaturate)
#' cowplot::plot_grid(p, p2, p3, p4)
#' @export
#' @importFrom ggplot2 last_plot
edit_colors <- function(plot = last_plot(),
                        colfun = passthrough,
                        fillfun = NULL,
                        ...) {
  # convert to grob if necessary
  if (!methods::is(plot, "grob")) {
    plot <- cowplot::plot_to_gtable(plot)
  }

  if (is.null(fillfun)) {
    fillfun <- colfun
  }
  edit_grob_colors(plot, colfun, fillfun, ...)
}


#' The function `edit_grob_colors()` is identical to `edit_colors()`
#' except it works only with objects of
#' class `grob` and doesn't check its input.
#'
#' @param grob The grid graphics object to edit.
#' @rdname edit_colors
#' @export
edit_grob_colors <- function(grob, colfun, fillfun, ...) {
  if (!is.null(grob$gp)) {
    if (!is.null(grob$gp$col)) {
      grob$gp$col <- colfun(grob$gp$col, ...)
    }
    if (!is.null(grob$gp$fill)) {
      grob$gp$fill <- fillfun(grob$gp$fill, ...)
    }
  }

  if (!is.null(grob$grobs)) {
    grob$grobs <- lapply(grob$grobs, edit_grob_colors,
                         colfun, fillfun, ...)
  }

  if (!is.null(grob$children)) {
    grob$children <- lapply(grob$children, edit_grob_colors,
                            colfun, fillfun, ...)
  }

  if (methods::is(grob, "rastergrob")) {
    grob <- edit_rastergrob_colors(grob, colfun, ...)
  }

  grob
}


# internal function that can adjust rastergrobs
# important: it only changes the raster data, not
# any of the other gp data.
edit_rastergrob_colors <- function(grob, colfun, ...) {
  rasternew <- colfun(c(grob$raster), ...)
  dim(rasternew) <- dim(grob$raster)
  class(rasternew) <- class(grob$raster)
  grid::editGrob(grob, raster = rasternew)
}

#' @rdname scale_okabeito
#' @export
#' @usage NULL
scale_colour_okabeito <- function(aesthetics = "colour", ...) {
  scale_okabeito(aesthetics, ...)
}

#' @rdname scale_okabeito
#' @export
#' @usage NULL
scale_color_okabeito <- scale_colour_okabeito

#' @rdname scale_okabeito
#' @export
#' @usage NULL
scale_fill_okabeito <- function(aesthetics = "fill", ...) {
  scale_okabeito(aesthetics, ...)
}

#' Okabe-Ito color scale
#'
#' This is a color-blind friendly, qualitative scale with eight
#' different colors. See [palette_okabeito] for details.
#' @param use_black If `TRUE`, scale includes black, otherwise includes gray.
#' @param order Numeric vector listing the order in which the
#' colors should be used. Default is 1:8.
#' @param darken Relative amount by which the scale should be
#' darkened (for positive values) or lightened (for negatice
#'   values).
#' @param alpha Alpha transparency level of the color. Default
#' is no transparency.
#' @param ... common discrete scale parameters: `name`, `breaks`,
#' `labels`, `na.value`, `limits`, `guide`, and `aesthetics`.
#'  See [discrete_scale] for more details.
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point() + scale_color_okabeito()
#' ggplot(iris, aes(Sepal.Length, fill = Species)) +
#'   geom_density(alpha = 0.7) + scale_fill_okabeito(order = c(1, 3, 5))
#'
#' cowplot::plot_grid(
#'   gg_color_swatches(8) + scale_fill_okabeito(darken = 0.6),
#'   gg_color_swatches(8) + scale_fill_okabeito(darken = 0.4),
#'   gg_color_swatches(8) + scale_fill_okabeito(darken = 0.2),
#'   gg_color_swatches(8) + scale_fill_okabeito(darken = 0),
#'   gg_color_swatches(8) + scale_fill_okabeito(darken = -0.2),
#'   gg_color_swatches(8) + scale_fill_okabeito(darken = -0.4),
#'   gg_color_swatches(8) + scale_fill_okabeito(darken = -0.6), ncol = 1)
#' @export
#' @usage NULL
scale_okabeito <-
  function(aesthetics,
           use_black = FALSE,
           order = 1:8,
           darken = 0,
           alpha = NA,
           ...) {
    if (use_black) {
      values <- palette_okabeito_black[order]
    }
    else {
      values <- palette_okabeito[order]
    }

    n <- length(values)
    darken <- rep_len(darken, n)
    alpha <- rep_len(alpha, n)

    di <- darken > 0
    if (sum(di) > 0) {
      # at least one color needs darkening
      values[di] <-
        colorspace::darken(values[di], amount = darken[di])
    }

    li <- darken < 0
    if (sum(li) > 0) {
      # at least one color needs lightening
      values[li] <-
        colorspace::lighten(values[li], amount = -1 * darken[li])
    }

    ai <- !is.na(alpha)
    if (sum(ai) > 0) {
      # at least one color needs alpha
      values[ai] <- scales::alpha(values[ai], alpha[ai])
    }

    pal <- function(n) {
      if (n > length(values)) {
        warning(
          "Insufficient values in manual scale. ",
          n,
          " needed but only ",
          length(values),
          " provided.",
          call. = FALSE
        )
      }
      values
    }
    ggplot2::discrete_scale(aesthetics, "manual", pal, ...)
  }

#' Visualize a color palette as swatches of colors, possibly
#' with labels printed on top
#'
#' @param colors Vector of color names or hex codes.
#' @param label_size Size of the color labels to be printed.
#' @param label_family Font family of the labels
#' @param color_labels Individual bool or vector of bools
#' indicating for which colors the
#'   color names should be printed on top of the color swatch.
#' @param ... Other parameters to be handed off to [gg_color_swatches].
#' @examples
#' palette_plot(palette_okabeito)
#' palette_plot(c("red", "green", "yellow", "magenta"), color_labels = FALSE)
#' @importFrom ggplot2 ggplot aes geom_rect scale_fill_manual geom_text theme
#' @export
palette_plot <- function(colors,
                         label_size = 6,
                         label_family = "",
                         color_labels = TRUE,
                         ...) {
  if (length(color_labels) == 1)
    color_labels <- rep_len(color_labels, length(colors))

  # find light and dark colors by converting to Lab space
  cols <- t(grDevices::col2rgb(colors))
  m <-
    grDevices::convertColor(cols,
                            from = "sRGB",
                            to = "Lab",
                            scale.in = 255)
  light <- m[, 1] >= 50

  # data frame of rectangles
  n <- length(colors)
  tiles <- data.frame(
    x = (0:(n - 1) + .5) / n,
    y = rep(0.5, n),
    color = colors,
    light = light
  )

  # code to appease CRAN check
  x <- y <- color <- NULL

  gg_color_swatches(n = n, ...) +
    scale_fill_manual(values = colors) +
    geom_text(
      data = tiles[tiles$light & color_labels, ],
      aes(x, y, label = color),
      color = "black",
      size = label_size,
      family = label_family
    ) +
    geom_text(
      data = tiles[!tiles$light & color_labels, ],
      aes(x, y, label = color),
      color = "white",
      size = label_size,
      family = label_family
    )
}


#' Generate a set of color swatches to be colored via
#' ggplot2's `scale_fill_*` mechanism
#'
#' This function is similar to [palette_plot], with two main
#' differences: First, unlike
#' [palette_plot], `gg_color_swatches` cannot label the colors
#' with their hex code. Second, the colors don't
#' need to be provided as an argument, the resulting plot can be
#' directly styled by adding
#' a `scale_fill_*` expression. Thus, this function is particularly
#' useful to visualize
#' existing ggplot2 color scales.
#' @param n Number of color swatches to generate
#' @param xmargin Fraction of each swatch to be used as
#' margin in the x direction
#' @param ymargin Fraction of each swatch to be used as
#' margin in the y direction
#' @param title Optional title to print above the color swatches.
#' Can also be provided via [ggtitle].
#' @param title_size Font size of the title
#' @param title_family Font family of the title
#' @param title_face Font face of the title
#' @param plot_margin Margin around the plot, specified via
#' the ggplot2 function [margin]
#' @examples
#' gg_color_swatches(8) + scale_fill_okabeito()
#' gg_color_swatches(10) + ggtitle("Default ggplot2 discrete color scale")
#' @importFrom ggplot2 ggplot aes geom_rect theme element_text
#' scale_x_continuous scale_y_continuous ggtitle margin
#' @export
gg_color_swatches <- function(n = 10,
                              xmargin = 0.2,
                              ymargin = 0,
                              title = NULL,
                              title_size = 14,
                              title_family = "",
                              title_face = "plain",
                              plot_margin = margin(title_size / 2,
                                                   title_size / 2,
                                                   title_size / 2,
                                                   title_size / 2)) {
  tiles <- data.frame(
    xmin = (0:(n - 1) + xmargin / 2) / n,
    xmax = ((1:n) - xmargin / 2) / n,
    ymin = rep(0, n) + ymargin / 2,
    ymax = rep(1, n) - ymargin / 2,
    fill = factor(1:n)
  )

  # code to appease CRAN check
  xmax <- xmin <- ymax <- ymin <- fill <- NULL


  ggplot() +
    geom_rect(data = tiles,
              aes(
                xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax,
                fill = fill
              )) +
    scale_x_continuous(limits = c(.99 * xmargin / (2 * n), 1 - .99 * xmargin /
                                    (2 * n)),
                       expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggtitle(title) +
    cowplot::theme_nothing() +
    theme(
      plot.margin = plot_margin,
      plot.title = element_text(
        family = title_family,
        face = title_face,
        size = title_size,
        margin = margin(b = title_size / 2),
        hjust = 0,
        vjust = 0.5
      )
    )
}


#' Generate a continuous color gradient to be colored via
#' ggplot2's `scale_fill_*` mechanism
#'
#' This function creates a ggplot2 object that can be directly
#' styled by adding
#' a `scale_fill_*` expression. Thus, this function is particularly
#' useful to visualize
#' existing ggplot2 color scales.
#' @param n Number of distinct color slices to be drawn
#' @param ymargin Fraction of each swatch to be used as margin
#' in the y direction
#' @param title Optional title to print above the color swatches.
#' Can also be provided via [ggtitle].
#' @param title_size Font size of the title
#' @param title_family Font family of the title
#' @param title_face Font face of the title
#' @param plot_margin Margin around the plot, specified via the
#' ggplot2 function [margin]
#' @examples
#' gg_color_gradient() + ggtitle("Default ggplot2 continuous color scale")
#' @importFrom ggplot2 ggplot aes geom_raster theme element_text
#' scale_x_continuous scale_y_continuous ggtitle margin
#' @export
gg_color_gradient <- function(n = 200,
                              ymargin = 0,
                              title = NULL,
                              title_size = 14,
                              title_family = "",
                              title_face = "plain",
                              plot_margin = margin(title_size / 2,
                                                   title_size / 2,
                                                   title_size / 2,
                                                   title_size / 2)) {
  tiles <- data.frame(x = seq(-1, 1, length.out = n),
                      y = rep(0.5, n))

  # code to appease CRAN check
  x <- y <- NULL

  ggplot() +
    geom_raster(
      data = tiles,
      aes(x = x, y = y, fill = x),
      interpolate = TRUE,
      hjust = 0
    ) +
    scale_x_continuous(limits = c(-1, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, ymargin / 2)) +
    ggtitle(title) +
    cowplot::theme_nothing() +
    theme(
      plot.margin = plot_margin,
      plot.title = element_text(
        family = title_family,
        face = title_face,
        size = title_size,
        margin = margin(b = title_size / 2),
        hjust = 0,
        vjust = 0.5
      )
    )
}


#' Color palette proposed by Okabe and Ito
#'
#' Two color palettes taken from the article "Color Universal Design"
#' by Okabe and Ito, http://jfly.iam.u-tokyo.ac.jp/color/.
#' The variant `palette_okabeito` contains a gray color, while
#' `palette_okabeito_black` contains black instead.
#' @export
palette_okabeito <-
  c(
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7",
    "#999999"
  )

#' @rdname palette_okabeito
#' @export
palette_okabeito_black <-
  c(
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7",
    "#000000"
  )


# a function that simply returns its argument
passthrough <- function(x)
  x

# a function that stops without creating an error message
stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

#' Interactively view cvd simulations of a figure or plot
#'
#' @param plot The plot or grid object to view
#' @examples
#' \dontrun{
#' library(ggplot2)
#' plot <- ggplot(iris, aes(Sepal.Length, fill=Species)) +
#'   geom_density(alpha = 0.7)
#' view_cvd(plot)
#' }
#' @importFrom colorspace interpolate_cvd_transform simulate_cvd
#' @importFrom ggplot2 last_plot
#' @export
view_cvd <- function(plot = last_plot()) {
  if (FALSE) {
    message(
      "Warning: You have open graphics devices.
      These will have to be closed before proceeding."
    )
    response <-
      readline(prompt = "Do you want to close all
               open graphics devices (y/n)? ")
    if (response == "y" | response == "Y") {
      # this is needed to get the output redirected to the shiny app
      grDevices::graphics.off()
      message(
        "Exiting view_cvd() and closing all graphics devices.
        Please run view_cvd() again."
      )
    }
    else {
      message("Exiting view_cvd() and leaving graphics devices open.")
    }
    stop_quietly()
  }
  cvdapp <-
    shiny::shinyApp(ui = cvdui(plot), server = cvdServer(plot))
  shiny::runApp(cvdapp)
}

cvdui <- function(plot) {
  shiny::shinyUI(
    shiny::pageWithSidebar(
      # application title
      shiny::headerPanel("Color-vision-deficiency simulation"),

      # sidebar panel, defined below
      cvdapp_sidebarPanel(),

      # main panel, defined below
      cvdapp_mainPanel()
    )
  )
}

cvdapp_sidebarpanel <- function() {
  # sidebar with controls to select the simulation choice
  shiny::sidebarPanel(
    shiny::selectInput(
      "variable",
      "Simulation type",
      list(
        "Desaturated",
        "Deutan (red/green)",
        "Protan (red/green)",
        "Tritan (blue/green)",
        "Original"
      )
    ),
    shiny::sliderInput(
      "sev",
      "Severity",
      min = 0,
      max = 1,
      value = .95
    )
  )
}


cvdapp_mainpanel <- function() {
  # Show the caption and plot of the requested variable against mpg
  shiny::mainPanel(shiny::h3(shiny::textOutput("caption")),

                   shiny::plotOutput("plot"))
}


cvdserver <- function(plot) {
  shiny::shinyServer(function(input, output) {
    # retrieve the simulation choise in a reactive expression since it is
    # shared by the output$caption and output$plot expressions
    simul_choice <- shiny::reactive({
      input$variable
    })

    # return the simulation option printing as a caption
    output$caption <- shiny::renderText({
      simul_choice()
    })

    # generate plot with modified colors
    output$plot <- shiny::renderPlot({
      # convert simulation choice into function call
      colfun <- switch(
        simul_choice(),
        `Desaturated` = function(c)
          desaturate(c, amount = input$sev),

        # here and below, precalculate matrix for increase
        # in speed for figures with many colors
        `Deutan (red/green)` = function(c) {
          cvd_transform <-
            interpolate_cvd_transform(colorspace::deutanomaly_cvd, input$sev)
          simulate_cvd(c, cvd_transform)
        },

        `Protan (red/green)` = function(c) {
          cvd_transform <-
            interpolate_cvd_transform(colorspace::protanomaly_cvd, input$sev)
          simulate_cvd(c, cvd_transform)
        },

        `Tritan (blue/green)` = function(c) {
          cvd_transform <-
            interpolate_cvd_transform(colorspace::tritanomaly_cvd, input$sev)
          simulate_cvd(c, cvd_transform)
        },

        passthrough
      )

      # draw the modified plot
      cur_dev <-
        grDevices::dev.cur() # this is needed to make shiny behave correctly
      grob <- edit_colors(plot, colfun = colfun)
      if (cur_dev > 1)
        grDevices::dev.set(cur_dev)

      grid::grid.draw(grob)
    })
  })
}

#' @rdname scale_okabeito
#' @export
#' @usage NULL
scale_colour_okabeito <- function(aesthetics = "colour", ...) {
  scale_okabeito(aesthetics, ...)
}

#' @rdname scale_okabeito
#' @export
#' @usage NULL
scale_color_okabeito <- scale_colour_okabeito

#' @rdname scale_okabeito
#' @export
#' @usage NULL
scale_fill_okabeito <- function(aesthetics = "fill", ...) {
  scale_okabeito(aesthetics, ...)
}

#' Okabe-Ito color scale
#'
#' This is a color-blind friendly, qualitative scale with eight
#' different colors. See [palette_okabeito] for details.
#' @param use_black If `TRUE`, scale includes black, otherwise includes gray.
#' @param order Numeric vector listing the order in which the
#' colors should be used. Default is 1:8.
#' @param darken Relative amount by which the scale should be
#' darkened (for positive values) or lightened (for negatice
#'   values).
#' @param alpha Alpha transparency level of the color. Default
#' is no transparency.
#' @param ... common discrete scale
#' parameters: `name`, `breaks`, `labels`, `na.value`,
#' `limits`, `guide`, and `aesthetics`.
#'  See [discrete_scale] for more details.
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point() + scale_color_okabeito()
#' ggplot(iris, aes(Sepal.Length, fill = Species)) +
#'   geom_density(alpha = 0.7) + scale_fill_okabeito(order = c(1, 3, 5))
#'
#' cowplot::plot_grid(
#'   gg_color_swatches(8) + scale_fill_okabeito(darken = 0.6),
#'   gg_color_swatches(8) + scale_fill_okabeito(darken = 0.4),
#'   gg_color_swatches(8) + scale_fill_okabeito(darken = 0.2),
#'   gg_color_swatches(8) + scale_fill_okabeito(darken = 0),
#'   gg_color_swatches(8) + scale_fill_okabeito(darken = -0.2),
#'   gg_color_swatches(8) + scale_fill_okabeito(darken = -0.4),
#'   gg_color_swatches(8) + scale_fill_okabeito(darken = -0.6), ncol = 1)
#' @export
#' @usage NULL
scale_okabeito <-
  function(aesthetics,
           use_black = FALSE,
           order = 1:8,
           darken = 0,
           alpha = NA,
           ...) {
    if (use_black) {
      values <- palette_okabeito_black[order]
    }
    else {
      values <- palette_okabeito[order]
    }

    n <- length(values)
    darken <- rep_len(darken, n)
    alpha <- rep_len(alpha, n)

    di <- darken > 0
    if (sum(di) > 0) {
      # at least one color needs darkening
      values[di] <-
        colorspace::darken(values[di], amount = darken[di])
    }

    li <- darken < 0
    if (sum(li) > 0) {
      # at least one color needs lightening
      values[li] <-
        colorspace::lighten(values[li], amount = -1 * darken[li])
    }

    ai <- !is.na(alpha)
    if (sum(ai) > 0) {
      # at least one color needs alpha
      values[ai] <- scales::alpha(values[ai], alpha[ai])
    }

    pal <- function(n) {
      if (n > length(values)) {
        warning(
          "Insufficient values in manual scale. ",
          n,
          " needed but only ",
          length(values),
          " provided.",
          call. = FALSE
        )
      }
      values
    }
    ggplot2::discrete_scale(aesthetics, "manual", pal, ...)
  }

cvd_grid2 <- function(plot = last_plot(), severity = 1) {
  p4 <- plot

  deut <- function(c)
    deutan(c, severity)
  p1 <- edit_colors(plot, deut)

  prot <- function(c)
    protan(c, severity)
  p2 <- edit_colors(plot, prot)

  trit <- function(c)
    tritan(c, severity)
  p3 <- edit_colors(plot, trit)

  cowplot::plot_grid(
    p4,
    p1,
    p2,
    p3,
    scale = 0.9,
    hjust = 0,
    vjust = 1,
    labels = c("Unmodified", "Deutanomaly", "Protanomaly", "Tritanomaly"),
    label_x = 0.01,
    label_y = 0.99,
    label_size = 12,
    label_fontface = "bold"
  )
}
