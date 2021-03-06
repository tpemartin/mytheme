#' Create a theme experiment instance for a ggplot
#'
#' @param ggObject A ggplot class object.
#'
#' @return An instance of environment
#' @export
#'
#' @examples
#' \dontrun{
#' drake$loadTarget$graph1_theme()
#' drake$loadTarget$graph1_theme()
#' graph1_theme
#' gg1themTry <- Theme(graph1_theme)
#' gg1themTry$economist_timeSeries$show()
#' gg1themTry$economist_timeSeries$adopt()
#' gg1themTry$.self
#' }
Theme <- function(ggObject){
  assertthat::assert_that(
    is(ggObject, "ggplot"),
    msg="The input argument should be a ggplot object."
  )
  gg_themeApply <- new.env()

  gg_themeApply$.self <- ggObject

  addThemesFun <- function(){}

  change_ggObjectFun <- function(ggObjNew){}

  economist_timeSeriesList <- list(
    show = showFun(gg_themeApply, economist_timeSeriesThm),
    adopt = adoptFun(gg_themeApply, economist_timeSeriesThm)
  )
  y_touchDownList <- list(
    show = showFun(gg_themeApply, y_touchDownThm),
    adopt = adoptFun(gg_themeApply, y_touchDownThm)
  )


  rlang::env_bind(
    .env = gg_themeApply,

    economist_timeSeries = economist_timeSeriesList,
    y_touchDown = y_touchDownList,
    export = exportFun,
    restore= restoreFun,
  )

  return(gg_themeApply)
}

# helpers -----------------------------------------------------------------
showFun <- function(instance, theme1){
  return(
    function(){
      instance$.self + theme1
    }
  )
}

adoptFun  <- function(instance, theme1){
  return(
    function(){
      instance$.backup <- instance$.self
      instance$.self + theme1 -> instance$.self
    }
  )
}

restoreFun  <- function(instance){
  return(
    function(){
      instance$.backup -> toSelf
      instance$.self -> toBackup
      toSelf -> instance$.self
      toBackup -> instance$.backup
    }
  )
}


exportFun  <- function(instance){
  return(
    function(){
        return(instance$.self)
    }
  )
}

# themes ------------------------------------------------------------------

economist_timeSeriesThm <-
  ggplot2::theme_classic() +
  ggplot2::theme(
    axis.line.y = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(
      color="#d8d8d8"
    ),
    axis.ticks.y = ggplot2::element_blank(),
  )

y_touchDownThm <- ggplot2::scale_y_continuous(
  expand = ggplot2::expansion(mult = 0, add = 0)
)
