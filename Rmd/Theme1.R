Theme <- function(ggObject){
  gg_themeApply <- new.env()
  
  gg_themeApply$.self <- ggObject
  
  exportFun <-  
    exportFun <- restoreFun <- addThemesFun <- function(){}
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

restore  <- function(instance){
  return(
    function(){
      instance$.backup -> toSelf
      instance$.self -> toBackup
      toSelf -> instance$.self
      toBackup -> instance$.backup
    }
  )
}


export  <- function(instance){
  return(
    function(){
        return(instance$.self)
    }
  )
}

# themes ------------------------------------------------------------------

economist_timeSeriesThm <- 
  theme_classic() +
  theme(
    axis.line.y = element_blank(),
    panel.grid.major.y = element_line(
      color="#d8d8d8"
    ),
    axis.ticks.y = element_blank(),
  )

y_touchDownThm <- scale_y_continuous(
  expand = expansion(mult = 0, add = 0)
)
  