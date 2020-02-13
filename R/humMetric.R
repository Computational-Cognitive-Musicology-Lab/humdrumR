#' @name humMetric
#' @export 
setClass('humMeter', contains = 'humdrumVector',
         slots = c(N = 'list', 
                   D = 'rhythmInterval')) -> humMeter