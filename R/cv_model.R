#' Return anova table to a norm way
#'
#' @description
#' \code{cv.model} Return anova cv
#' @param MET data: data: Loc, Rep, Cul, yield
#'
#' @examples
#' library(met)
#' data("maize")
#' head(maize)
#' dat = maize
#' head(dat)
#' str(dat)
#' mod = aov(yield ~ Cul*Loc + Loc:Rep, data=dat)
#' anova_tab(mod)
#' cv_model(mod)

cv_model = function (x) {
  suma2 <- sum(x$residual^2)
  gl <- x$df.residual
  promedio <- mean(x$fitted.values)
  return(sqrt(suma2/gl) * 100/promedio)
}
#
# set.seed("123")
# y1 = rnorm(50,3,1)
# y2 = rnorm(50,30,10)
# y3 = rnorm(50,30,10)
# y4 = rnorm(50,30,10)
# y5 = rnorm(50,30,10)
# dat <- data.frame(y1=y1,y2=y2,y3=y3,y4=y4,y5=y5)
# re = learnasreml::huizong(dat)
# library(xtable)
# library(flextable)
# m1 = xtable_to_flextable(xtable(re))
