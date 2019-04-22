#' Return anova table to a norm way
#'
#' @description
#' \code{dikuai} Return anova and LSD for the MET analysis
#' @param MET data: data: Loc, Rep, Cul, yield
#'
#' @examples
#' library(met)
#' data(years_locs_dat)
#' dat = years_locs_dat
#' dd = dat[,c(3,4,2,1,6)]
#' setDF(dd)
#' dat = dd
#'
#'
#'
dikuai <- function(row,col,b_row,b_col,plot_col){
  cul = paste0("Cul_",1:(row*col/plot_col))
  n_block = (col/b_col)*(row/b_row)
  pp = b_col/plot_col
  te=NULL;dcol=NULL
  for(i in 1:(col/b_col)){
    x = (i-1)*pp+1
    y = i*pp
    te =c(rep(c(x:y,y:x),row%/%2),rep(x:y,row%%2))
    dcol = append(dcol,te)
  }
  drow = rep(c(1:row),each=(b_col/plot_col))
  block = rep(1:n_block,each=(b_row*(b_col/plot_col)))
  dat = data.frame(cul,drow,dcol,block)
  return(dat)
}
