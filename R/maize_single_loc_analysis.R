#' Return anova table to a norm way
#'
#' @description
#' \code{maize_met_analysis} Return anova and LSD for the MET analysis
#' @param MET data: data: Loc, Rep, Cul, yield
#'
#' @examples
#' library(met)
#' data(single_loc)
#' dat = single_loc
#' maize_single_loc_analysis(dat)
#' maize_single_loc_analysis(dat,json=F)



maize_single_loc_analysis = function(dat,json=TRUE){
  suppressMessages(require(jsonlite))
  names(dat) = c("Rep","Cul","yield")
  dat$Rep = as.factor(dat$Rep)
  dat$Cul = as.factor(dat$Cul)

  mod = aov(yield ~ Rep + Cul , data = dat)
  aa = anova_tab(mod)

  nn = length(aa[[1]][,3])
  mse = aa[[1]][,3][(nn-1)]
  mean = mean(dat$yield,na.rm=TRUE)
  cv = round(sqrt(mse)*100/mean,4)

  cul_lsd0.05 = LSD_test(mod,"Cul",alpha = 0.05)$groups
  cul_lsd0.05_Name = rownames(cul_lsd0.05)
  cul_lsd0.05$Name =  rownames(cul_lsd0.05)
  cul_lsd0.05 = cul_lsd0.05[,c(3,1,2)]

  cul_lsd0.01 = LSD_test(mod,"Cul",alpha = 0.01)$groups
  cul_lsd0.01_Name = rownames(cul_lsd0.01)
  cul_lsd0.01$Name =  rownames(cul_lsd0.01)
  cul_lsd0.01 = cul_lsd0.01[,c(3,1,2)]

  cul_lsd_value0.05 = LSD_value(mod,"Cul",alpha = 0.05);cul_lsd_value0.01 = LSD_value(mod,"Cul",alpha = 0.01)


  re = c(aa,cul_lsd0.05,cul_lsd0.01,cul_lsd_value0.05,cul_lsd_value0.01,
         # loc_lsd0.05,loc_lsd0.01,loc_lsd_value0.05,loc_lsd_value0.01,
         # loc.cul_lsd0.05,loc.cul_lsd0.01,loc.cul_lsd_value0.05,loc.cul_lsd_value0.01,
         cv)

  if(json){
    re = lapply(re, toJSON)
  }else{
    re = list(aa,cul_lsd0.05,cul_lsd0.01,cul_lsd_value0.05,cul_lsd_value0.01,
              # loc_lsd0.05,loc_lsd0.01,loc_lsd_value0.05,loc_lsd_value0.01,
              # loc.cul_lsd0.05,loc.cul_lsd0.01,loc.cul_lsd_value0.05,loc.cul_lsd_value0.01,
              cv)
  }
  return(re)
}

