#' Return anova table to a norm way
#'
#' @description
#' \code{maize_met_analysis} Return anova and LSD for the MET analysis
#' @param MET data: data: Loc, Rep, Cul, yield
#'
#' @examples
#' library(met)
#' data(years_locs_dat)
#' dat = years_locs_dat
#' dd = dat[,c(3,4,2,1,6)]
#' setDF(dd)
#' dat = dd
#' head(dat)
#' str(dat)
#' maize_multiple_year_multiple_loc_analysis(dat,json=F)
#' maize_multiple_year_multiple_loc_analysis(dat)



maize_multiple_year_multiple_loc_analysis = function(dat,json=TRUE){
  suppressMessages(require(jsonlite))
  names(dat) = c("Year","Loc","Rep","Cul","yield")
  dat$Year.Loc.Rep = as.factor(paste0(dat$Year,dat$Loc,dat$Rep))
  dat$Loc.Cul = as.factor(paste0(dat$Loc,dat$Cul))
  dat$Year.Cul = as.factor(paste0(dat$Year,dat$Cul))
  mod = aov(yield ~ Year + Loc + Year.Loc.Rep + Cul + Loc.Cul + Year.Cul, data = dat)
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

  loc_lsd0.05 = LSD_test(mod,"Loc",alpha=0.05)$groups
  loc_lsd0.05_Name = rownames(loc_lsd0.05)
  loc_lsd0.05$Name = rownames(loc_lsd0.05)
  loc_lsd0.05 = loc_lsd0.05[,c(3,1,2)]

  loc_lsd0.01 = LSD_test(mod,"Loc",alpha=0.01)$groups
  loc_lsd0.01_Name = rownames(loc_lsd0.01)
  loc_lsd0.01$Name = rownames(loc_lsd0.01)
  loc_lsd0.01 = loc_lsd0.01[,c(3,1,2)]

  loc_lsd_value0.05 = LSD_value(mod,"Loc",alpha = 0.05);loc_lsd_value0.01 = LSD_value(mod,"Loc",alpha = 0.01)


  year_lsd0.05 = LSD_test(mod,"Year",alpha=0.05)$groups
  year_lsd0.05_Name = rownames(year_lsd0.05)
  year_lsd0.05$Name = rownames(year_lsd0.05)
  year_lsd0.05 = year_lsd0.05[,c(3,1,2)]

  year_lsd0.01 = LSD_test(mod,"Year",alpha=0.01)$groups
  year_lsd0.01_Name = rownames(year_lsd0.01)
  year_lsd0.01$Name = rownames(year_lsd0.01)
  year_lsd0.01 = year_lsd0.01[,c(3,1,2)]

  year_lsd_value0.05 = LSD_value(mod,"Year",alpha = 0.05);year_lsd_value0.01 = LSD_value(mod,"Year",alpha = 0.01)

  loc.cul_lsd0.05 = LSD_test(mod,"Loc.Cul",alpha = 0.05)$groups
  loc.cul_lsd0.05_Name = rownames(loc.cul_lsd0.05)
  loc.cul_lsd0.05$Name = rownames(loc.cul_lsd0.05)
  loc.cul_lsd0.05 = loc.cul_lsd0.05[,c(3,1,2)]

  loc.cul_lsd0.01 = LSD_test(mod,"Loc.Cul",alpha = 0.01)$groups
  loc.cul_lsd0.01_Name = rownames(loc.cul_lsd0.01)
  loc.cul_lsd0.01$Name = rownames(loc.cul_lsd0.01)
  loc.cul_lsd0.01 = loc.cul_lsd0.01[,c(3,1,2)]

  loc.cul_lsd_value0.05 = LSD_value(mod,"Loc.Cul",alpha = 0.05);loc.cul_lsd_value0.01 = LSD_value(mod,"Loc.Cul",alpha = 0.01)

  year.cul_lsd0.05 = LSD_test(mod,"Year.Cul",alpha = 0.05)$groups
  year.cul_lsd0.05_Name = rownames(year.cul_lsd0.05)
  year.cul_lsd0.05$Name = rownames(year.cul_lsd0.05)
  year.cul_lsd0.05 = year.cul_lsd0.05[,c(3,1,2)]

  year.cul_lsd0.01 = LSD_test(mod,"Year.Cul",alpha = 0.01)$groups
  year.cul_lsd0.01_Name = rownames(year.cul_lsd0.01)
  year.cul_lsd0.01$Name = rownames(year.cul_lsd0.01)
  year.cul_lsd0.01 = year.cul_lsd0.01[,c(3,1,2)]

  year.cul_lsd_value0.05 = LSD_value(mod,"Year.Cul",alpha = 0.05);year.cul_lsd_value0.01 = LSD_value(mod,"Year.Cul",alpha = 0.01)

  re = c(aa,cul_lsd0.05,cul_lsd0.01,cul_lsd_value0.05,cul_lsd_value0.01,
         loc_lsd0.05,loc_lsd0.01,loc_lsd_value0.05,loc_lsd_value0.01,
         loc.cul_lsd0.05,loc.cul_lsd0.01,loc.cul_lsd_value0.05,loc.cul_lsd_value0.01,
         year_lsd0.05,year_lsd0.01,year_lsd_value0.05,year_lsd_value0.01,
         year.cul_lsd0.05,year.cul_lsd0.01,year.cul_lsd_value0.05,year.cul_lsd_value0.01,
         cv)

  if(json){
    re = lapply(re, toJSON)
  }else{
    re = list(aa,cul_lsd0.05,cul_lsd0.01,cul_lsd_value0.05,cul_lsd_value0.01,
              loc_lsd0.05,loc_lsd0.01,loc_lsd_value0.05,loc_lsd_value0.01,
              loc.cul_lsd0.05,loc.cul_lsd0.01,loc.cul_lsd_value0.05,loc.cul_lsd_value0.01,
              year_lsd0.05,year_lsd0.01,year_lsd_value0.05,year_lsd_value0.01,
              year.cul_lsd0.05,year.cul_lsd0.01,year.cul_lsd_value0.05,year.cul_lsd_value0.01,
              cv)
  }
  return(re)
}

