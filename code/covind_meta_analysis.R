
need_package <- c("meta", "tidyverse", "XLConnect", "lubridate")
have_package <- need_package %in% rownames(installed.packages())
if (any(have_package == FALSE)) {
  install.packages(need_package[!have_package])
}
library(XLConnect)
library(tidyverse)
library(meta)
 
#----DATA PREPARATION----#
meta_data<-readWorksheetFromFile("~/meta_data.xlsx", sheet=1, header=T)%>%
           mutate(authoryear=paste(name,year,sep=" "),
                  midpoint_date=start_date + floor((as.Date(end_date)-as.Date(start_date))/2),
                  midpoint_month=lubridate::month(midpoint_date),
                  authordate=paste(authoryear, " (", as.Date(end_date), ")", sep=""),
                  locationdate=paste(location, " (", format(as.Date(end_date), "%b %d %y"), ", ",name, ")", sep=""),
                  #convert percentages into proportions (i.e. decimal form)
                  seroprevalence=seroprevalence/100,
                  lower=lower/100,
                  upper=upper/100,
                  #create Age Adjusted Population variable
                  population=tot_population_millions*1000000*(age_proportion/100),
                  #create Estimated Total Cumulative Infections variable as denominator for ifr
                  total_estimated_cases=floor((population)*seroprevalence),
                  #create IFR1 variable as Reported Deaths divided by Estimated Total Cumulative Infections
                  ifr1=(deaths/total_estimated_cases),
                  #create lower bound of IFR1 95% confidence interval
                  lower_ifr1=ifr1-(1.96*sqrt((ifr1*(1-ifr1))/total_estimated_cases)),
                  #create upper bound of IFR1 95% confidence interval
                  upper_ifr1=ifr1+(1.96*sqrt((ifr1*(1-ifr1))/total_estimated_cases))) %>%
          mutate(
                  #add stars to denote which IFRs are precalculated; *=IFR1 precalc, **=IFR2 precalc, ***=IFR1 & IFR2 precalc.
                  locationdate=ifelse((is.na(ifr1_precalc)==F)&(is.na(ifr2_precalc)==T),paste(locationdate,"*",sep=""),
                                      ifelse((is.na(ifr1_precalc)==T)&(is.na(ifr2_precalc)==F),paste(locationdate,"**",sep=""),
                                             ifelse((is.na(ifr1_precalc)==F)&(is.na(ifr2_precalc)==F),paste(locationdate,"***",sep=""),
                                                    locationdate))),
                  #for studies with pre-computed IFR1 replace with directly provided IFR1 and 95% CIs
                  ifr1 = ifelse(is.na(ifr1_precalc)==F,ifr1_precalc/100,ifr1),
                  lower_ifr1 = ifelse(is.na(ifr1_precalc)==F,ifr1_precalc_lower/100,lower_ifr1),
                  upper_ifr1 = ifelse(is.na(ifr1_precalc)==F,ifr1_precalc_upper/100,upper_ifr1)) %>%
          mutate(
                  #create IFR2 variable as Reported Deaths*URF (D) divided by Estimated Total Cumulative Infections
                  ifr2_L=ifr1*death_urf1,
                  lower_ifr2_L=lower_ifr1*death_urf1,
                  upper_ifr2_L=upper_ifr1*death_urf1,
                  ifr2_U=ifr1*death_urf2,
                  lower_ifr2_U=lower_ifr1*death_urf2,
                  upper_ifr2_U=upper_ifr1*death_urf2)%>%
          mutate(
                 #for studies with pre-calculated IFR2 replace with directly provided IFR2 and 95% CIs
                 ifr2_L = ifelse(is.na(ifr2_precalc)==F,ifr2_precalc/100,ifr2_L),
                 lower_ifr2_L = ifelse(is.na(ifr2_precalc)==F,ifr2_precalc_lower/100,lower_ifr2_L),
                 upper_ifr2_L = ifelse(is.na(ifr2_precalc)==F,ifr2_precalc_upper/100,upper_ifr2_L),
                 ifr2_U = ifelse(is.na(ifr2_precalc)==F,ifr2_precalc/100,ifr2_U),
                 lower_ifr2_U = ifelse(is.na(ifr2_precalc)==F,ifr2_precalc_lower/100,lower_ifr2_U),
                 upper_ifr2_U = ifelse(is.na(ifr2_precalc)==F,ifr2_precalc_upper/100,upper_ifr2_U)) %>%
          mutate(
                 #create time windows
                 Time_Window = ifelse(as.Date(end_date)<as.Date('2020-07-01'),"From Jan 1 to Jun 30, 2020",
                                      ifelse((as.Date(end_date)>=as.Date('2020-07-01'))&(as.Date(end_date)<as.Date('2021-02-01')),"From Jul 1 2020 to Jan 31, 2021",
                                             ifelse((as.Date(end_date)>=as.Date('2021-02-01'))&(as.Date(end_date)<as.Date('2021-07-01')),"From Feb 1 to Jun 30, 2021",
                                                    ifelse(as.Date(end_date)>=as.Date('2021-07-01'),"From Jul 1 to Dec 31, 2021","NA")))),
                 #change to character in order to print 1 digit of URF in forest plots
                 death_urf1=as.character(round(death_urf1,digits=1)),
                 death_urf2=as.character(round(death_urf2,digits=1)))

#sort by end date of study time period
meta_data<-meta_data[order(meta_data$locationdate),]

#limit to studies with nationwide IFR estimates to be included in nationwide IFR analysis
data_ifr_overall <- meta_data %>%
  filter(include_ifr_overall==1)

#limit to studies with statewide, citywide, or districtwide IFR estimates to be included
#in regional IFR analysis
data_ifr_region <- meta_data %>%
  filter(include_ifr_region==1)

##Pool studies that occur in the same state (e.g., state, city, district) 
#to obtain one pooled IFR estimate per state
meta_ifr1 = metagen(log(ifr1),
                    lower=log(lower_ifr1),
                    upper=log(upper_ifr1),
                    byvar=State,
                    studlab=State,
                    method.tau="DL",
                    comb.random=T,
                    data=data_ifr_region)
meta_ifr1 <- update(meta_ifr1, sm="PLN")
meta_ifr2_L = metagen(log(ifr2_L),
                    lower=log(lower_ifr2_L),
                    upper=log(upper_ifr2_L),
                    byvar=State,
                    studlab=State,
                    method.tau="DL",
                    comb.random=T,
                    data=data_ifr_region)
meta_ifr2_L <- update(meta_ifr2_L, sm="PLN")
meta_ifr2_U = metagen(log(ifr2_U),
                      lower=log(lower_ifr2_U),
                      upper=log(upper_ifr2_U),
                      byvar=State,
                      studlab=State,
                      method.tau="DL",
                      comb.random=T,
                      data=data_ifr_region)
meta_ifr2_U <- update(meta_ifr2_U, sm="PLN")

#save pooled estimates as a data frame for later use in meta-analysis
meta.regionspld=data.frame(as.vector(unlist(summary(meta_ifr1)$bylevs)),
                           as.vector(unlist(summary(meta_ifr1)$within.random[c(1)])), 
                           as.vector(unlist(summary(meta_ifr1)$within.random[c(2)])),
                           as.vector(unlist(summary(meta_ifr1)$within.random[c(3)])),
                           as.vector(unlist(summary(meta_ifr1)$within.random[c(4)])),
                           as.vector(unlist(summary(meta_ifr2_L)$bylevs)),
                           as.vector(unlist(summary(meta_ifr2_L)$within.random[c(1)])), 
                           as.vector(unlist(summary(meta_ifr2_L)$within.random[c(2)])),
                           as.vector(unlist(summary(meta_ifr2_L)$within.random[c(3)])),
                           as.vector(unlist(summary(meta_ifr2_L)$within.random[c(4)])),
                           as.vector(unlist(summary(meta_ifr2_U)$bylevs)),
                           as.vector(unlist(summary(meta_ifr2_U)$within.random[c(1)])), 
                           as.vector(unlist(summary(meta_ifr2_U)$within.random[c(2)])),
                           as.vector(unlist(summary(meta_ifr2_U)$within.random[c(3)])),
                           as.vector(unlist(summary(meta_ifr2_U)$within.random[c(4)])))
names(meta.regionspld)=c('State', 'log_ifr1', 'se_ifr1', 'log_lower_ifr1','log_upper_ifr1',
                          'State2_L', 'log_ifr2_L', 'se_ifr2_L', 'log_lower_ifr2_L','log_upper_ifr2_L',
                          'State2_U', 'log_ifr2_U', 'se_ifr2_U', 'log_lower_ifr2_U','log_upper_ifr2_U')

#back transform log estimates by exponentiating 
data_ifr_region_pooled <- meta.regionspld %>%
  mutate(ifr1=exp(log_ifr1),
         lower_ifr1=exp(log_lower_ifr1),
         upper_ifr1=exp(log_upper_ifr1),
         ifr2_L=exp(log_ifr2_L),
         lower_ifr2_L=exp(log_lower_ifr2_L),
         upper_ifr2_L=exp(log_upper_ifr2_L),
         ifr2_U=exp(log_ifr2_U),
         lower_ifr2_U=exp(log_lower_ifr2_U),
         upper_ifr2_U=exp(log_upper_ifr2_U))%>%
  select(State, ifr1, lower_ifr1, upper_ifr1, 
                ifr2_L, lower_ifr2_L, upper_ifr2_L,
                ifr2_U, lower_ifr2_U, upper_ifr2_U)

#merge back on Region variable
region_lookup <- data_ifr_region %>%
  select(State, Region) %>%
  distinct(State, Region)
data_ifr_reg_pool<- left_join(data_ifr_region_pooled, region_lookup, by="State")


#---- META-ANALYSIS OF IFRs ----#
#examine whether proportion is normally distributed; note it is right skewed
#proceed with log transformation
hist(data_ifr_region$ifr1)
hist(log(data_ifr_region$ifr1))
hist(data_ifr_region$ifr2_L)
hist(log(data_ifr_region$ifr2_L))
hist(data_ifr_region$ifr2_U)
hist(log(data_ifr_region$ifr2_U))

##NATIONWIDE INDIA - IFR1
#transform to log scale
meta1a = metagen(log(ifr1),
                 lower=log(lower_ifr1),
                 upper=log(upper_ifr1),
                 studlab=locationdate,
                 method.tau="DL",
                 comb.random=T,
                 data=data_ifr_overall)
#transform back to non-log scale
meta1a <- update(meta1a, sm="PLN")
pdf(file = "forestplot_ifr1_india.pdf", width=8, height=10)
forest(meta1a,
       comb.fixed=F, prediction=T,
       digits=3,pscale=100,xlim=c(0,0.5),
       leftcols=c("locationdate"),
       sortvar=as.Date(end_date),
       rightcols=c("effect", "ci"),
       leftlabs=c("Study (End Date, First Author)"),
       rightlabs=c("IFR1 (%)", "[95% CI]"),
       smlab="National IFR1",
       col.study="blue", col.square="gray", col.square.lines="white",col.inside=("blue"), 
       col.random="brown", col.diamond="orange",col.diamond.lines="white", col.predict="green",
       fontfamily="serif")
dev.off()

##NATIONWIDE INDIA - IFR2 Low
#transform to log scale
meta1b = metagen(log(ifr2_L),
                 lower=log(lower_ifr2_L),
                 upper=log(upper_ifr2_L),
                 studlab=locationdate,
                 method.tau="DL",
                 comb.random=T,
                 subset=!(is.na(ifr2_L)),
                 data=data_ifr_overall)
#transform back to non-log scale
meta1b <- update(meta1b, sm="PLN")
pdf(file = "forestplot_ifr2_L_india.pdf", width = 10, height = 12)
grid.text("Nationwide IFR2 for India w Lower URF", .5, .9, gp=gpar(cex=2))
forest(meta1b,
       comb.fixed=F, prediction=T,
       digits=3,pscale=100,xlim=c(0,2),
       leftcols=c("locationdate", "death_urf1", "death_urf_ref1"),
       sortvar=as.Date(end_date),
       rightcols=c("effect", "ci"),
       leftlabs=c("Study (End Date, First Author)", "URF (D)", "URF Reference"),
       rightlabs=c("IFR2 (%)", "[95% CI]"),
       smlab="National IFR2 w Lower URF",
       col.study="blue", col.square="gray", col.square.lines="white",
       col.inside="blue", col.random="brown", col.diamond="orange",col.diamond.lines="white", col.predict="green",
       fontfamily="serif")
dev.off()

##NATIONWIDE INDIA - IFR2 High
#transform to log scale
meta1c = metagen(log(ifr2_U),
                 lower=log(lower_ifr2_U),
                 upper=log(upper_ifr2_U),
                 studlab=locationdate,
                 method.tau="DL",
                 comb.random=T,
                 subset=!(is.na(ifr2_U)),
                 data=data_ifr_overall)
#transform back to non-log scale
meta1c <- update(meta1c, sm="PLN")
pdf(file = "forestplot_ifr2_U_india.pdf", width = 10, height = 12)
forest(meta1c,
       comb.fixed=F, prediction=T,
       digits=3,pscale=100,xlim=c(0,2),
       sortvar=as.Date(end_date),
       rightcols=c("effect", "ci"),
       smlab="National IFR2 w Higher URF",
       leftcols=c("locationdate", "death_urf2", "death_urf_ref2"),
       leftlabs=c("Study (End Date, First Author)", "URF (D)", "URF Reference" ),
       rightlabs=c("IFR2 (%)", "[95% CI]"),
       col.study="blue", col.square="gray", col.square.lines="white",
       col.inside="blue", col.random="brown", col.diamond="orange",col.diamond.lines="white", col.predict="green",
       fontfamily="serif")
dev.off()


##REGIONAL INDIA - IFR1
#sort by state
data_ifr_reg_pool<-data_ifr_reg_pool[order(data_ifr_reg_pool$Region,data_ifr_reg_pool$State),]
#transform to log scale
meta2a = metagen(log(ifr1),
                 lower=log(lower_ifr1),
                 upper=log(upper_ifr1),
                 byvar=Region,
                 studlab=State,
                 method.tau="DL",
                 comb.random=T,
                 data=data_ifr_reg_pool)
#transform back to non-log scale
meta2a <- update(meta2a, sm="PLN")
pdf(file = "forestplot_ifr1_byregion.pdf", width = 8, height = 10)
forest(meta2a,
       comb.fixed=F, prediction=F, 
       comb.random=T, hetstat=T,
       overall=F,overall.hetstat=F,
       digits=3,pscale=100,xlim=c(0,1),
       smlab="Regional IFR1",
       rightcols=c("effect", "ci"),
       leftlabs=c("State"),
       rightlabs=c("IFR1 (%)", "[95% CI]"),
       leftcols=c("studlab"),
       col.study="blue", col.square="gray", col.square.lines="white",
       col.inside="blue", col.random="brown", col.diamond="orange",col.diamond.lines="white", col.predict="green",
       fontfamily="serif")
dev.off()

##REGIONAL INDIA - IFR2 Low
#transform to log scale
meta2b = metagen(log(ifr2_L),
                 lower=log(lower_ifr2_L),
                 upper=log(upper_ifr2_L),
                 byvar=Region,
                 studlab=State,
                 subset=!(is.na(ifr2_L)),
                 method.tau="DL",
                 comb.random=T,
                 data=data_ifr_reg_pool)
#transform back to non-log scale
meta2b <- update(meta2b, sm="PLN")
pdf(file = "forestplot_ifr2_L_byregion.pdf", width = 8, height = 10)
forest(meta2b,
       comb.fixed=F, prediction=F, 
       comb.random=T, hetstat=T,
       overall=F,overall.hetstat=F,
       digits=3,pscale=100,xlim=c(0,1.5),
       smlab="Regional IFR2 with Lower URF",
       leftcols=c("studlab"),
       leftlabs=c("State"),
       rightcols=c("effect", "ci"),
       rightlabs=c("IFR2 (%)", "[95% CI]"),
       col.study="blue", col.square="gray", col.square.lines="white",
       col.inside="blue", col.random="brown", col.diamond="orange",col.diamond.lines="white", col.porangeict="green",
       fontfamily="serif")
dev.off()

##REGIONAL INDIA - IFR2 High
#transform to log scale
meta2c = metagen(log(ifr2_U),
                 lower=log(lower_ifr2_U),
                 upper=log(upper_ifr2_U),
                 byvar=Region,
                 studlab=State,
                 subset=!(is.na(ifr2_U)),
                 method.tau="DL",
                 comb.random=T,
                 data=data_ifr_reg_pool)
#transform back to non-log scale
meta2c <- update(meta2c, sm="PLN")
pdf(file = "forestplot_ifr2_U_byregion.pdf", width = 8, height = 10)
forest(meta2c,
       comb.fixed=F, prediction=F, 
       comb.random=T, hetstat=T,
       overall=F,overall.hetstat=F,
       digits=3,pscale=100,xlim=c(0,1.5),
       smlab="Regional IFR2 with Higher URF",
       leftcols=c("studlab"),
       leftlabs=c("State"),
       rightcols=c("effect", "ci"),
       rightlabs=c("IFR2 (%)", "[95% CI]"),
       col.study="blue", col.square="gray", col.square.lines="white",
       col.inside="blue", col.random="brown", col.diamond="orange",col.diamond.lines="white", col.porangeict="green",
       fontfamily="serif")
dev.off()


##Supplementary Materials: seroprevalence estimates
#display seroprevalence estimates utilized across nationwide or regional analysis
data_all <- meta_data[order(meta_data$end_date),] %>%
  filter((include_ifr_overall==1)|(include_ifr_region==1))
metas1 = metagen(log(seroprevalence),
                 lower=log(lower),
                 upper=log(upper),
                 byvar=Time_Window,
                 subset=!(is.na(used_prev)),
                 studlab=locationdate,
                 method.tau="DL",
                 comb.random=T,
                 data=data_all)
#transform back to non-log scale
metas1 <- update(metas1, sm="PLN")
pdf(file = "forestplot_seroprev_bytime.pdf", width = 10, height = 12)
forest(metas1,
       comb.fixed=F, prediction=F, 
       comb.random=F, hetstat=F, 
       #print.byvar=T,
       overall=F,overall.hetstat=F,
       digits=2,pscale=100,xlim=c(0,70),
       rightcols=c("effect", "ci"),
       smlab="",
       leftcols=c("studlab"),
       leftlabs=c("Study (End Date, First Author)"),
       rightlabs=c("Seroprevalence (%)", "[95% CI]"),
       col.study="blue", col.square="gray", col.square.lines="white",
       col.inside="blue", col.random="brown", col.diamond="orange",col.diamond.lines="white", col.predict="green",
       fontfamily="serif")
dev.off()


##Examine publication bias among included studies
metas2 = metagen(log(ifr1),
                 lower=log(lower_ifr1),
                 upper=log(upper_ifr1),
                 byvar=Time_Window,
                 studlab=locationdate,
                 method.tau="DL",
                 comb.random=T,
                 data=data_all)
metas2 <- update(metas2, sm="PLN")
#funnel plot
pdf(file = "funnelplot.pdf")
funnel(metas2, comb.random = TRUE,
       level = 0.95, contour = c(0.9, 0.95, 0.99),
       col.contour = c("darkblue", "blue", "lightblue"),
       lwd = 2
       )
legend(0.05, 0.05,
       c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),
       fill = c("darkblue", "blue", "lightblue"))
dev.off()

#tests for funnel plot asymmetry
metas2_eggers<-metabias(metas2$TE, metas2$seTE,k.min=3,method.bias="Egger")
metas2_begg<-metabias(metas2$TE, metas2$seTE,method.bias="Begg")
print(metas2_eggers)
print(metas2_begg)
