
# Creating risk of bias summary chart 
# R codes adapted from Harrer, M, Cuijpers, P, Furukawa, TA, & Ebert, DD (2019) 
# Doing Meta-Analysis in R: A Hands-on Guide. DOI: 10.5281/zenodo.2551803. 
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/)

rob.summary<-function(data){
  rob.vars<-data.frame(data)
  rob.vars$Author<-NULL
  ncol.rob.vars<-ncol(rob.vars)
  last<-colnames(rob.vars[ncol.rob.vars])
  first<-colnames(rob.vars[1])
  rob.long <- gather(data,
                     condition, measurement,
                     first:last,
                     factor_key=TRUE)
  rob.long$measurement<-as.factor(rob.long$measurement)
  rob.long$measurement<-factor(rob.long$measurement,
                               levels(rob.long$measurement)[c(1,3,2)])
  rob.plot<-ggplot(data=rob.long)+
    geom_bar(mapping=aes(x=condition,fill=measurement),
             width=0.7,
             position = "fill",
             color="black")+
    coord_flip(ylim = c(0,1))+
    guides(fill = guide_legend(reverse = TRUE))+
    scale_fill_manual("Risk of Bias",
                      labels = c("    High risk          ",
                                 "    Moderate risk       ",
                                 "    Low risk  "),
                      values = alpha( c("Moderate risk" = "#f1efd9",
                                        "High risk" = "#8eb3aa",
                                        "Low risk" = "#235f83"), 0.7) )+
    scale_y_continuous(labels = scales::percent)+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y = element_text(size=16, color = "black"),
          axis.line.x = element_line(colour = "black",
                                     size = 0.5, linetype = "solid"),
          legend.position = "bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.background = element_rect(linetype="solid",
                                           colour ="black"),
          legend.title = element_blank(),
          legend.key.size = unit(0.75,"cm"),
          legend.text=element_text(size=14))
  return(rob.plot)
}

spot.outliers.random <- function(data){
  data<-data
  Author<-data$studlab
  lowerci<-data$lower
  upperci<-data$upper
  m.outliers<-data.frame(Author,lowerci,upperci)
  te.lower<-data$lower.random
  te.upper<-data$upper.random
  dplyr::filter(m.outliers,upperci < te.lower)
  dplyr::filter(m.outliers,lowerci > te.upper)
}

influence.analysis <- function(data,method.tau,hakn){
  
  influence.data<-data
  TE<-data$TE
  seTE<-data$seTE
  method.tau<-method.tau
  hakn<-hakn
  
  if(hakn == TRUE){
    res <- rma(yi=TE, sei=seTE, measure="ZCOR", 
               data=influence.data, 
               method = paste(method.tau),
               test="knha")
    res
    inf <- influence(res)
    influence.data<-metainf(data)
    influence.data$I2<-format(round(influence.data$I2,2),nsmall=2)
    plot(inf)
    baujat(data)
    forest(influence.data,
           sortvar=I2,
           rightcols = c("TE","ci","I2"),
           smlab = "Sorted by I-squared")
    forest(influence.data,
           sortvar=TE,
           rightcols = c("TE","ci","I2"),
           smlab = "Sorted by Effect size")
    
  } else {
    
    res <- rma(yi=TE, sei=seTE, measure="ZCOR", 
               data=influence.data, 
               method = paste(method.tau))
    res
    inf <- influence(res)
    influence.data<-metainf(data)
    influence.data$I2<-format(round(influence.data$I2,2),nsmall=2)
    plot(inf)
    baujat(data)
    forest(influence.data,
           sortvar=I2,
           rightcols = c("TE","ci","I2"),
           smlab = "Sorted by I-squared")
    forest(influence.data,
           sortvar=TE,
           rightcols = c("TE","ci","I2"),
           smlab = "Sorted by Effect size")
  }}
