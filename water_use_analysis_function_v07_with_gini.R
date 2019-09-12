# Th1is function is written in R. Th1e primary purpose of th1is function is to analyze (i)data,(ii) produce
# h1istogram, (iii) ch1eck wh1eth1ter th1e data is normally distributed or not and th1en (iV) transform (log10) th1e data to ch1eck
# if it fits a normal distribution or not; (v) eventually it will ch1eck th1e normality of th1e transformed data and (iv)finally
# create a log normal curve. Required data (e.g. observation no, mean, sd) to draw th1e log normal graph1 are automatically 
# transformed by th1e function.Th1e user must import th1e data into R (e.g using readxl package). Th1e imported
# dataset sh1ould not contain any non-neumeric ch1aracter. Th1e function can eleminate th1e rows with1 
# ZERO (0) values returning only th1e actual usage scenario.

# Th1is version adds 1 to th1e actual observed value to eleminate th1e (-)ve values Wh1en transforming data 
# to log10, th1e function

# Description of variables:
#   hh_col= population data
#   h1= the data set
#   file_name= name of the exported excel file
#   lab_h1is_x= label for x-axis in th1e h1istogram (e.g. gal or gal/day)
#   lab_h1is_main = Main h1eader for th1e h1istogram
#   qqmain= h1eader for qq plot
#   trans_log_main= Main h1eader for th1e log transformed graph1
#   log_norm_main= Main h1eader for log normal graph1
#need anoth1er variable for bin width(???)
#setting the working directory
setwd("D:/R/R/log_normal_tests")

library(readxl)
daily.use<- read_excel("DAILY_USE_R_111.xlsx")

analyze_water_use_v2<-function (hh_col,h,file_name,lab_his_x='',lab_his_main='',qqmain='', trans_log_main='', log_norm_main=''){   
  library (ggplot2)
  library (egg)
  library (dplyr)
  library (ggpubr)
  library (DescTools)
  library (sn)
  library(ggExtra)
  library(cowplot)
  library(scales)
  library(grid)
  library(fBasics)
  library(data.table)
  library(gridExtra)
  library (xlsx)
  library(ineq)
  library(reldist)
  h1=data.frame(h)
  
  #Check for more efficient way to remove the 0 values
  #replace 0s with N/A s and then remove the NAs
  for (i in h1){
    if (0 %in% h1$h){
      
      #View(h1)
      #creating a data frame by eleminating th1e rows with1 zero (0) values
      
      h1.row_sub=apply(h1,1,function(row) all(row !=0))
      h1[h1.row_sub,]
      h1.ex0<-data.frame(h1[h1.row_sub,])
      names(h1.ex0)[1]<-"usage"
      #View(h1.ex0)
      
    }else{
      h1.ex0=data.frame(h)
      names(h1.ex0)[1]<-"usage"
      
    }
    #creating a data frame for the stat. summary.  A data frame b is being created which includes Mean, Max, Min, 1st and 3rd Quantile of the given data
    summary(h1.ex0$usage)
    b<-data.frame(unclass(summary(h1.ex0$usage)),check.names = TRUE, stringsAsFactors = TRUE) 
    names(b)[1]<-"values"
    b$para<-c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max")
    b<-b[c(2,1)] #to change th1e column index 
    
    #ploting a boxplot over the histogram using the original data set. It also includes the stat. summary
    p1 <- ggplot(h1.ex0, aes(x = usage)) + geom_histogram(bins = 30)+ theme_classic()+xlab(lab_his_x)
    p2 <- ggplot(h1.ex0, aes(x="", y=usage))+
      geom_boxplot() + coord_flip() +ggtitle(lab_his_main)+
      theme_classic()+theme(axis.title.x=element_blank(),axis.title.y=element_blank(), axis.text.x=element_blank(),axis.text.y=element_blank(),
                            axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),axis.line= element_blank())
    plt<-egg::ggarrange(p2,p1,heights=0.2:2)
    plt2<-ggdraw(plt)+draw_grob(tableGrob(b, rows=NULL),x=.3,y=-.1)
    print(plt2)
    
    
    #creating a qq plot to ch1eck th1e normality of th1e distribution of th1e data set
    qqnorm(h1.ex0$usage, pch = 1,main=qqmain,xlab = 'Theoretical quantiles', ylab = 'Sample quantiles of observed data', frame = FALSE, plot.it=TRUE)
    qqline(h1.ex0$usage, col='red')
    
    
    #transforming the data set to its ln to ch1eck if the transformed data has a normal distribution
    #counting the data that has value less than 1
    countValuesLessThan1<-sum(h1.ex0$usage<1)
    #all the data have been log transformed and plotted for histogram
    h1.ex0.log<<-log(h1.ex0)
    #View(h1.ex0.log)
    h1.ex0.ggplot<- ggplot(h1.ex0.log,aes(x=usage, fill=..count..))+geom_histogram()+ggtitle(trans_log_main)+xlab(lab_his_x)
    
    #all values less than 1 have been rounded up,and then log transformed and plotted for histogram
      #Replacing all data those are lower than 1 to 1
    h1.ex0.less1<- pmax(h1.ex0$usage,1) 
    h1.ex0.less1<-data.frame(h1.ex0.less1)
    names(h1.ex0.less1)[1]<-"usage"
    #View(h1.ex0.less1)
    countValuesLessThanEq1<-sum(h1.ex0.less1$usage<=1)
    #plotting the data
    h1.ex0.less1.log<<-log(h1.ex0.less1)
    #View(h1.ex0.less1.log)
    h1.ex0.less1.ggplot<- ggplot(h1.ex0.less1.log,aes(x=usage, fill=..count..))+geom_histogram()+ggtitle(paste(trans_log_main,"(values less than 1 has been upper rounded)"))+xlab(lab_his_x)
    #h1.ex0.ggplot_1<- ggplot(h1.ex0.log,aes(x=usage+1, fill=..count..))+geom_histogram()+ggtitle(paste(trans_log_main,"(adding 1 to the x axis label)"))+xlab(lab_his_x)
    
    #all values less than or equal to 1 have been removed and then rest of the data log transformed and plotted for histogram
     #Removing data less than or equal to 1
    
    h1.ex0.remove1<- h1.ex0[h1.ex0>1]
    h1.ex0.remove1<-data.frame(h1.ex0.remove1)
    names(h1.ex0.remove1)[1]<-"usage"
    #View(h1.ex0.remove1)
    countValuesLessThan1_afterRemoval<-sum(h1.ex0.remove1$usage>1)
    print(countValuesLessThan1_afterRemoval)
    
    #plotting the data
    h1.ex0.remove1.log<<-log(h1.ex0.remove1)
    #View(h1.ex0.remove1.log)
    h1.ex0.remove1.ggplot<- ggplot(h1.ex0.remove1.log,aes(x=usage, fill=..count..))+geom_histogram()+ggtitle(paste(trans_log_main,"(values less than 1 has been removed)"))+xlab(lab_his_x)
    #h1.ex0.ggplot_1<- ggplot(h1.ex0.log,aes(x=usage+1, fill=..count..))+geom_histogram()+ggtitle(paste(trans_log_main,"(adding 1 to the x axis label)"))+xlab(lab_his_x)
    
    print(h1.ex0.ggplot)    
    print(h1.ex0.less1.ggplot)
    print(h1.ex0.remove1.ggplot)
    
   
    
    #creating a qq plot for th1e transformed data
    t="transformed"
    #qq plot for all log transformed the data (including values less than 1)
    qqnorm(log(h1.ex0$usage), main=paste(qqmain,"after log transformation", sep = "  "),pch = 1,ylab = 'Transformed Sample quantiles of observed data(exponents)', frame = FALSE)
    qqline(log(h1.ex0$usage), col='red')
    #qq plot for modified data set where all the values less than 1 are rounded up and then log transformed
    qqnorm(log(h1.ex0.less1$usage), main=paste(qqmain,"after log transformation(rounding up values less than 1)", sep = "  "),pch = 1,ylab = 'Transformed Sample quantiles of observed data(exponents)', frame = FALSE)
    qqline(log(h1.ex0.less1$usage), col='red')
    #qq plot for modified data set where all the values less than or equal to 1 are removed and then log transformed
    qqnorm(log(h1.ex0.remove1$usage), main=paste(qqmain,"after log transformation(removed all data less than or eqal to 1)", sep = "  "),pch = 1,ylab = 'Transformed Sample quantiles of observed data(exponents)', frame = FALSE)
    qqline(log(h1.ex0.remove1$usage), col='red')
    
    
    #creating the log normal curve
    m<-mean(h1.ex0$usage)
    s<-sd(h1.ex0$usage)
    #using method of moments (see stedinger, J.R., 1980)
    location<-log(m^2/sqrt(s^2+m^2)) #the mean
    shape<-sqrt(log(1+(s^2/m^2)))    #the SD
    mod.log.norm.h1.ex0<<-rlnorm(length(h1.ex0$usage),location,shape)

    #png(file = "C://Users//Administrator//Documents//R//log_normal_tests//Images//function_test//log_normal_plot_1.png")
    plot((density(mod.log.norm.h1.ex0[mod.log.norm.h1.ex0<length(h1.ex0$usage)])), main=log_norm_main, xlab=lab_his_x, frame=FALSE)
    
    mod.log.norm.h1.ex0_df<- data.frame(mod.log.norm.h1.ex0)
    names(mod.log.norm.h1.ex0_df)[1]<- "random_variable"
    mod.log.norm.h1.ex0_df<<- data.frame(mod.log.norm.h1.ex0_df)
    #return( mod.log.norm.h1.ex0_df)
    #View(mod.log.norm.h1.ex0_df)
    

    
    #Plotting log normal curve over histogram
    # hist(h1.ex0$usage, freq=FALSE, breaks=300, main= "log normal plot over histogram", xlab=lab_his_x)
    # lines(density(mod.log.norm.h1.ex0_df$random_variable),col="red")
    
    plt13<-ggplot(h1.ex0,aes(usage))+geom_histogram(aes(y=..density..),bins = 300,col="blue",fill="green",alpha=.02)+ theme_classic()+xlab(lab_his_x)+geom_density(data=mod.log.norm.h1.ex0_df,aes(random_variable),col=2, adjust=3)+ggtitle("lognormal curve over histogram")#+geom_smooth(data=mod.log.norm.h1.ex0_df,aes(y=random_variable))
    print(plt13)
    
    # plt14<- ggplot(h1.ex0,aes(x=usage))+geom_histogram(aes(y=..density..))+geom_density(data=mod.log.norm.h1.ex0_df,aes(random_variable),col=2)+ggtitle("lognormal curve over histogram")
    # print (plt14)
    
    #gini coefficient
    
    #creating a data frame for gini coefficient calculation
    popu_data= data.frame(hh_col,h)
    names(popu_data)[2]<-"usage"
    #View(popu_data)
    #checking for cells with 0 values and removing them
    for (i in popu_data){
     if (0 %in% popu_data$usage){
       
       popu_data_1= apply(popu_data,1,function(row)all(row!=0))
       popu_data[popu_data_1,]
       popu_data<-data.frame(popu_data[popu_data_1,])
       
     }
      
       
    gini_df<- data.frame(popu_data)
    names(gini_df)[1]<-"hh_data"
    names(gini_df)[2]<-"usage"
    gini_df<- gini_df%>%group_by(gini_df$hh_data)%>% summarize_all(sum)
    gini_df$hh_data<-NULL
    names(gini_df)[1]<-"hh_keycode"
    #creating an additional column for House hold number
    gini_df$hh_no<-ifelse(gini_df$hh_keycode>0,1,0)
    #rearranging column index
    gini_df<-gini_df[c(1,3,2)]
    #creating the % columns for house hold and usage
    gini_df=mutate(gini_df,hh_pct=hh_no/sum(hh_no)*100,usage_pct=usage/sum(usage)*100)
    #ranking the data according to usage_pct
    gini_df<- gini_df[order(gini_df$usage_pct),]
    #creating cumulative % columns
    gini_df$cum_hh_pct<-cumsum(gini_df$hh_pct)
    gini_df$cum_usage_pct<-cumsum(gini_df$usage_pct)
    data_frame_gini<<-data.frame(gini_df)
    #View(gini_df)
    
    #plotting the data
    
    plt_gini<-ggplot(gini_df, aes(cum_usage_pct,cum_hh_pct))+geom_point()+xlab("Cum % usage")+ylab("cum % population")+ggtitle("Cum % Total population Vs Cum% use")
    print(plt_gini+ geom_smooth())
    gini_coeff<<-ineq(gini_df$cum_usage_pct,type="Gini")
    print(paste("gini coefficient for the",file_name,"=",ineq(gini_df$cum_usage_pct,type="Gini")))
    plot(Lc(gini_df$cum_usage_pct),col="darkred",lwd=2, xlab="cummulative population", ylab="cumulative usage",main=paste("lorenz curve for",file_name))
   
    gini(gini_df[which(gini_df$cum_hh_pct > 0), ]$cum_usage_pct)
    break}
    #gini coeff can be calculated for each hh
    
    
    
  }
  print(paste("Count of data those have value less than 1=",countValuesLessThan1))
  print(paste("Count of data those have value less than or equal to 1(after modifying the data)=",countValuesLessThanEq1))
  
  print (paste("Location Parameter for the random generated log normal values=",location))
  print (paste("Shape Parameter for the random generated log normal values=",shape))
  important_parameters<<-data.frame(location,shape,gini_coeff)
  #Exporting data to excel
  write.xlsx(h1.ex0.remove1.log,file=paste(file_name,"exported.xlsx"), sheetName="ModifiedDataAfterRemoving0", append=FALSE)
  write.xlsx(important_parameters,file=paste(file_name,"exported.xlsx"), sheetName="CalculatedParameters", append=TRUE)
  write.xlsx(mod.log.norm.h1.ex0_df,file=paste(file_name,"exported.xlsx"), sheetName="RandomGenVariableForlogNormal", append=TRUE)
  write.xlsx(data_frame_gini,file=paste(file_name,"exported.xlsx"), sheetName="GiniCalculationValues", append=TRUE)
  return()
  }

analyze_water_use_v2(daily.use$KEYCODE, daily.use$TOILET,"Toilet_use_data_1998","Gal/d","histogram for Toilet usage from 1998 study","qq plot for Toiletr usage from 1998 study","transformed histogram for Toilet usage","log normal curve for Toilet usage")
#dataT<-data.frame(mod.log.norm.h1.ex0)
#names(dataT)[1]<-"log_n_data"
#View(dataT)
#function pass back results
#try to return the output
