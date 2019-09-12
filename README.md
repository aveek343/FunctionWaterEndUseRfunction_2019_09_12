# FunctionWaterEndUseRfunction_2019_09_12
the function was created to analyze any residential water end-use data to check its distribution and gini-coefficient. This function is written in R. The primary purpose of th1is function is to analyze (i)data,(ii) produce h1istogram, (iii) ch1eck wh1eth1ter th1e data is normally distributed or not and th1en (iV) transform (log10) th1e data to ch1eck if it fits a normal distribution or not; (v) eventually it will ch1eck th1e normality of th1e transformed data and (iv)finally create a log normal curve. Required data (e.g. observation no, mean, sd) to draw th1e log normal graph1 are automatically transformed by the function.Th1e user must import th1e data into R (e.g using readxl package). Th1e imported dataset sh1ould not contain any non-neumeric ch1aracter. Th1e function can eleminate th1e rows with ZERO (0) values returning only th1e actual usage scenario.

# Th1is version adds 1 to th1e actual observed value to eleminate th1e (-)ve values When transforming data

## Description of variables:
  hh_col= population data
  h1= the data set
  file_name= name of the exported excel file
  lab_h1is_x= label for x-axis in th1e h1istogram (e.g. gal or gal/day)
  lab_h1is_main = Main h1eader for th1e h1istogram
  qqmain= h1eader for qq plot
  trans_log_main= Main h1eader for th1e log transformed graph1
  log_norm_main= Main h1eader for log normal graph1
