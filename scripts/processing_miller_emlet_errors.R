# load libraries ----------------------------------------------------------
library(tidyverse)


#### Step 2: read in the data
data<-read_csv(file="raw_data/miller and emlet data.csv")

data%>%
  ggplot(aes(y=stage, x=age, col=temp)) + geom_point()
  
data %>%
  group_by(temp) %>%
  do(tidy(lm(stage~age, data=.))) %>%
  filter(., term=="age")

