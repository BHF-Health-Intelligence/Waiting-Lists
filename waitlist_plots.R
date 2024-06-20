library(ggplot2)
library(scales)
NHS_England_WL_DATES<-NHS_England_WL_DATES%>%
  filter(rtt_part != "Part_2A")
Total_plot <-ggplot(NHS_England_WL_DATES, aes(Date, total, color = rtt_description,group=rtt_description))+
    geom_point(size = 3) +
    geom_line(size = 2.6)+
  bhf_style()+
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE))+
    scale_color_bhf(palette = "red and light blue", name="Referral to Treatment (RTT) Part Description")+
    labs(title="NHS England Consultant-led Referral to Treatment Waiting Times Data from Apr 2019 to Apr 2024", y="Number of people",  x="Date")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size=14)) 

Total_plot