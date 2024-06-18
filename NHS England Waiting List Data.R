# Project: NHS Waiting List Analysis - 2024
# Authors: Tom Almond and Lorna Robertson
# Script description: 
### This code downloads and cleans the RTT waiting list data from the NHS website, from the 2019/20 financial year up to the present day.
### It outputs an Excel/CSV file that can be used by other analysis/scripts to look at the RTT waiting list in England, at a national or commissioner level.

library(tidyverse)
library(paletteer)
library(curl)
library(readxl)
library(lubridate)
library(forcats)
library(ggtext)
library(devtools)
library(waffle)
library(RColorBrewer)
library(RcppRoll)
library(data.table)
library(readxl)
library(cowplot)
library(sf)
library(ggridges)
library(gridExtra)
library(patchwork)
library(scales)
library(readr)
library(xml2)
library(rvest)
library(flextable)
#library(xlsx)
library(readr)
library(janitor)
library(readxl)

# Step 1: Download the data for each financial year, by month

##### Financial year: 2023/24 ####
#24/25
#April 2024
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/06/Full-CSV-data-file-Apr24-ZIP-3855K-11417.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Apr_24 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

WL_24_25 <- rbind(Apr_24)
WL_24_25_national <- WL_24_25 %>%
  filter(treatment_function_code == "C_320") %>%
  group_by(period, rtt_part_type, rtt_part_description, treatment_function_code) %>%
  summarise(total = sum(total_all))

##23/24
#March 2024
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/05/Full-CSV-data-file-Mar24-ZIP-3832K-16220.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Mar_24 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Feb 2024
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/04/Full-CSV-data-file-Feb24-ZIP-3642K-08666.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Feb_24 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Jan 2024
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/03/Full-CSV-data-file-Jan24-ZIP-3702K-55749-1.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Jan_24 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")


# Dec 2023
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/02/Full-CSV-data-file-Dec23-ZIP-3569K-58522.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Dec_23 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Nov 2023
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/Full-CSV-data-file-Nov23-ZIP-3711K-61260.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Nov_23 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Oct 2023
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/12/Full-CSV-data-file-Oct23-ZIP-3928K-39245-1.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Oct_23 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Sep 2023
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/Full-CSV-data-file-Sep23-ZIP-3654K-revised.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Sep_23 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Aug 2023
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/Full-CSV-data-file-Aug23-ZIP-3646K-revised.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Aug_23 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# July 2023
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/Full-CSV-data-file-Jul23-ZIP-3623K-revised.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Jul_23 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# June 2023
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/08/Full-CSV-data-file-Jun23-ZIP-3659K-64970.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Jun_23 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# May 2023
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/Full-CSV-data-file-May23-ZIP-3627K-revised.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
May_23 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# April 2023
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/06/Full-CSV-data-file-Apr23-ZIP-3528K-36960.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Apr_23 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

WL_23_24 <- rbind(Apr_23, May_23, Jun_23, Jul_23, Aug_23, Sep_23, Oct_23, Nov_23, Dec_23, Jan_24, Feb_24, Mar_24)
WL_23_24_national <- WL_23_24 %>%
  filter(treatment_function_code == "C_320") %>%
  group_by(period, rtt_part_type, rtt_part_description, treatment_function_code) %>%
  summarise(total = sum(total_all))
    

#22/23


#March 2023
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/08/Full-CSV-data-file-Mar23-revised-ZIP-3589K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Mar_23 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Feb 2023
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/08/Full-CSV-data-file-Feb23-revised-ZIP-3784K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Feb_23 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Jan 2023
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/08/Full-CSV-data-file-Jan23-revised-ZIP-3609K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Jan_23 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")


# Dec 2022
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/08/Full-CSV-data-file-Dec22-revised-ZIP-3631K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Dec_22 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Nov 2022
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/08/Full-CSV-data-file-Nov22-revised-ZIP-3508K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Nov_22 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Oct 2022
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/08/Full-CSV-data-file-Oct22-revised-ZIP-3473K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Oct_22 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Sep 2022
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Full-CSV-data-file-Sep22-revised-ZIP-3542K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Sep_22 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Aug 2022
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Full-CSV-data-file-Aug22-revised-ZIP-3488K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Aug_22 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# July 2022
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Full-CSV-data-file-Jul22-revised-ZIP-3642K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Jul_22 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# June 2022
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Full-CSV-data-file-Jun22-revised-ZIP-4253.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Jun_22 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# May 2022
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Full-CSV-data-file-May22-revised-ZIP-4392K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
May_22 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")


##### Financial year: 2021/22 #####

# April 2022
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Full-CSV-data-file-Apr22-revised-ZIP-4253.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Apr_22 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

WL_22_23 <- rbind(Apr_22, May_22, Jun_22, Jul_22, Aug_22, Sep_22, Oct_22, Nov_22, Dec_22, Jan_23, Feb_23, Mar_23)
WL_22_23_national <- WL_22_23 %>%
  filter(treatment_function_code == "C_320") %>%
  group_by(period, rtt_part_type, rtt_part_description, treatment_function_code) %>%
  summarise(total = sum(total_all))

# Financial year 2021/22

#March 2022
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Full-CSV-data-file-Mar22-revised-ZIP-111805K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Mar_22 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Feb 2022
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Full-CSV-data-file-Feb22-revised-ZIP-109268K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Feb_22 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Jan 2022
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Full-CSV-data-file-Jan22-revised-ZIP-4266K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Jan_22 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")


# Dec 2021
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Full-CSV-data-file-Dec21-revised-ZIP-3744K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Dec_21 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Nov 2021
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Full-CSV-data-file-Nov21-revised-ZIP-3826K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Nov_21 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Oct 2021
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Full-CSV-data-file-Oct21-revised-ZIP-3787K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Oct_21 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Sep 2021
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Full-CSV-data-file-Sep21-revised-ZIP-3768K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Sep_21 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Aug 2021
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Full-CSV-data-file-Aug21-revised-ZIP-3682K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Aug_21 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# July 2021
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Full-CSV-data-file-Jul21-revised-ZIP-3710K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Jul_21 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# June 2021
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Full-CSV-data-file-Jun21-revised-ZIP-3739K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Jun_21 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# May 2021
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Full-CSV-data-file-May21-revised-ZIP-3656K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
May_21 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# April 2021
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Full-CSV-data-file-Apr21-revised-ZIP-3587K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Apr_21 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

WL_21_22 <- rbind(Apr_21, May_21, Jun_21, Jul_21, Aug_21, Sep_21, Oct_21, Nov_21, Dec_21, Jan_22, Feb_22, Mar_22)
WL_21_22_national <- WL_21_22 %>%
  filter(treatment_function_code == "C_320") %>%
  group_by(period, rtt_part_type, rtt_part_description, treatment_function_code) %>%
  summarise(total = sum(total_all))




##### Financial year: 2020/22 #####
#March 2021
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/05/Full-CSV-data-file-Mar21-ZIP-2888K-76325.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Mar_21 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Feb 2021
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/Full-CSV-data-file-Feb21-ZIP-2739K-25692.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Feb_21 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Jan 2021
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/03/Full-CSV-data-file-Jan21-ZIP-2714K-24158.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Jan_21 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")


# Dec 2020
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/Full-CSV-data-file-Dec20-revised-ZIP-2860K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Dec_20 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Nov 2020
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/01/Full-CSV-data-file-Nov20-ZIP-2758K-26885.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Nov_20 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Oct 2020
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/Full-CSV-data-file-Oct20-revised-ZIP-2772K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Oct_20 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Sep 2020
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/Full-CSV-data-file-Sep20-ZIP-2738K-20720.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Sep_20 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Aug 2020
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/10/Full-CSV-data-file-Aug20-ZIP-2594K-09869.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Aug_20 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# July 2020
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/09/Full-CSV-data-file-Jul20-ZIP-2546K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Jul_20 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# June 2020
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/Full-CSV-data-file-Jun20-revised-ZIP-2649K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Jun_20 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# May 2020
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/Full-CSV-data-file-May20-revised-ZIP-2216K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
May_20 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# April 2020
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/Full-CSV-data-file-Apr20-revised-ZIP-2295K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Apr_20 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

WL_20_21 <- rbind(Apr_20, May_20, Jun_20, Jul_20, Aug_20, Sep_20, Oct_20, Nov_20, Dec_20, Jan_21, Feb_21, Mar_21)
WL_20_21_national <- WL_20_21 %>%
  filter(treatment_function_code == "C_320") %>%
  group_by(period, rtt_part_type, rtt_part_description, treatment_function_code) %>%
  summarise(total = sum(total_all))


##### Financial year 2019/20 ####

#March 2020
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/05/Full-CSV-data-file-Mar20-ZIP-2995K-73640.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Mar_20 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Feb 2020
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Full-CSV-data-file-Feb20-revised-ZIP-3171K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Feb_20 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Jan 2020
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Full-CSV-data-file-Jan20-revised-ZIP-3227K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Jan_20 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")


# Dec 2019
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Full-CSV-data-file-Dec19-revised-ZIP-3109K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Dec_19 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Nov 2019
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Full-CSV-data-file-Nov19-revised-ZIP-3528K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Nov_19 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Oct 2019
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Full-CSV-data-file-Oct19-revised-ZIP-3580K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Oct_19 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Sep 2019
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/11/Full-CSV-data-file-Sep19-ZIP-3532K-62303.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Sep_19 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# Aug 2019
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/01/Full-CSV-data-file-Aug19-ZIP-3493K-revised.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Aug_19 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# July 2019
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/01/Full-CSV-data-file-Jul19-ZIP-3550K-revised.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Jul_19 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# June 2019
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/01/Full-CSV-data-file-Jun19-ZIP-3502K.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Jun_19 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# May 2019
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/01/Full-CSV-data-file-May19-ZIP-3497K-revised.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
May_19 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

# April 2019
link <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/01/Full-CSV-data-file-Apr19-ZIP-3436K-revised.zip"
temp <- tempfile()
temp <- curl_download(url=link, destfile=temp, quiet=FALSE, mode="wb")
Apr_19 <- read_csv(temp) %>%
  select(-contains(c("Gt", "Provider"))) %>%
  clean_names(case = "snake") %>%
  select(-c("total", "patients_with_unknown_clock_start_date")) %>%
  filter(commissioner_org_code != "NONC")

WL_19_20 <- rbind(Apr_19, May_19, Jun_19, Jul_19, Aug_19, Sep_19, Oct_19, Nov_19, Dec_19, Jan_20, Feb_20, Mar_20)
WL_19_20_national <- WL_19_20 %>%
  filter(treatment_function_code == "C_320") %>%
  group_by(period, rtt_part_type, rtt_part_description, treatment_function_code) %>%
  summarise(total = sum(total_all))



# Step 2: Merge the dataframes for each financial year

##### Merge national annual data frames together #####

NHS_England_WL <-rbind(WL_24_25_national, WL_23_24_national, WL_22_23_national, WL_21_22_national, WL_20_21_national, WL_19_20_national)









