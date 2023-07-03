library(tidyverse)
library(zoo)
library(sf)
library(fuzzyjoin)
Sys.setlocale("LC_ALL","English")
options(timeout=360)
# Download data
# Google LLC "Google COVID-19 Community Mobility Reports".
# https://www.google.com/covid19/mobility/ Accessed: <01. July 2023>.

mobility_report_path <- "../Case_Study/Data/Global_Mobility_Report.csv"
de_polygon_path <- "../Case_Study/Data/gadm_1_germany.rds"
if(!file.exists(mobility_report_path)){
  download.file("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv",
                destfile = mobility_report_path)
}

if(!file.exists(de_polygon_path)){
  download.file("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_DEU_1_sf.rds",
                destfile = de_polygon_path)
}

# load and keep only entries for germany
mobility_de <- read_csv(mobility_report_path) |> filter(country_region_code=="DE")

mobility_de <- 
  mobility_de |> 
  # the change from baseline can also be considered an index, where the value from previous years is a baseline
  mutate(transit_index = transit_stations_percent_change_from_baseline + 100) |> 
  # prepare time parameters
  mutate(date = ymd(date),
         doy = yday(date),
         month = month(date),
         year = year(date),
         ticket = (month %in% c(6,7,8) & year == 2022),
         # calculate rolling 7day average
         rollmean_transit_index = rollmeanr(transit_index, 7,na.pad=T,align="center")) 

# load polygons for germany
sf_de <- 
  read_rds(de_polygon_path) |> 
  # create the iso_3166_2_code from the HASC_1 code, which is in this case the same
  # except for Brandenburg, where we have to make a manual adjustment
  mutate(iso_3166_2_code = str_replace(HASC_1,"\\.","-"),
         iso_3166_2_code = ifelse(HASC_1=="DE.BR","DE-BB",iso_3166_2_code))|> 
  # add the sub_region_name via the iso_3166_2_code 
  left_join(mobility_de |> count(sub_region_1,iso_3166_2_code)|> drop_na())


# mobility_de |> 
#   # remove aggregated data for whole of germany
#   filter(is.na(sub_region_1)) |>
#   ggplot(aes(date,rollmean_transit_index,xend = lead(date), yend = lead(rollmean_transit_index)))+
#   geom_rect(xmin = ymd("2022-06-01"),xmax=ymd("2022-08-31"),ymin=-Inf,ymax=Inf,fill="#ffe4ab",inherit.aes=FALSE)+
#   geom_rect(xmin = ymd("2021-06-01"),xmax=ymd("2021-08-31"),ymin=-Inf,ymax=Inf,fill="#bdccff",inherit.aes=FALSE)+
#   geom_rect(xmin = ymd("2020-06-01"),xmax=ymd("2020-08-31"),ymin=-Inf,ymax=Inf,fill="#bde1ff",inherit.aes=FALSE)+
#   geom_segment()+
#   # geom_point(aes(col=ticket))+
#   theme_bw()

#
## comparing years

mobility_de |> 
  # remove aggregated data for whole of germany
  filter(is.na(sub_region_1)) |> 
  mutate(year=factor(year)) |> 
  ggplot(aes(doy,rollmean_transit_index,col=year,group=year))+
  geom_line()+
  # geom_point(aes(col=ticket))+
  theme_bw()+
  scale_color_manual(values=c("#249cff","#246bff","#ffb824"))+
  labs(title = "Transit station visits",
       caption = "Data by Google LLC",
       y="Transit index",x="Day of Year")

ggsave("../Case_Study/Documentation/F1_overview.png",width = 7,height = 7)
ggsave("../Case_Study/Documentation/F1_overview.svg",width = 7,height = 7)


# function which calculates summer excess from a linearly interpolated baseline
divergence_from_lin <- function(df){
  linear_model <- lm(formula = rollmean_transit_index~doy,
                     data = df |> filter(month %in% c(5,9)))
  df$lin_pred <- predict(linear_model,newdata = df)
  df$excess <- df$rollmean_transit_index - df$lin_pred 
  df$excess_rel <- df$excess/df$lin_pred
  return(df)
}

mobility_de <- 
  mobility_de |> 
  # remove aggregated data for whole of germany
  # filter(is.na(sub_region_1)) |> 
  # filter(month %in% c(5,6,7,8,9))|> 
  mutate(year=factor(year)) |> 
  group_by(year,sub_region_1) |> 
  group_map(~divergence_from_lin(.x),.keep=TRUE) |> 
  bind_rows()

mobility_de |> 
  filter(is.na(sub_region_1)) |> 
  ggplot(aes(doy,rollmean_transit_index,col=year,group=year,fill=year))+
  geom_line()+
  geom_ribbon(aes(x=doy, ymax=rollmean_transit_index, ymin=lin_pred),data = mobility_de |> filter(month %in% c(6,7,8),is.na(sub_region_1)), alpha=.3)+
  geom_line(aes(doy,lin_pred),data = mobility_de |> filter(month %in% c(5,6,7,8,9),is.na(sub_region_1)),col="black")+
  facet_wrap(~year,ncol=1)+
  theme_bw()+
  theme(legend.position = "none")+
  scale_color_manual(values=c("#249cff","#246bff","#ffb824"))+
  scale_fill_manual(values=c("#249cff","#246bff","#ffb824"))+
  labs(title = "Transit station visits",
       subtitle = "The highlighted area indicates the excess visits during June - August\ncompared to a linear regression between May and September",
       caption = "Data by Google LLC",
       y="Transit index",x="Day of Year")
ggsave("../Case_Study/Documentation/F2_excess.png",width = 7,height = 7)
ggsave("../Case_Study/Documentation/F2_excess.svg",width = 7,height = 7)

excess_stats <- 
  mobility_de |> 
  filter(month %in% c(6,7,8)) |> 
  group_by(year,sub_region_1) |> 
  summarise(excess=sum(excess),
            excess_rel=mean(excess_rel)) |> 
  group_by(sub_region_1) |> 
  mutate(excess_rel_prev_year = lag(excess_rel),
         excess_rel_over_prev_year = excess_rel-excess_rel_prev_year,
         excess_rel_over_prev_year_label = paste0(" + ", round(excess_rel_over_prev_year*100)," %"))

excess_stats |> 
  filter(!is.na(sub_region_1)) |> 
  ggplot(aes(year,excess_rel,fill=year))+
  geom_col()+  
  geom_errorbar(aes(ymin=excess_rel_prev_year,ymax=excess_rel),data=excess_stats |> filter(year==2022,!is.na(sub_region_1)))+
  geom_text(aes(y= excess_rel+0.05,label=excess_rel_over_prev_year_label),data=excess_stats |> filter(year==2022,!is.na(sub_region_1)))+
  facet_wrap(~sub_region_1) + 
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x = element_blank())+
  scale_y_continuous(labels=scales::label_percent())+
  scale_fill_manual(values=c("#249cff","#246bff","#ffb824"))+
  labs(title = "Excess transit station activity during summer months",
       caption = "Data by Google LLC",
       y="Excess transit station activity relative to May and September")
ggsave("../Case_Study/Documentation/F3_excess_relative.png",width = 7,height = 7)
ggsave("../Case_Study/Documentation/F3_excess_relative.svg",width = 7,height = 7)


# Map

excess_stats 

sf_de  |>
  # add the excess stats via the sub_region_name
  left_join(excess_stats |> filter(year==2022,!is.na(sub_region_1)),by = "sub_region_1") |> 
  ggplot(aes(fill=excess_rel_over_prev_year,label=excess_rel_over_prev_year_label))+
  geom_sf()+ 
  geom_sf_label(size=2.5)+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())+
  scale_fill_gradient(low="gray80",high="#ffb824")+
  labs(title = "Effect of 9€-Ticket on transit station activity",
       subtitle = "Percentage-points increase in transit station activity in summer 2022\nover the increase in summer 2021",
       caption = "Data by Google LLC")
ggsave("../Case_Study/Documentation/F4_excess_map.png",width = 7,height = 7)
ggsave("../Case_Study/Documentation/F4_excess_map.svg",width = 7,height = 7)


# by weekdays


# # by week
# mobility_de |> 
#   # remove aggregated data for whole of germany
#   filter(is.na(sub_region_1)) |> 
#   # summarize by week
#   mutate(week = floor_date(date,"week")) |> 
#   group_by(week) |> summarize(transit_index = mean(transit_index)) |> 
#   ggplot(aes(week,transit_index))+
#   geom_rect(xmin = ymd("2022-06-01"),xmax=ymd("2022-08-31"),ymin=-Inf,ymax=Inf,fill="#fffed8",inherit.aes=FALSE)+
#   geom_line()+
#   theme_bw()
