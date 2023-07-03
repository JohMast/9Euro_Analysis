<p align="center"><img src="https://github.com/JohMast/9Euro_Analysis/blob/main/Documentation/Mobility_Changes_Summer_2022.png"  width="600" ></p>

# The effect of the 9€-Ticket on transit activity
## 1. Introduction

The 9€-Ticket was heavily discounted travel pass for German regional and local public transport. Tickets for each calendar month June, July, and August, were available for the comparatively low price of 9€ [-source-](https://www.bundesregierung.de/breg-de/aktuelles/faq-9-euro-ticket-2028756).

Most travelers agree that the use of regional trains increased a lot, leading in many cases to overcrowding, particularly on routes towards the coast. As DW reports, Deutsche Bahn recorded a 10% increase in people taking regional trains in June 2022 [-source-](https://www.dw.com/en/germanys-9-euro-travel-ticket-success-or-failure/a-62329405).

It is a different question, however, whether **overall** mobility, including other transport modes than regional trains, has been affected by the ticket.

In this case study, we will seek to answer this question, in the following forms:

-   Has the 9€-Ticket has had a noticeable effect on overall mobility, measured as an increase over the expected transit activity?
-   Are there regional differences in this effect?

This information can help evaluate and assess the effects of the scheme, and support the planning of similar schemes in the future.

## 2. Data

The [Community Mobility Reports by Google](https://www.google.com/covid19/mobility/) are a global data set on the change in visits to certain location types (transit stations, retail & recreation, parks, etc.) during the time period between February 2020 and October 2022.

To support research on the effects of the covid19 pandemic, Google made this dataset public and freely downloadable. Besides reports in pdf format, the raw data can be downloaded as tables in csv format, either by country and globally. These tables provide, for each day, location, and location type, the change in user visits *compared to a baseline*, which is the median of the corresponding weekdays in January 2020. Thus, absolute counts are not provided.

Regardless the data is a good fit for estimating the effect of the 9€-Ticket.

## 3. Preprocessing

To reduce the effect of within-week variation, we calculate the 7-day rolling average of transit station activity. Further, we set the baseline to 100 instead of 0 to ease visualization later on.

One further data cleaning step is necessary. While both the GADM and the mobility data includes a code for each state of Germany, the mobility data uses the [iso_3166_2\_code](https://www.iso.org/standard/63546.html) while the GADM polygons uses the [HASC_1](https://data.apps.fao.org/catalog/dataset/hasc-codes) format, which is similar, but not precisely the same. We manually adjust the codes to match, which will allow us to join the results to the map later.

Let's plot the data as yearly activity curves for all of Germany.

<p align="center"><img src="https://github.com/JohMast/9Euro_Analysis/blob/main/Documentation/F1_overview.png"  width="400" ></p>

```{r fig.height=3, fig.width=4}
mobility_de |> 
  # select aggregated data for whole of germany
  filter(is.na(sub_region_1)) |> 
  mutate(year=factor(year)) |> 
  ggplot(aes(doy,rollmean_transit_index,col=year,group=year))+
  geom_line()+
  theme_bw()+
  scale_color_manual(values=c("#249cff","#246bff","#ffb824"))+
  labs(title = "Transit station visits",
       caption = "Data by Google LLC",
       y="Transit index",x="Day of Year")
```

And for each federal state:

```{r}
mobility_de |> 
  # select aggregated data for whole of germany
  filter(!is.na(sub_region_1)) |> 
  mutate(year=factor(year)) |> 
  ggplot(aes(doy,rollmean_transit_index,col=year,group=year))+
  geom_line()+  theme_bw()+
  facet_wrap(~sub_region_1)+
  scale_color_manual(values=c("#249cff","#246bff","#ffb824"))+
  labs(title = "Transit station visits for each german state",
       caption = "Data by Google LLC",
       y="Transit index",x="Day of Year")
```

The data looks plausible, and there are no gaps! Only the year 2021 is complete from start to finish, though.

We also already see that for many states, there is a little extended peak in the summer of 2022, like a plateau or mesa. This gives a first indication of the effect we seek to investigate further.

## 4. Analysis

To isolate this effect, however, we must consider:

-   There seems to be a natural increase in activity in the summer months, even in the years preceding the availability of the ticket. Thus, the most straightforward approach is to compare this excess activity in the summer of 2022 (when the 9€-Ticket was available) to the excess activity in the summers of 2021, when it was not. Thus, we need to calculate a measure for all years, not just 2022.

-   There seems to be an increase between years, likely due to the fading effects of the pandemic. To compare across years, we need to take a relative measure, rather than an absolute one. As a baseline, we fit a linear regression between the months of May (before summer) and September (just after summer) and predict which values would be expected based on this linear trend. The actual values, of course, are higher than this trend. The mean relative increase (in percent) during a summer is the measure of this summer's **excess** activity.

Then it is just a matter of subtracting the excess activity in 2021 from the excess activity in 2022 to arrive at a heuristic measure for the effect of the 9€-Ticket.

```{r}

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
  mutate(year=factor(year)) |> 
  group_by(year,sub_region_1) |> 
  group_map(~divergence_from_lin(.x),.keep=TRUE) |> 
  bind_rows()

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

```

The following figure illustrates the regression and calculation of the excess activity.

```{r fig.height=5, fig.width=6}

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
       subtitle = "The highlighted area indicates the excess activity during June - August\ncompared to a linear regression between May and September",
       caption = "Data by Google LLC",
       y="Transit index",x="Day of Year")
```

We should check if the results are plausible.

Let's take a look at the excess activity for some states:

```{r}
DT::datatable(excess_stats |> filter(year==2022) |> mutate(across(where(is.numeric), round, 3)))
```

The excess is roughly around 10%-20% which is higher than the previous year (2021) by several percentage points. That seems like a plausible results, as it roughly aligns with the information reported by Deutsche Bahn [-source-](https://www.rnd.de/politik/9-euro-ticket-deutsche-bahn-sagt-es-funktioniert-OYJHQEUV4ZCOTBAXJ2B3WCDMZM.html)

## 5. Visualize

The numbers become much clearer in a figure.

```{r fig.height=7, fig.width=7}
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
```

We can see that the influence of summer is much stronger in some states than in others. In Berlin, little seems to change, with transit station visits increasing by around 5% only. It is completely different in Mecklenburg-Vorpommern, where activity is around 25% higher than expected. The additional increase in 2022 (presumably resulting from the 9€-Ticket), is shown by the error-bars. Is there a spatial pattern? Time to put the effect on a map!

```{r}
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
```

It is clear that substantial differences exist, however, there is no clear north-south or east-west gradient. The effect is only moderate in the largest states (Bavaria, Lower Saxony) and highest in the moderately large states (Saxony-Anhalt, Schleswig Holstein).

## 6. Insights

To sum up, we make the following observations:

-   In all states, there is a distinctive *summer effect* which creates excess transit station visits in June, July, and August. Likely, this is a result of school-holidays and associated journeys. Some states, such as Mecklenburg-Vorpommern, feel this summer effect much more strongly than others. A possible conclusion might be that in these states, transport is used more for leisure.

-   In all states, except Berlin and Hamburg, we find that Summer 2022 saw a much higher transit than expected, substantially exceeding the summer effect of the previous years. It is plausible that the increase is due to the availability of the 9€-Ticket. Maybe the public transportation network of large cities like Berlin and Hamburg is used mostly for local journeys like commuting, making the demand for public transport less elastic.

Altogether, it is clear that overall mobility increased in the summer of 2022. The goal of the 9€-Ticket scheme, which was to ensure citizens' mobility despite rising fuel costs, seems to have been reached. The data does not allow us to differentiate whether this is actually due to increased use of public transport or indirect effects. Identifying this, by cross-referencing with passenger counts, would be key to evaluating the desirability of future schemes with regard to sustainability goals.

Also, it is not possible to say how much mobility would have suffered without the scheme - it is likely that it still would have increased, as it did in the other months of 2022. However, the present analysis gives **strong indication that the 9€-Ticket increased transit activity.**

When evaluating all these effects, it is likely that the covid19 pandemic also played role in shaping transit use, either via policy changes or the effect of climate on the spread of the disease. Linking the mobility data to school holidays and changes in covid policy could give further insight and potentially isolate the different causes. That way, future schemes can be planned on a robust foundation.
