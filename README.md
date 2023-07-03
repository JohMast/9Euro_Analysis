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

And for each state:

<p align="center"><img src="https://github.com/JohMast/9Euro_Analysis/blob/main/Documentation/F1_overview_states.png"  width="400" ></p>

The data looks plausible, and there are no gaps! Only the year 2021 is complete from start to finish, though.

We also already see that for many states, there is a little extended peak in the summer of 2022, like a plateau or mesa. This gives a first indication of the effect we seek to investigate further.

## 4. Analysis

To isolate this effect, however, we must consider:

-   There seems to be a natural increase in activity in the summer months, even in the years preceding the availability of the ticket. Thus, the most straightforward approach is to compare this excess activity in the summer of 2022 (when the 9€-Ticket was available) to the excess activity in the summers of 2021, when it was not. Thus, we need to calculate a measure for all years, not just 2022.

-   There seems to be an increase between years, likely due to the fading effects of the pandemic. To compare across years, we need to take a relative measure, rather than an absolute one. As a baseline, we fit a linear regression between the months of May (before summer) and September (just after summer) and predict which values would be expected based on this linear trend. The actual values, of course, are higher than this trend. The mean relative increase (in percent) during a summer is the measure of this summer's **excess** activity.

Then it is just a matter of subtracting the excess activity in 2021 from the excess activity in 2022 to arrive at a heuristic measure for the effect of the 9€-Ticket.

The following figure illustrates the regression and calculation of the excess activity.

<p align="center"><img src="https://github.com/JohMast/9Euro_Analysis/blob/main/Documentation/F2_excess.png"  width="600" ></p>

The excess is roughly around 10%-20% which is higher than the previous year (2021) by several percentage points. That seems like a plausible results, as it roughly aligns with the information reported by Deutsche Bahn [-source-](https://www.rnd.de/politik/9-euro-ticket-deutsche-bahn-sagt-es-funktioniert-OYJHQEUV4ZCOTBAXJ2B3WCDMZM.html)

## 5. Visualize

<p align="center"><img src="https://github.com/JohMast/9Euro_Analysis/blob/main/Documentation/F3_excess_relative.png"  width="600" ></p>


We can see that the influence of summer is much stronger in some states than in others. In Berlin, little seems to change, with transit station visits increasing by around 5% only. It is completely different in Mecklenburg-Vorpommern, where activity is around 25% higher than expected. The additional increase in 2022 (presumably resulting from the 9€-Ticket), is shown by the error-bars. Is there a spatial pattern? Time to put the effect on a map!

<p align="center"><img src="https://github.com/JohMast/9Euro_Analysis/blob/main/Documentation/F4_excess_map.png"  width="600" ></p>

It is clear that substantial differences exist, however, there is no clear north-south or east-west gradient. The effect is only moderate in the largest states (Bavaria, Lower Saxony) and highest in the moderately large states (Saxony-Anhalt, Schleswig Holstein).

## 6. Insights

To sum up, we make the following observations:

-   In all states, there is a distinctive *summer effect* which creates excess transit station visits in June, July, and August. Likely, this is a result of school-holidays and associated journeys. Some states, such as Mecklenburg-Vorpommern, feel this summer effect much more strongly than others. A possible conclusion might be that in these states, transport is used more for leisure.

-   In all states, except Berlin and Hamburg, we find that Summer 2022 saw a much higher transit than expected, substantially exceeding the summer effect of the previous years. It is plausible that the increase is due to the availability of the 9€-Ticket. Maybe the public transportation network of large cities like Berlin and Hamburg is used mostly for local journeys like commuting, making the demand for public transport less elastic.

Altogether, it is clear that overall mobility increased in the summer of 2022. The goal of the 9€-Ticket scheme, which was to ensure citizens' mobility despite rising fuel costs, seems to have been reached. The data does not allow us to differentiate whether this is actually due to increased use of public transport or indirect effects. Identifying this, by cross-referencing with passenger counts, would be key to evaluating the desirability of future schemes with regard to sustainability goals.

Also, it is not possible to say how much mobility would have suffered without the scheme - it is likely that it still would have increased, as it did in the other months of 2022. However, the present analysis gives **strong indication that the 9€-Ticket increased transit activity.**

When evaluating all these effects, it is likely that the covid19 pandemic also played role in shaping transit use, either via policy changes or the effect of climate on the spread of the disease. Linking the mobility data to school holidays and changes in covid policy could give further insight and potentially isolate the different causes. That way, future schemes can be planned on a robust foundation.
