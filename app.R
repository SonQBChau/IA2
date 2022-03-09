library(dash)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(tidyr)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

df <- readr::read_csv(here::here("data", "raw", "world-data-gapminder_raw.csv"))
df_non_na <- df %>%
    mutate(
        across(everything(), ~ replace_na(.x, 0))
    )

df_year_region <- df_non_na %>% 
    group_by(year, region) %>% 
    summarise(sum_co2_per_capita = sum(co2_per_capita))


app$layout(
    dbcContainer(
        list(
            dccGraph(id = "bar_chart"),
            dccSlider(
        id = "slider",
        min = 1914,
        max = 2014,
        step = 10,
        marks = list (
            "1914" = "1914",
            "1924" = "1924",
            "1934" = "1934",
            "1944" = "1944",
            "1954" = "1954",
            "1964" = "1964",
            "1974" = "1974",
            "1984" = "1984",
            "1994" = "1994",
            "2004" = "2004",
            "2014" = "2014"
        ),
        value = 1914,
      ),
            dccDropdown(
                id = "dropdown",
                options = c(
                    "Africa",
                    "Americas",
                    "Asia",
                    "Europe",
                    "Oceania"
                ),
                value = c("Americas", "Asia", "Europe", "Oceania", "Africa"),
                multi = TRUE
        )
    )
)
)

app$callback(
    output("bar_chart", "figure"),
    list(
        input("slider", "value"),
        input("dropdown", "value")
    ),
    function(selected_year, selected_regions) {
        p <- ggplot(df_year_region %>% 
        filter(year == selected_year, region %in% selected_regions )) +
        aes(y = reorder(region, sum_co2_per_capita,), x = sum_co2_per_capita,  fill = region) + 
            geom_bar(stat = "identity") + 
            labs(x = "CO2 emissions per capita", y = "Region")

        print(ggplotly(p) %>% layout())
        ggplotly(p) %>% layout()
    }
)


app$run_server(host = "0.0.0.0")
