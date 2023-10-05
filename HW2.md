HW2
================
Tongxi Yu
2023-10-02

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dplyr)
library(tidyr)
library(readxl)
```

``` r
pols_month_data = read_csv("./fivethirtyeight_datasets/pols-month.csv")
```

    ## Rows: 822 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, r...
    ## date (1): mon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
unemployment_data = read_csv("./fivethirtyeight_datasets/unemployment.csv")
```

    ## Rows: 68 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (13): Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
snp_data = read_csv("./fivethirtyeight_datasets/snp.csv")
```

    ## Rows: 787 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# Question 1

First, clean the data in pols-month.csv. Use separate() to break up the
variable mon into integer variables year, month, and day; replace month
number with month name; create a president variable taking values gop
and dem, and remove prez_dem and prez_gop; and remove the day variable.

``` r
pols_month_data = 
  janitor::clean_names(pols_month_data) |>
  separate(mon,c("year","month","day")) |>
  mutate( month = month.name[as.numeric(month)]) |>
  mutate(
    president = case_when(
      prez_dem == 1 ~ "dem",
      prez_gop == 1 ~ "gop",
    )
  )|>
  select(-prez_dem, -prez_gop, - day)
```

Second, clean the data in snp.csv using a similar process to the above.
For consistency across datasets, arrange according to year and month,
and organize so that year and month are the leading columns.

``` r
snp_data = janitor::clean_names(snp_data) |>
  separate(date,c("day","month","year")) |>
  mutate( month = month.name[as.numeric(month)]) |>
  relocate(c(year, month), .before = day) |>
  mutate(year = paste0("20", year)) |>
  select(-day)
```

Third, tidy the unemployment data so that it can be merged with the
previous datasets. This process will involve switching from “wide” to
“long” format; ensuring that key variables have the same name; and
ensuring that key variables take the same values.

``` r
unemployment_data = janitor::clean_names(unemployment_data) |>
  pivot_longer(
    cols = -year,
    names_to = "month",         
    values_to = "unemployment_rate", 
    names_prefix = ""
  )
month_name_mapping <- c(
  "jan" = "January",
  "feb" = "February",
  "mar" = "March",
  "apr" = "April",
  "may" = "May",
  "jun" = "June",
  "jul" = "July",
  "aug" = "August",
  "sep" = "September",
  "oct" = "October",
  "nov" = "November",
  "dec" = "December"
)
unemployment_data = 
  unemployment_data |>
  mutate(month = month_name_mapping[month])
```

Join the datasets by merging snp into pols, and merging unemployment
into the result.

``` r
merged_pols_snp <- merge(pols_month_data, snp_data, by = c('year', 'month'), all.x = TRUE)
final_merged_data <- merge(merged_pols_snp, unemployment_data, by = c('year', 'month'), all.x = TRUE)
```

Write a short paragraph about these datasets. Explain briefly what each
dataset contained, and describe the resulting dataset (e.g. give the
dimension, range of years, and names of key variables).

``` r
colnames(final_merged_data)
```

    ##  [1] "year"              "month"             "gov_gop"          
    ##  [4] "sen_gop"           "rep_gop"           "gov_dem"          
    ##  [7] "sen_dem"           "rep_dem"           "president"        
    ## [10] "close"             "unemployment_rate"

``` r
range(final_merged_data$year)
```

    ## [1] "1947" "2015"

The dataset “pols-month” comprises 822 observations and 9 variables,
providing insights into the political landscape in the United States.
The variables include “mon” (date of the count), indicators for whether
the president was Republican or Democratic (“prez_gop” and “prez_dem”).
The dataset “snp” consists of 787 observations and 2 variables, focusing
on the performance of the Standard & Poor’s stock market index (S&P).
Lastly, the dataset “unemployment” encompasses 68 observations with 13
variables. It tracks unemployment percentages for each month of the
year, from January to December, over multiple years. This merged dataset
has 985 observation of 12 variables and combines political, economic,
and stock market data from 1947 to 2015. The merged dataset contains
columns “year”, “month”, “gov_gop”, “sen_gop”, “rep_gop”, “gov_dem”,
“sen_dem”, “rep_dem”, “president”, “day” , “close”, “unemployment_rate”

# Question 2

``` r
trash_wheel_data <- read_excel("~/Desktop/P8105/HW2/202309 Trash Wheel Collection Data.xlsx",
                               sheet = "Mr. Trash Wheel")
```

    ## New names:
    ## • `` -> `...15`
    ## • `` -> `...16`

``` r
trash_wheel_data = janitor::clean_names(trash_wheel_data) |>
  mutate(
    homes_powered = weight_tons * 500 / 30
  ) |>
  drop_na(trash_wheel_data$dumpster) |>
  mutate(
    trash_wheel_info = "Mr. Trash Wheel"
         )
```

    ## Warning: Unknown or uninitialised column: `dumpster`.

``` r
professor_trash_df = read_excel("~/Desktop/P8105/HW2/202309 Trash Wheel Collection Data.xlsx",
                               sheet = "Professor Trash Wheel") |>
  janitor::clean_names() |>
  mutate(
    homes_powered = weight_tons * 500 / 30
  ) |>
  drop_na(dumpster) |>
  mutate(
    trash_wheel_info = "professor"
         )
```

``` r
Gwynnda_trash_df = read_excel("~/Desktop/P8105/HW2/202309 Trash Wheel Collection Data.xlsx",
                               sheet = "Gwynnda Trash Wheel") |>
  janitor::clean_names() |>
  mutate(
    homes_powered = weight_tons * 500 / 30
  ) |>
  drop_na(dumpster) |>
  mutate(
    trash_wheel_info = "Gwynnda"
         )
```

# Question 3

``` r
baseline_data = read_csv("~/Desktop/P8105/HW2/data_mci/MCI_baseline.csv", skip = 1, na = ".") |>
  janitor::clean_names() |>
  mutate(sex = ifelse(sex == 1, "Male", "Female"))|>
  mutate(apoe4 = ifelse(apoe4 == 1, "carrier", "non-carrier"))
```

    ## Rows: 483 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (6): ID, Current Age, Sex, Education, apoe4, Age at onset
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
no_baseline_removed = baseline_data |>
  filter(
    current_age <  age_at_onset | is.na(age_at_onset)
  )
```

How many participants were recruited, and of these how many develop MCI?
What is the average baseline age? What proportion of women in the study
are APOE4 carriers?

``` r
nrow(baseline_data)
```

    ## [1] 483

``` r
MCI_developed = baseline_data |>
    filter(
    (current_age <  age_at_onset)
  )

nrow(MCI_developed)
```

    ## [1] 93

``` r
mean(baseline_data$current_age)
```

    ## [1] 65.04679

``` r
female_APOE4_carrier <- baseline_data |>
  filter(
    sex == "Female" & apoe4 == "carrier"
  )
nrow(female_APOE4_carrier) / nrow(baseline_data)
```

    ## [1] 0.1304348

483 participants were recruited. 93 of these developed MCI. The average
baseline age is 65.05. 0.1304348 of women in the study are APOE4
carriers

``` r
mci_amyloid_data <- read_csv("data_mci/mci_amyloid.csv", skip = 1) |>
  janitor::clean_names() |>
  mutate(
    id = study_id
  )|>
  select(- study_id)
```

    ## Rows: 487 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (5): Baseline, Time 2, Time 4, Time 6, Time 8
    ## dbl (1): Study ID
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

The data set recorded time (in years) elapsed since the study baseline
to the visit where biomarker Amyloid \_ 42/40 ratio was measured

``` r
baseline_unique <- no_baseline_removed |>
  anti_join(
    mci_amyloid_data, by = "id"
  )
nrow(baseline_unique)
```

    ## [1] 8

``` r
amyloid_unique <- mci_amyloid_data |>
  anti_join(
    no_baseline_removed, by = "id"
  )
nrow(amyloid_unique)
```

    ## [1] 16

8 participants appear in only the baseline dataset and 16 participants
appear in only the amyloid dataset.

``` r
combined_data <- inner_join(no_baseline_removed,mci_amyloid_data, by = "id")
```

The resulting data set contains information for 471 participants that
appeared both in the demographic and biomarker datasets. The dataset
contains information about their demographic information and time (in
years) elapsed since the study baseline to the visit where biomarker
Amyloid \_ 42/40 ratio was measured.

``` r
file_path <- "/Users/yutongxi/Desktop/P8105/HW2/data_mci/combined_data.csv"
write_csv(combined_data, file = file_path)
```
