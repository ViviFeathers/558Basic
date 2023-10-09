Cosmetic API Vignette
================
Vivi Feathers
2023-10-03

- [Requirement](#requirement)
- [API Interaction Functions](#api-interaction-functions)
  - [`clean_data`](#clean_data)
  - [`price_sum` and `rating_sum`](#price_sum-and-rating_sum)
  - [`pick_makeup`](#pick_makeup)
- [Exploratory Data Analysis (EDA)](#exploratory-data-analysis-eda)
  - [First call](#first-call)
    - [Two-way contingency table](#two-way-contingency-table)
    - [Stacked bar graph](#stacked-bar-graph)
    - [Price summary by cosmetic type.](#price-summary-by-cosmetic-type)
    - [Violin plot](#violin-plot)
    - [Scatter plots](#scatter-plots)
  - [Second call](#second-call)
    - [Second stacked bar graph](#second-stacked-bar-graph)
    - [Rating summary by product type.](#rating-summary-by-product-type)
    - [A box plot](#a-box-plot)
- [Wrap-up](#wrap-up)

This document is a vignette with the purpose of exploring how to read in
data using a function created for interacting with a specific API, as
well as showing different types of data analyses that can be conducted
on the data obtained.

I’ll be demonstrating with the [*MAKEUP
API*](http://makeup-api.herokuapp.com/) which contains cosmetic items’
name, brand, type, price and rating. I’m going to build a few functions
to interact with 2 endpoints: **product brand and product type**.

I will also summarize the statistics of price and rating by product type
with all the data available. After data are fetched and cleaned based on
the brand and/or type selected, I will merge the summaries with the
fetched data and compare each item’s price and rating verse price and
rating summaries from the corresponding product type, thus the user may
have some ideas about whether to choose this item or not.

The second part of the vignette focus on data exploration, I will
investigate cosmetic items’ brand, type, price and rating by creating
some contingency tables, numerical summaries and couple of plots.

# Requirement

The following packages were used to create this document:

- `tidyverse`: used for data manipulation, piping and visualization  
- `jsonlite`: used for interacting with JSON in the API and returning
  data in data frame  
- `knitr`: used to manage code in R Markdown format

# API Interaction Functions

## `clean_data`

I created a data cleaning function that removes confusing columns and
keep brand, name, product_type, price, currency, rating, description,
image_link columns. It also omits records with missing price or 0.0 as
price value, and removes carriage returns from the name and description
column. Additionally, it converts price to numeric values, rounds them
to two decimal places and standardizes them as US dollars according to
their currency name.(CAD x 0.73, GBP x 1.22, if currency is missing, I
take it as USD by default).

``` r
# create a data cleaning function that removes confusing columns, keep records with non-missing price, 
# removes carriage returns,converts price to numeric values, standardized them as US dollars according to 
# currency name.

clean_data <- function(df){
    df%>%
      select(brand, name, product_type, price, currency, rating, description, image_link) %>%
      filter (is.na(price) == FALSE & price != "0.0") %>%
      mutate(description = gsub("[\r\n]", "", description),
             name = gsub("[\r\n]", "", name),
             usd_price = if_else(is.na(currency) == TRUE, round(as.numeric(price)*1, digits =2),
                                 if_else(currency == "GBP", round(as.numeric(price)*1.22, digits =2),
                                         if_else(currency == "USD", round(as.numeric(price)*1, digits =2),
                                                 if_else(currency == "CAD", round(as.numeric(price)*0.73, digits =2), 0)))))
  }
```

## `price_sum` and `rating_sum`

I grabbed all the available data by calling the base URL in `fromJSON`
function. After cleaned the raw “all_type” data frame by using the
“clean_data” function. I summarized the price statistics (mean, median,
25th and 75th percentile) across product type. Then I removed the
records with missing rating, and calculated the rating statistics (mean,
median, 25th and 75th percentile) by product type as well, At the end, I
stored the results into two data frames: `price_sum` and `rating_sum`.

``` r
# use `fromJSON` function to get data frame from the base url and name it as "all_type"

all_type <- fromJSON("http://makeup-api.herokuapp.com/api/v1/products.json?")

# clean the raw "all_type" data frame by calling the "clean_data" function

all_type_clean <- clean_data(all_type)

#calculate the price statistics (mean, median, 25th and 75th percentile)

price_sum <- all_type_clean %>%
               group_by(product_type) %>%
               summarise(type_price_avg = mean(usd_price),
                         type_price_q1 = quantile(usd_price, probs = 0.25),
                         type_price_q3 = quantile(usd_price, probs = 0.75),
                         type_price_median = median(usd_price))
  
# remove the missing values from rating and calculate the rating statistics
  
rating_sum <- all_type_clean %>%
                filter(is.na(all_type_clean$rating) == FALSE) %>%
                group_by(product_type) %>%
                summarise(type_rate_avg = mean(rating),
                          type_rate_q1 = quantile(rating, probs = 0.25),
                          type_rate_q3 = quantile(rating, probs = 0.75),
                          type_rate_median = median(rating))
```

## `pick_makeup`

I created an API interaction function that fetches data based on user
selected cosmetic brand or/and product type.

Step one, I set up the base URL which returns all the available data,
the brand pool and the type pool to limit user’s selections in
“benefit”, “dior”, “covergirl”, “maybelline”, “smashbox”, “nyx”,
“clinique” as cosmetic brands and “lipstick”, “foundation”, “eyeliner”,
“eyeshadow”, “mascara”, “blush” as product types.

Step two, I checked the user’s argument input values to make sure both
makeup_brand and makeup_type are character strings that within the brand
and type pool, otherwise the function will stop executing. After making
sure the input values are good, I paste them with the base URL to form
the API with endpoints.

Step three, I used `fromJSON` function to obtain a data frame from the
“api_url” and name it as “target”.

Step four, I cleaned the raw “target” data frame by calling the
“clean_data” helper function, and stored the processed data frame in
“target_clean”.

Step five, I merged price and rating summaries from “price_sum” and
“rating_sum” data frames with “target_clean” data frame by product type.

Step six, I compared the price and rating of each observation from the
“target_clean” data frame vs the corresponding price summary and rating
summary from the same type products, and return “higher than”, “equal
to” and “less than” accordingly.

Step seven, I pasted the item price, product type, price/rating summary
and comparison results from step six together and output them into
“price_stat” and “rating_stat” column as price and rating status for
each observation.

Lastly, I returned the final data frame by keeping the necessary columns
as brand, name, product_type, usd_price, price_stat, rating,
rating_stat, description and image_link.

``` r
pick_makeup <- function(makeup_brand = NULL, makeup_type = NULL){
  
  # set up the base URL
  api_url <- "http://makeup-api.herokuapp.com/api/v1/products.json?"
  
  #set up brand and type option pool
  brand_base <- c("benefit", "dior", "covergirl", "maybelline", "smashbox", "nyx", "clinique")
  type_base <- c("lipstick", "foundation", "eyeliner", "eyeshadow", "mascara", "blush")
  
  # check the input values 
  if (!is.null(makeup_brand)){
    # makeup brand must be a characters string, if not, stop the function and return message.
    if (!is.character(makeup_brand)){
      stop("makeup brand must be a character string.")
    }
    # makeup brand must be selected from the brand pool, if not, stop the function and return message.
    makeup_brand <- tolower(makeup_brand)
    if (! (makeup_brand %in% brand_base)){
      stop("makeup brand must be selected from benefit, dior, covergirl, maybelline, smashbox, nyx and clinique.")
    }
    # if makeup_brand is not missing, it is a character string that from the brand pool, paste it into base url.
    api_url <- paste0(api_url, "brand=", makeup_brand)
  } # if makeup_brand is missing, leave "brand=" piece empty in the url 
  else{api_url <- paste0(api_url, "brand=")}
  
  if (!is.null(makeup_type)){
    # makeup type must be a characters string, if not, stop the function and return message.
    if (!is.character(makeup_type)){
      stop("makeup type must be a character string.")
    }
    # makeup type must be selected from the type pool, if not, stop the function and return message.
    makeup_type <- tolower(makeup_type)
    if (! (makeup_type %in% type_base)){
      stop("makeup type must be selected from lipstick, foundation, eyeliner, eyeshadow, mascara, blush.")
    }
    # if makeup_type is not missing, it is a character string that from the type pool, paste it into base url.
    api_url <- paste0(api_url, "&product_type=", makeup_type)
  }# if makeup_type is missing, the "api_url" remains unchanged
  
  # use `fromJSON` function to get data frame from the "api_url" we just set up and name it as "target"
  target <- fromJSON(api_url)
  
  # clean the raw "target" data frame by calling the "clean_data" function
  target_clean <- clean_data(target)
  
  # merge price and rating summaries with target data frame by product type and do the comparison
  
  target_price <- merge(target_clean, price_sum, by="product_type")
  target_price_rate <- merge( target_price, rating_sum, by="product_type")
  
  # compared the price and rating of each observation vs summaries from the same type products
  
  target2 <-  target_price_rate %>%
    mutate(comp_mean = if_else(usd_price > type_price_avg, "higher than",
                               if_else(usd_price < type_price_avg, "lower than",
                                       if_else(usd_price == type_price_avg, "equal to", ""))),
           comp_median = if_else(usd_price > type_price_median, "higher than",
                                 if_else(usd_price < type_price_median, "lower than",
                                         if_else(usd_price == type_price_median, "equal to", ""))),
           comp_q1 = if_else(usd_price > type_price_q1, "higher than",
                             if_else(usd_price < type_price_q1, "lower than",
                                     if_else(usd_price == type_price_q1, "equal to", ""))),
           comp_q3 = if_else(usd_price > type_price_q3, "higher than",
                             if_else(usd_price < type_price_q3, "lower than",
                                     if_else(usd_price == type_price_q3, "equal to", ""))),
           comp_rt_mean =  if_else(as.numeric(rating) > type_rate_avg, "higher than",
                                   if_else(as.numeric(rating) < type_rate_avg, "lower than",
                                           if_else(as.numeric(rating) == type_rate_avg, "equal to", ""))),
           comp_rt_median = if_else(as.numeric(rating) > type_rate_median, "higher than",
                                    if_else(as.numeric(rating) < type_rate_median, "lower than",
                                            if_else(as.numeric(rating) == type_rate_median, "equal to", ""))),
           comp_rt_q1 = if_else(as.numeric(rating) > type_rate_q1, "higher than",
                                if_else(as.numeric(rating) < type_rate_q1, "lower than",
                                        if_else(as.numeric(rating) == type_rate_q1, "equal to", ""))),
           comp_rt_q3 = if_else(as.numeric(rating) > type_rate_q3, "higher than",
                                if_else(as.numeric(rating) < type_rate_q3, "lower than",
                                        if_else(as.numeric(rating) == type_rate_q3, "equal to", ""))))
  
  # generate "price_stat" and "rating_stat" column by pasting the item price, product type, price/rating summary and comparison results
  
  target3 <- target2 %>%
    mutate(price_stat = paste0("This ", product_type, "'s price is $", usd_price, " which is ", comp_mean,
                               " the overall ", product_type, " price mean (", round(type_price_avg, digits=2), "), ", comp_median,
                               " the price median(", round(type_price_median, digits=2),"), ", comp_q1,
                               " the 25th percentile(", round(type_price_q1, digits=2),"), and ", comp_q3,
                               " the 75th percentile(", round(type_price_q3, digits=2),")."),
           rating_stat = if_else(is.na(rating)==TRUE, "No rating",
                                 paste0("This ", product_type, "'s rating is ", rating, " which is ", comp_rt_mean,
                                        " the overall ", product_type, " rating mean (", round(type_rate_avg, digits=2), "), ", comp_rt_median,
                                        " the rating median(", round(type_rate_median, digits=2),"), ", comp_rt_q1,
                                        " the 25th percentile(", round(type_rate_q1, digits=2),"), and ", comp_rt_q3,
                                        " the 75th percentile(", round(type_rate_q3, digits=2),").")))
  # keep necessary columns and return
  target_final <- target3 %>%
    select(brand, name, product_type, usd_price, price_stat, rating, rating_stat, description, image_link)
  return(target_final)
}
```

# Exploratory Data Analysis (EDA)

Now the API interaction function is done, I can explore some data
analysis.

## First call

First of all, let’s call the API function with default argument inputs
and return all the available data, then write a function `add_factor`
which converts cosmetic brand and product type as factors, that way it
will be better for data analysis.

``` r
# call the function and return all the data
all <- pick_makeup(makeup_brand = NULL, makeup_type = NULL)

# write a function to convert "brand" and "product_type" to factors
add_factor <- function(df_1) {
                    df_1 %>%
                       mutate(Cosmetic_Brand = as.factor(df_1$brand), Cosmetic_Type = as.factor(df_1$product_type))
}

all_factor <- add_factor(all)
```

### Two-way contingency table

Let’s use a `table` function and create a two-way contingency table and
break the item counts by brand and type.

``` r
two_way <- table( all_factor$Cosmetic_Type,all_factor$Cosmetic_Brand)
two_way
```

    ##              
    ##               almay alva anna sui annabelle benefit boosh burt's bees butter london cargo cosmetics china glaze
    ##   blush           1    0        1         1       0     0           0             0               3           0
    ##   bronzer         1    0        0         1       6     0           0             0               9           0
    ##   eyeliner        3    0        3         7       3     0           0             0               2           0
    ##   eyeshadow       2    1        0         0       0     0           0             0               1           0
    ##   foundation      3    0        0         1       2     0           0             0               2           0
    ##   lip_liner       0    0        0         1       0     0           0             0               1           0
    ##   lipstick        1    0        1         0      13     1           2             1               2           0
    ##   mascara         3    0        0         0       6     0           0             0               0           0
    ##   nail_polish     0    0        1         0       0     0           0             1               0           1
    ##              
    ##               clinique colourpop covergirl dalish deciem dior dr. hauschka e.l.f. essie fenty glossier l'oreal
    ##   blush              7         0         7      0      0    4            0      2     0     0        1       2
    ##   bronzer            5         0         2      0      0    1            3      5     0     0        0       1
    ##   eyeliner           9         0        12      0      0    6            2      4     0     1        0       9
    ##   eyeshadow         12         0         5      0      0   10            2      6     0     0        0       1
    ##   foundation        34         1        10      0      2    9            2      3     0     2        4       7
    ##   lip_liner          2         1         1      0      0    0            0      1     0     0        0       2
    ##   lipstick          17         2         4      1      0   15            2      3     0     2        1       7
    ##   mascara            0         0        12      0      0    8            1      3     0     0        0       8
    ##   nail_polish        0         0         1      0      0   13            0      0     4     0        0       9
    ##              
    ##               marcelle maybelline milani mineral fusion misa mistura moov nyx orly pacifica physicians formula
    ##   blush              1          4      2              1    0       0    0  12    0        1                  8
    ##   bronzer            2          3      1              1    0       0    0   9    0        1                  9
    ##   eyeliner           4          8      5              1    0       0    0  32    0        1                  9
    ##   eyeshadow          1          7      1              0    0       1    0   1    0        4                  3
    ##   foundation         2         10      1              2    0       0    0  32    0        0                  8
    ##   lip_liner          3          1      1              0    0       0    0  10    0        1                  0
    ##   lipstick           1          7      2              1    0       0    0  38    0        1                  0
    ##   mascara            1         11      0              1    0       0    0  11    0        2                  6
    ##   nail_polish        0          3      0              1    1       0    3   0    4        2                  0
    ##              
    ##               piggy paint pure anada revlon salon perfect sante sinful colours smashbox stila suncoat wet n wild
    ##   blush                 0          3      2             0     1              0        3     2       0          0
    ##   bronzer               0          1      0             0     0              0        2     1       0          0
    ##   eyeliner              0          1      4             0     1              0        6     1       1          5
    ##   eyeshadow             0          2      1             0     1              0        7     0       0          3
    ##   foundation            0          4      7             0     1              0        9     0       0          0
    ##   lip_liner             0          0      1             0     1              0        0     0       0          0
    ##   lipstick              0          1     11             0     0              0        6     0       0          2
    ##   mascara               0          1      0             0     0              0        6     0       1          1
    ##   nail_polish           1          3      3             1     1              1        0     0       4          1
    ##              
    ##               zorah
    ##   blush           0
    ##   bronzer         0
    ##   eyeliner        1
    ##   eyeshadow       0
    ##   foundation      0
    ##   lip_liner       0
    ##   lipstick        0
    ##   mascara         1
    ##   nail_polish     0

The two-way contingency tables shows that there are 44 cosmetic brands
and 9 product types. “covergirl”, “l’oreal” and “maybelline” are the
only 3 brands have products in all 9 types. “alva”, “boosh”, “burt’s
bees”, “china glaze”, “dalish”, “deciem”, “essie”, “misa”, “mistura”,
“moov”, “orly”, “piggy paint”, “salon perfect” and “sinful colours” only
have one type products in the database. “nyx” has the most items and
“alva”, “boosh”, “china glaze”, “dalish”, “misa”, “mistura”, “piggy
paint”, “salon perfect” and “sinful colours” only have one item.

### Stacked bar graph

I want to know the item count by product type. I will use `gglot` and
`geom_bar` to create a stacked bar graph to present item count for each
product type with cosmetic_type as x, also fill colors based on
different product type.

``` r
g <- ggplot(data = all_factor, aes(x = Cosmetic_Type, fill = Cosmetic_Type))
g + geom_bar(alpha = 0.6) +
  labs(x = "Cosmetic Type", y = "Item count", title = "Bar Plot of Item Count for Each Cosmetic Type") 
```

![](README_files/figure-gfm/graphics-1.png)<!-- -->

From the bar plot we can see, the blush, bronzer, eyeshadow, mascara and
nail polish product types each has around 60 to 80 items, the eyeliner
and the lipstick categories have around 40 item each. The foundation
category has the most items and the lip liner has the least products.

### Price summary by cosmetic type.

In order to know more about price, I will calculate the price mean,
standard deviation, variance, median, Q1 and Q3 across product types by
using a `group_by` and a `summarise` function.

``` r
all_factor %>%
  group_by(Cosmetic_Type) %>%
  summarise(Mean = mean(usd_price),  Standard_Deviation = sd(usd_price), 
            Variance = var(usd_price), Median = median(usd_price), 
            q1 = quantile(usd_price, probs = 0.25),
            q3 = quantile(usd_price, probs = 0.75))
```

    ## # A tibble: 9 × 7
    ##   Cosmetic_Type  Mean Standard_Deviation Variance Median    q1    q3
    ##   <fct>         <dbl>              <dbl>    <dbl>  <dbl> <dbl> <dbl>
    ## 1 blush          18.5              10.8     117.   15.2   9.99  24.1
    ## 2 bronzer        23.5              13.9     194.   21.0  11.2   32  
    ## 3 eyeliner       13.2               6.88     47.4  11     8     17.0
    ## 4 eyeshadow      22.2              16.3     267.   17.7   9.98  28  
    ## 5 foundation     21.4              11.4     129.   20.0  12     28  
    ## 6 lip_liner      10.2               5.16     26.6   9.99  4.99  13.0
    ## 7 lipstick       15.9              11.0     121.   12     8     21  
    ## 8 mascara        15.4               8.54     72.9  13.0   9     22  
    ## 9 nail_polish    14.1               7.25     52.6  11.0   8.14  22.2

From this summary table we can tell that the data distribution of almost
all product types are skewed. The data distribution of lip liner
category is the closest to normal distribution compared with other
product types. Bronze category has the highest mean and median,
eyeshadow category has the widest variance while lip liner category has
the lowest mean, median and narrowest variance.

### Violin plot

I am curious about rating distribution. Because lots of cosmetic
products have rating as missing, let’s choose five brands which have the
least missing rating values, and use `gglot` and `geom_violin` to create
violin graph for their rating distributions. I will assign
cosmetic_brand as x, rating as y also fill the violins with customized
colors based on different cosmetic brand.

``` r
#filter to 5 brands that have most non-missing rating
five_brand <- all_factor %>%
              filter(Cosmetic_Brand %in% c("l'oreal", "physicians formula", "covergirl", "maybelline", "revlon"))

p <- ggplot(data = five_brand, aes(x = Cosmetic_Brand, y = rating, fill = Cosmetic_Brand))
p + geom_violin(alpha = 0.6) +
scale_fill_manual(values=c("#EF6F6A", "#cc9900", "#69b3a2", "#404080", "#9172EC")) +
labs(x = "Cosmetic Brand", y = "Rating Distribution", title = "Violin Plot of rating distribution across cosmetic brands") 
```

![](README_files/figure-gfm/graphics2-1.png)<!-- -->

Almost all distributions are left-skewed, meaning most rating are at the
high end. Overall, “revlon” has the best rating since all of its rating
are about 3.3; “maybelline” has the worst rating because most of its
rating are below 4.5, only a small portion reached 5. It follows a
multimodal distribution with a big peak at 4.3 and a small peak at 3.
“physicians formula” also follows a multimodel distribution with 2 peaks
at 4.8 and 4.

### Scatter plots

Now I want to investigate whether there is any relationship between
price and rating. I will still pick those 5 brand names that have most
non-missing rating values, and put them in a list “a”. Then I am using a
`lapply` function to apply an anonymous function to every element in the
list “a”.

In my anonymous function, data frame will be filtered by the brand name
input and stored in “b”, then I am using `ggplot` function, assigning
“b\$rating” as y and “usd_price” as x. Later I will add a `geom_point`
layer to generate the corresponding scatter plot.

After run this `lapply` function, 5 scatter plots will be returned in a
list.

``` r
a <- list("covergirl", "l'oreal", "physicians formula", "maybelline", "revlon")
unlist(lapply(X=a, FUN= function(x) {
        b <-   all_factor %>%
                 filter(Cosmetic_Brand == x)
        s <- ggplot(data = b, aes(y = rating, x = usd_price))
          s + geom_point( alpha = 0.5, size = 2, position = "jitter") +
          labs(y = "Rating", x="USA Price", title = paste0("Scatter Plot of the Relationship between Price vs Rating for ",x))}))
```

    ## $data.brand1
    ## [1] "covergirl"
    ## 
    ## $data.brand2
    ## [1] "covergirl"
    ## 
    ## $data.brand3
    ## [1] "covergirl"
    ## 
    ## $data.brand4
    ## [1] "covergirl"
    ## 
    ## $data.brand5
    ## [1] "covergirl"
    ## 
    ## $data.brand6
    ## [1] "covergirl"
    ## 
    ## $data.brand7
    ## [1] "covergirl"
    ## 
    ## $data.brand8
    ## [1] "covergirl"
    ## 
    ## $data.brand9
    ## [1] "covergirl"
    ## 
    ## $data.brand10
    ## [1] "covergirl"
    ## 
    ## $data.brand11
    ## [1] "covergirl"
    ## 
    ## $data.brand12
    ## [1] "covergirl"
    ## 
    ## $data.brand13
    ## [1] "covergirl"
    ## 
    ## $data.brand14
    ## [1] "covergirl"
    ## 
    ## $data.brand15
    ## [1] "covergirl"
    ## 
    ## $data.brand16
    ## [1] "covergirl"
    ## 
    ## $data.brand17
    ## [1] "covergirl"
    ## 
    ## $data.brand18
    ## [1] "covergirl"
    ## 
    ## $data.brand19
    ## [1] "covergirl"
    ## 
    ## $data.brand20
    ## [1] "covergirl"
    ## 
    ## $data.brand21
    ## [1] "covergirl"
    ## 
    ## $data.brand22
    ## [1] "covergirl"
    ## 
    ## $data.brand23
    ## [1] "covergirl"
    ## 
    ## $data.brand24
    ## [1] "covergirl"
    ## 
    ## $data.brand25
    ## [1] "covergirl"
    ## 
    ## $data.brand26
    ## [1] "covergirl"
    ## 
    ## $data.brand27
    ## [1] "covergirl"
    ## 
    ## $data.brand28
    ## [1] "covergirl"
    ## 
    ## $data.brand29
    ## [1] "covergirl"
    ## 
    ## $data.brand30
    ## [1] "covergirl"
    ## 
    ## $data.brand31
    ## [1] "covergirl"
    ## 
    ## $data.brand32
    ## [1] "covergirl"
    ## 
    ## $data.brand33
    ## [1] "covergirl"
    ## 
    ## $data.brand34
    ## [1] "covergirl"
    ## 
    ## $data.brand35
    ## [1] "covergirl"
    ## 
    ## $data.brand36
    ## [1] "covergirl"
    ## 
    ## $data.brand37
    ## [1] "covergirl"
    ## 
    ## $data.brand38
    ## [1] "covergirl"
    ## 
    ## $data.brand39
    ## [1] "covergirl"
    ## 
    ## $data.brand40
    ## [1] "covergirl"
    ## 
    ## $data.brand41
    ## [1] "covergirl"
    ## 
    ## $data.brand42
    ## [1] "covergirl"
    ## 
    ## $data.brand43
    ## [1] "covergirl"
    ## 
    ## $data.brand44
    ## [1] "covergirl"
    ## 
    ## $data.brand45
    ## [1] "covergirl"
    ## 
    ## $data.brand46
    ## [1] "covergirl"
    ## 
    ## $data.brand47
    ## [1] "covergirl"
    ## 
    ## $data.brand48
    ## [1] "covergirl"
    ## 
    ## $data.brand49
    ## [1] "covergirl"
    ## 
    ## $data.brand50
    ## [1] "covergirl"
    ## 
    ## $data.brand51
    ## [1] "covergirl"
    ## 
    ## $data.brand52
    ## [1] "covergirl"
    ## 
    ## $data.brand53
    ## [1] "covergirl"
    ## 
    ## $data.brand54
    ## [1] "covergirl"
    ## 
    ## $data.name1
    ## [1] "CoverGirl Instant Cheekbones Contouring Blush"
    ## 
    ## $data.name2
    ## [1] "CoverGirl Cheekers Blush"
    ## 
    ## $data.name3
    ## [1] "CoverGirl Clean Glow Blush"
    ## 
    ## $data.name4
    ## [1] "CoverGirl truBLEND Blush in Medium Rose"
    ## 
    ## $data.name5
    ## [1] "CoverGirl Cheekers Blush "
    ## 
    ## $data.name6
    ## [1] "CoverGirl truBLEND Blush in Light Rose"
    ## 
    ## $data.name7
    ## [1] "CoverGirl Clean Glow Blush "
    ## 
    ## $data.name8
    ## [1] "CoverGirl truBLEND Bronzer"
    ## 
    ## $data.name9
    ## [1] "CoverGirl Clean Glow Bronzer"
    ## 
    ## $data.name10
    ## [1] "CoverGirl Ink It! By Perfect Point Plus Violet Ink"
    ## 
    ## $data.name11
    ## [1] "CoverGirl Ink it! By Perfect Point Plus Eyeliner"
    ## 
    ## $data.name12
    ## [1] "CoverGirl Trunaked Waterproof Eyeliner Duo in Mocha/Ebony"
    ## 
    ## $data.name13
    ## [1] "CoverGirl Bombshell Powder Brow & Liner in Dark Brown"
    ## 
    ## $data.name14
    ## [1] "CoverGirl Trunaked Waterproof Eyeliner Duo in Cashmere/Espresso"
    ## 
    ## $data.name15
    ## [1] "CoverGirl Full Lash Bloom Mascara + Perfect Point Plus Eye Liner"
    ## 
    ## $data.name16
    ## [1] "CoverGirl Liquiline Blast Eyeliner "
    ## 
    ## $data.name17
    ## [1] "CoverGirl Perfect Point Plus Eyeliner Pencil "
    ## 
    ## $data.name18
    ## [1] "CoverGirl Bombshell Powder Brow & Liner in Blonde"
    ## 
    ## $data.name19
    ## [1] "CoverGirl Intensify Me! Liquid Liner"
    ## 
    ## $data.name20
    ## [1] "CoverGirl LineExact Liquid Eyeliner "
    ## 
    ## $data.name21
    ## [1] "CoverGirl Ink it! By Perfect Point Plus Eyeliner "
    ## 
    ## $data.name22
    ## [1] "CoverGirl Eye Enhancers 3-Kit Shadows "
    ## 
    ## $data.name23
    ## [1] "CoverGirl Eye Enhancers 4-Kit Shadows"
    ## 
    ## $data.name24
    ## [1] "CoverGirl Eye Enhancers 3-Kit Shadows"
    ## 
    ## $data.name25
    ## [1] "CoverGirl Trunaked Eyeshadow Palettes in Roses"
    ## 
    ## $data.name26
    ## [1] "CoverGirl Eye Enhancers 1-Kit Shadow "
    ## 
    ## $data.name27
    ## [1] "CoverGirl Outlast Stay Fabulous 3-in-1 Foundation "
    ## 
    ## $data.name28
    ## [1] "CoverGirl Outlast Stay Fabulous 3-in-1 Foundation"
    ## 
    ## $data.name29
    ## [1] "CoverGirl Clean Oil Control Makeup "
    ## 
    ## $data.name30
    ## [1] "CoverGirl Advanced Radiance Age Defying Liquid Makeup "
    ## 
    ## $data.name31
    ## [1] "CoverGirl & Olay Simply Ageless Foundation "
    ## 
    ## $data.name32
    ## [1] "CoverGirl Clean Liquid Makeup Normal Skin "
    ## 
    ## $data.name33
    ## [1] "CoverGirl Outlast Stay Luminous Foundation Creamy Natural (820) "
    ## 
    ## $data.name34
    ## [1] "CoverGirl Smoothers All-Day Hydrating Makeup "
    ## 
    ## $data.name35
    ## [1] "CoverGirl Ready, Set Gorgeous Liquid Makeup 105"
    ## 
    ## $data.name36
    ## [1] "CoverGirl Ultimate Finish Liquid Powder Makeup "
    ## 
    ## $data.name37
    ## [1] "CoverGirl Lip Perfection Lipliner "
    ## 
    ## $data.name38
    ## [1] "CoverGirl Colorlicious Lipstick"
    ## 
    ## $data.name39
    ## [1] "CoverGirl Outlast Lipcolor"
    ## 
    ## $data.name40
    ## [1] "CoverGirl Outlast Lipcolor Moisturizing Clear Topcoat (500)"
    ## 
    ## $data.name41
    ## [1] "CoverGirl Outlast Longwear Lipstick"
    ## 
    ## $data.name42
    ## [1] "CoverGirl Professional Mascara Curved Brush Very Black"
    ## 
    ## $data.name43
    ## [1] "CoverGirl Professional Mascara Curved Brush Black Brown"
    ## 
    ## $data.name44
    ## [1] "CoverGirl Professional Waterproof Mascara Very Black"
    ## 
    ## $data.name45
    ## [1] "CoverGirl Professional Super Thick Lash Mascara Very Black"
    ## 
    ## $data.name46
    ## [1] "CoverGirl LashBlast Fusion Water Resistant Mascara "
    ## 
    ## $data.name47
    ## [1] "CoverGirl LastBlast Clump Crusher Mascara "
    ## 
    ## $data.name48
    ## [1] "CoverGirl Professional Natural Lash Mascara Clear"
    ## 
    ## $data.name49
    ## [1] "CoverGirl LastBlast Clump Crusher Water Resistant Mascara "
    ## 
    ## $data.name50
    ## [1] "CoverGirl LashBlast Full Lash Bloom Mascara Very Black (800)"
    ## 
    ## $data.name51
    ## [1] "CoverGirl Lashblast Volume Blasting Waterproof Mascara"
    ## 
    ## $data.name52
    ## [1] "CoverGirl Lashblast Volume Blasting Mascara "
    ## 
    ## $data.name53
    ## [1] "CoverGirl LashBlast Fusion Mascara "
    ## 
    ## $data.name54
    ## [1] "CoverGirl Outlast Stay Brilliant Nail Gloss"
    ## 
    ## $data.product_type1
    ## [1] "blush"
    ## 
    ## $data.product_type2
    ## [1] "blush"
    ## 
    ## $data.product_type3
    ## [1] "blush"
    ## 
    ## $data.product_type4
    ## [1] "blush"
    ## 
    ## $data.product_type5
    ## [1] "blush"
    ## 
    ## $data.product_type6
    ## [1] "blush"
    ## 
    ## $data.product_type7
    ## [1] "blush"
    ## 
    ## $data.product_type8
    ## [1] "bronzer"
    ## 
    ## $data.product_type9
    ## [1] "bronzer"
    ## 
    ## $data.product_type10
    ## [1] "eyeliner"
    ## 
    ## $data.product_type11
    ## [1] "eyeliner"
    ## 
    ## $data.product_type12
    ## [1] "eyeliner"
    ## 
    ## $data.product_type13
    ## [1] "eyeliner"
    ## 
    ## $data.product_type14
    ## [1] "eyeliner"
    ## 
    ## $data.product_type15
    ## [1] "eyeliner"
    ## 
    ## $data.product_type16
    ## [1] "eyeliner"
    ## 
    ## $data.product_type17
    ## [1] "eyeliner"
    ## 
    ## $data.product_type18
    ## [1] "eyeliner"
    ## 
    ## $data.product_type19
    ## [1] "eyeliner"
    ## 
    ## $data.product_type20
    ## [1] "eyeliner"
    ## 
    ## $data.product_type21
    ## [1] "eyeliner"
    ## 
    ## $data.product_type22
    ## [1] "eyeshadow"
    ## 
    ## $data.product_type23
    ## [1] "eyeshadow"
    ## 
    ## $data.product_type24
    ## [1] "eyeshadow"
    ## 
    ## $data.product_type25
    ## [1] "eyeshadow"
    ## 
    ## $data.product_type26
    ## [1] "eyeshadow"
    ## 
    ## $data.product_type27
    ## [1] "foundation"
    ## 
    ## $data.product_type28
    ## [1] "foundation"
    ## 
    ## $data.product_type29
    ## [1] "foundation"
    ## 
    ## $data.product_type30
    ## [1] "foundation"
    ## 
    ## $data.product_type31
    ## [1] "foundation"
    ## 
    ## $data.product_type32
    ## [1] "foundation"
    ## 
    ## $data.product_type33
    ## [1] "foundation"
    ## 
    ## $data.product_type34
    ## [1] "foundation"
    ## 
    ## $data.product_type35
    ## [1] "foundation"
    ## 
    ## $data.product_type36
    ## [1] "foundation"
    ## 
    ## $data.product_type37
    ## [1] "lip_liner"
    ## 
    ## $data.product_type38
    ## [1] "lipstick"
    ## 
    ## $data.product_type39
    ## [1] "lipstick"
    ## 
    ## $data.product_type40
    ## [1] "lipstick"
    ## 
    ## $data.product_type41
    ## [1] "lipstick"
    ## 
    ## $data.product_type42
    ## [1] "mascara"
    ## 
    ## $data.product_type43
    ## [1] "mascara"
    ## 
    ## $data.product_type44
    ## [1] "mascara"
    ## 
    ## $data.product_type45
    ## [1] "mascara"
    ## 
    ## $data.product_type46
    ## [1] "mascara"
    ## 
    ## $data.product_type47
    ## [1] "mascara"
    ## 
    ## $data.product_type48
    ## [1] "mascara"
    ## 
    ## $data.product_type49
    ## [1] "mascara"
    ## 
    ## $data.product_type50
    ## [1] "mascara"
    ## 
    ## $data.product_type51
    ## [1] "mascara"
    ## 
    ## $data.product_type52
    ## [1] "mascara"
    ## 
    ## $data.product_type53
    ## [1] "mascara"
    ## 
    ## $data.product_type54
    ## [1] "nail_polish"
    ## 
    ## $data.usd_price1
    ## [1] 7.99
    ## 
    ## $data.usd_price2
    ## [1] 6.99
    ## 
    ## $data.usd_price3
    ## [1] 8.99
    ## 
    ## $data.usd_price4
    ## [1] 13.99
    ## 
    ## $data.usd_price5
    ## [1] 6.99
    ## 
    ## $data.usd_price6
    ## [1] 13.99
    ## 
    ## $data.usd_price7
    ## [1] 8.99
    ## 
    ## $data.usd_price8
    ## [1] 13.99
    ## 
    ## $data.usd_price9
    ## [1] 8.99
    ## 
    ## $data.usd_price10
    ## [1] 6.99
    ## 
    ## $data.usd_price11
    ## [1] 6.99
    ## 
    ## $data.usd_price12
    ## [1] 11.99
    ## 
    ## $data.usd_price13
    ## [1] 9.49
    ## 
    ## $data.usd_price14
    ## [1] 11.99
    ## 
    ## $data.usd_price15
    ## [1] 14.99
    ## 
    ## $data.usd_price16
    ## [1] 9.99
    ## 
    ## $data.usd_price17
    ## [1] 7.49
    ## 
    ## $data.usd_price18
    ## [1] 9.49
    ## 
    ## $data.usd_price19
    ## [1] 9.99
    ## 
    ## $data.usd_price20
    ## [1] 7.49
    ## 
    ## $data.usd_price21
    ## [1] 6.99
    ## 
    ## $data.usd_price22
    ## [1] 6.49
    ## 
    ## $data.usd_price23
    ## [1] 6.49
    ## 
    ## $data.usd_price24
    ## [1] 6.49
    ## 
    ## $data.usd_price25
    ## [1] 12.99
    ## 
    ## $data.usd_price26
    ## [1] 4.49
    ## 
    ## $data.usd_price27
    ## [1] 12.49
    ## 
    ## $data.usd_price28
    ## [1] 12.49
    ## 
    ## $data.usd_price29
    ## [1] 9.49
    ## 
    ## $data.usd_price30
    ## [1] 12.49
    ## 
    ## $data.usd_price31
    ## [1] 15.99
    ## 
    ## $data.usd_price32
    ## [1] 9.49
    ## 
    ## $data.usd_price33
    ## [1] 12.49
    ## 
    ## $data.usd_price34
    ## [1] 8.99
    ## 
    ## $data.usd_price35
    ## [1] 8.99
    ## 
    ## $data.usd_price36
    ## [1] 9.99
    ## 
    ## $data.usd_price37
    ## [1] 9.99
    ## 
    ## $data.usd_price38
    ## [1] 10.49
    ## 
    ## $data.usd_price39
    ## [1] 12.99
    ## 
    ## $data.usd_price40
    ## [1] 12.99
    ## 
    ## $data.usd_price41
    ## [1] 10.99
    ## 
    ## $data.usd_price42
    ## [1] 6.99
    ## 
    ## $data.usd_price43
    ## [1] 6.99
    ## 
    ## $data.usd_price44
    ## [1] 6.99
    ## 
    ## $data.usd_price45
    ## [1] 6.99
    ## 
    ## $data.usd_price46
    ## [1] 9.99
    ## 
    ## $data.usd_price47
    ## [1] 9.99
    ## 
    ## $data.usd_price48
    ## [1] 6.99
    ## 
    ## $data.usd_price49
    ## [1] 9.99
    ## 
    ## $data.usd_price50
    ## [1] 9.99
    ## 
    ## $data.usd_price51
    ## [1] 9.99
    ## 
    ## $data.usd_price52
    ## [1] 9.99
    ## 
    ## $data.usd_price53
    ## [1] 9.99
    ## 
    ## $data.usd_price54
    ## [1] 5.99
    ## 
    ## $data.price_stat1
    ## [1] "This blush's price is $7.99 which is lower than the overall blush price mean (18.49), lower than the price median(15.25), lower than the 25th percentile(9.99), and lower than the 75th percentile(24.12)."
    ## 
    ## $data.price_stat2
    ## [1] "This blush's price is $6.99 which is lower than the overall blush price mean (18.49), lower than the price median(15.25), lower than the 25th percentile(9.99), and lower than the 75th percentile(24.12)."
    ## 
    ## $data.price_stat3
    ## [1] "This blush's price is $8.99 which is lower than the overall blush price mean (18.49), lower than the price median(15.25), lower than the 25th percentile(9.99), and lower than the 75th percentile(24.12)."
    ## 
    ## $data.price_stat4
    ## [1] "This blush's price is $13.99 which is lower than the overall blush price mean (18.49), lower than the price median(15.25), higher than the 25th percentile(9.99), and lower than the 75th percentile(24.12)."
    ## 
    ## $data.price_stat5
    ## [1] "This blush's price is $6.99 which is lower than the overall blush price mean (18.49), lower than the price median(15.25), lower than the 25th percentile(9.99), and lower than the 75th percentile(24.12)."
    ## 
    ## $data.price_stat6
    ## [1] "This blush's price is $13.99 which is lower than the overall blush price mean (18.49), lower than the price median(15.25), higher than the 25th percentile(9.99), and lower than the 75th percentile(24.12)."
    ## 
    ## $data.price_stat7
    ## [1] "This blush's price is $8.99 which is lower than the overall blush price mean (18.49), lower than the price median(15.25), lower than the 25th percentile(9.99), and lower than the 75th percentile(24.12)."
    ## 
    ## $data.price_stat8
    ## [1] "This bronzer's price is $13.99 which is lower than the overall bronzer price mean (23.54), lower than the price median(20.99), higher than the 25th percentile(11.24), and lower than the 75th percentile(32)."
    ## 
    ## $data.price_stat9
    ## [1] "This bronzer's price is $8.99 which is lower than the overall bronzer price mean (23.54), lower than the price median(20.99), lower than the 25th percentile(11.24), and lower than the 75th percentile(32)."
    ## 
    ## $data.price_stat10
    ## [1] "This eyeliner's price is $6.99 which is lower than the overall eyeliner price mean (13.19), lower than the price median(11), lower than the 25th percentile(8), and lower than the 75th percentile(16.99)."
    ## 
    ## $data.price_stat11
    ## [1] "This eyeliner's price is $6.99 which is lower than the overall eyeliner price mean (13.19), lower than the price median(11), lower than the 25th percentile(8), and lower than the 75th percentile(16.99)."
    ## 
    ## $data.price_stat12
    ## [1] "This eyeliner's price is $11.99 which is lower than the overall eyeliner price mean (13.19), higher than the price median(11), higher than the 25th percentile(8), and lower than the 75th percentile(16.99)."
    ## 
    ## $data.price_stat13
    ## [1] "This eyeliner's price is $9.49 which is lower than the overall eyeliner price mean (13.19), lower than the price median(11), higher than the 25th percentile(8), and lower than the 75th percentile(16.99)."
    ## 
    ## $data.price_stat14
    ## [1] "This eyeliner's price is $11.99 which is lower than the overall eyeliner price mean (13.19), higher than the price median(11), higher than the 25th percentile(8), and lower than the 75th percentile(16.99)."
    ## 
    ## $data.price_stat15
    ## [1] "This eyeliner's price is $14.99 which is higher than the overall eyeliner price mean (13.19), higher than the price median(11), higher than the 25th percentile(8), and lower than the 75th percentile(16.99)."
    ## 
    ## $data.price_stat16
    ## [1] "This eyeliner's price is $9.99 which is lower than the overall eyeliner price mean (13.19), lower than the price median(11), higher than the 25th percentile(8), and lower than the 75th percentile(16.99)."
    ## 
    ## $data.price_stat17
    ## [1] "This eyeliner's price is $7.49 which is lower than the overall eyeliner price mean (13.19), lower than the price median(11), lower than the 25th percentile(8), and lower than the 75th percentile(16.99)."
    ## 
    ## $data.price_stat18
    ## [1] "This eyeliner's price is $9.49 which is lower than the overall eyeliner price mean (13.19), lower than the price median(11), higher than the 25th percentile(8), and lower than the 75th percentile(16.99)."
    ## 
    ## $data.price_stat19
    ## [1] "This eyeliner's price is $9.99 which is lower than the overall eyeliner price mean (13.19), lower than the price median(11), higher than the 25th percentile(8), and lower than the 75th percentile(16.99)."
    ## 
    ## $data.price_stat20
    ## [1] "This eyeliner's price is $7.49 which is lower than the overall eyeliner price mean (13.19), lower than the price median(11), lower than the 25th percentile(8), and lower than the 75th percentile(16.99)."
    ## 
    ## $data.price_stat21
    ## [1] "This eyeliner's price is $6.99 which is lower than the overall eyeliner price mean (13.19), lower than the price median(11), lower than the 25th percentile(8), and lower than the 75th percentile(16.99)."
    ## 
    ## $data.price_stat22
    ## [1] "This eyeshadow's price is $6.49 which is lower than the overall eyeshadow price mean (22.15), lower than the price median(17.74), lower than the 25th percentile(9.98), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat23
    ## [1] "This eyeshadow's price is $6.49 which is lower than the overall eyeshadow price mean (22.15), lower than the price median(17.74), lower than the 25th percentile(9.98), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat24
    ## [1] "This eyeshadow's price is $6.49 which is lower than the overall eyeshadow price mean (22.15), lower than the price median(17.74), lower than the 25th percentile(9.98), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat25
    ## [1] "This eyeshadow's price is $12.99 which is lower than the overall eyeshadow price mean (22.15), lower than the price median(17.74), higher than the 25th percentile(9.98), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat26
    ## [1] "This eyeshadow's price is $4.49 which is lower than the overall eyeshadow price mean (22.15), lower than the price median(17.74), lower than the 25th percentile(9.98), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat27
    ## [1] "This foundation's price is $12.49 which is lower than the overall foundation price mean (21.41), lower than the price median(19.99), higher than the 25th percentile(12), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat28
    ## [1] "This foundation's price is $12.49 which is lower than the overall foundation price mean (21.41), lower than the price median(19.99), higher than the 25th percentile(12), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat29
    ## [1] "This foundation's price is $9.49 which is lower than the overall foundation price mean (21.41), lower than the price median(19.99), lower than the 25th percentile(12), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat30
    ## [1] "This foundation's price is $12.49 which is lower than the overall foundation price mean (21.41), lower than the price median(19.99), higher than the 25th percentile(12), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat31
    ## [1] "This foundation's price is $15.99 which is lower than the overall foundation price mean (21.41), lower than the price median(19.99), higher than the 25th percentile(12), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat32
    ## [1] "This foundation's price is $9.49 which is lower than the overall foundation price mean (21.41), lower than the price median(19.99), lower than the 25th percentile(12), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat33
    ## [1] "This foundation's price is $12.49 which is lower than the overall foundation price mean (21.41), lower than the price median(19.99), higher than the 25th percentile(12), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat34
    ## [1] "This foundation's price is $8.99 which is lower than the overall foundation price mean (21.41), lower than the price median(19.99), lower than the 25th percentile(12), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat35
    ## [1] "This foundation's price is $8.99 which is lower than the overall foundation price mean (21.41), lower than the price median(19.99), lower than the 25th percentile(12), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat36
    ## [1] "This foundation's price is $9.99 which is lower than the overall foundation price mean (21.41), lower than the price median(19.99), lower than the 25th percentile(12), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat37
    ## [1] "This lip_liner's price is $9.99 which is lower than the overall lip_liner price mean (10.16), equal to the price median(9.99), higher than the 25th percentile(4.99), and lower than the 75th percentile(12.99)."
    ## 
    ## $data.price_stat38
    ## [1] "This lipstick's price is $10.49 which is lower than the overall lipstick price mean (15.95), lower than the price median(12), higher than the 25th percentile(8), and lower than the 75th percentile(21)."
    ## 
    ## $data.price_stat39
    ## [1] "This lipstick's price is $12.99 which is lower than the overall lipstick price mean (15.95), higher than the price median(12), higher than the 25th percentile(8), and lower than the 75th percentile(21)."
    ## 
    ## $data.price_stat40
    ## [1] "This lipstick's price is $12.99 which is lower than the overall lipstick price mean (15.95), higher than the price median(12), higher than the 25th percentile(8), and lower than the 75th percentile(21)."
    ## 
    ## $data.price_stat41
    ## [1] "This lipstick's price is $10.99 which is lower than the overall lipstick price mean (15.95), lower than the price median(12), higher than the 25th percentile(8), and lower than the 75th percentile(21)."
    ## 
    ## $data.price_stat42
    ## [1] "This mascara's price is $6.99 which is lower than the overall mascara price mean (15.39), lower than the price median(12.99), lower than the 25th percentile(9), and lower than the 75th percentile(22)."
    ## 
    ## $data.price_stat43
    ## [1] "This mascara's price is $6.99 which is lower than the overall mascara price mean (15.39), lower than the price median(12.99), lower than the 25th percentile(9), and lower than the 75th percentile(22)."
    ## 
    ## $data.price_stat44
    ## [1] "This mascara's price is $6.99 which is lower than the overall mascara price mean (15.39), lower than the price median(12.99), lower than the 25th percentile(9), and lower than the 75th percentile(22)."
    ## 
    ## $data.price_stat45
    ## [1] "This mascara's price is $6.99 which is lower than the overall mascara price mean (15.39), lower than the price median(12.99), lower than the 25th percentile(9), and lower than the 75th percentile(22)."
    ## 
    ## $data.price_stat46
    ## [1] "This mascara's price is $9.99 which is lower than the overall mascara price mean (15.39), lower than the price median(12.99), higher than the 25th percentile(9), and lower than the 75th percentile(22)."
    ## 
    ## $data.price_stat47
    ## [1] "This mascara's price is $9.99 which is lower than the overall mascara price mean (15.39), lower than the price median(12.99), higher than the 25th percentile(9), and lower than the 75th percentile(22)."
    ## 
    ## $data.price_stat48
    ## [1] "This mascara's price is $6.99 which is lower than the overall mascara price mean (15.39), lower than the price median(12.99), lower than the 25th percentile(9), and lower than the 75th percentile(22)."
    ## 
    ## $data.price_stat49
    ## [1] "This mascara's price is $9.99 which is lower than the overall mascara price mean (15.39), lower than the price median(12.99), higher than the 25th percentile(9), and lower than the 75th percentile(22)."
    ## 
    ## $data.price_stat50
    ## [1] "This mascara's price is $9.99 which is lower than the overall mascara price mean (15.39), lower than the price median(12.99), higher than the 25th percentile(9), and lower than the 75th percentile(22)."
    ## 
    ## $data.price_stat51
    ## [1] "This mascara's price is $9.99 which is lower than the overall mascara price mean (15.39), lower than the price median(12.99), higher than the 25th percentile(9), and lower than the 75th percentile(22)."
    ## 
    ## $data.price_stat52
    ## [1] "This mascara's price is $9.99 which is lower than the overall mascara price mean (15.39), lower than the price median(12.99), higher than the 25th percentile(9), and lower than the 75th percentile(22)."
    ## 
    ## $data.price_stat53
    ## [1] "This mascara's price is $9.99 which is lower than the overall mascara price mean (15.39), lower than the price median(12.99), higher than the 25th percentile(9), and lower than the 75th percentile(22)."
    ## 
    ## $data.price_stat54
    ## [1] "This nail_polish's price is $5.99 which is lower than the overall nail_polish price mean (14.1), lower than the price median(10.99), lower than the 25th percentile(8.14), and lower than the 75th percentile(22.24)."
    ## 
    ## $data.rating1
    ## [1] 3.3
    ## 
    ## $data.rating2
    ## [1] 5
    ## 
    ## $data.rating3
    ## [1] NA
    ## 
    ## $data.rating4
    ## [1] 5
    ## 
    ## $data.rating5
    ## [1] 5
    ## 
    ## $data.rating6
    ## [1] NA
    ## 
    ## $data.rating7
    ## [1] 5
    ## 
    ## $data.rating8
    ## [1] NA
    ## 
    ## $data.rating9
    ## [1] 5
    ## 
    ## $data.rating10
    ## [1] NA
    ## 
    ## $data.rating11
    ## [1] 2.5
    ## 
    ## $data.rating12
    ## [1] NA
    ## 
    ## $data.rating13
    ## [1] 3
    ## 
    ## $data.rating14
    ## [1] NA
    ## 
    ## $data.rating15
    ## [1] NA
    ## 
    ## $data.rating16
    ## [1] 4.5
    ## 
    ## $data.rating17
    ## [1] 4
    ## 
    ## $data.rating18
    ## [1] NA
    ## 
    ## $data.rating19
    ## [1] NA
    ## 
    ## $data.rating20
    ## [1] 4.3
    ## 
    ## $data.rating21
    ## [1] NA
    ## 
    ## $data.rating22
    ## [1] 4
    ## 
    ## $data.rating23
    ## [1] 4.6
    ## 
    ## $data.rating24
    ## [1] 4
    ## 
    ## $data.rating25
    ## [1] 5
    ## 
    ## $data.rating26
    ## [1] 5
    ## 
    ## $data.rating27
    ## [1] 4
    ## 
    ## $data.rating28
    ## [1] NA
    ## 
    ## $data.rating29
    ## [1] 4
    ## 
    ## $data.rating30
    ## [1] 5
    ## 
    ## $data.rating31
    ## [1] 5
    ## 
    ## $data.rating32
    ## [1] 5
    ## 
    ## $data.rating33
    ## [1] NA
    ## 
    ## $data.rating34
    ## [1] NA
    ## 
    ## $data.rating35
    ## [1] 2
    ## 
    ## $data.rating36
    ## [1] 5
    ## 
    ## $data.rating37
    ## [1] NA
    ## 
    ## $data.rating38
    ## [1] NA
    ## 
    ## $data.rating39
    ## [1] 5
    ## 
    ## $data.rating40
    ## [1] NA
    ## 
    ## $data.rating41
    ## [1] NA
    ## 
    ## $data.rating42
    ## [1] NA
    ## 
    ## $data.rating43
    ## [1] NA
    ## 
    ## $data.rating44
    ## [1] NA
    ## 
    ## $data.rating45
    ## [1] 5
    ## 
    ## $data.rating46
    ## [1] 4.8
    ## 
    ## $data.rating47
    ## [1] 4.5
    ## 
    ## $data.rating48
    ## [1] 4
    ## 
    ## $data.rating49
    ## [1] 5
    ## 
    ## $data.rating50
    ## [1] 4
    ## 
    ## $data.rating51
    ## [1] 5
    ## 
    ## $data.rating52
    ## [1] 4.8
    ## 
    ## $data.rating53
    ## [1] 4.8
    ## 
    ## $data.rating54
    ## [1] NA
    ## 
    ## $data.rating_stat1
    ## [1] "This blush's rating is 3.3 which is lower than the overall blush rating mean (4.42), lower than the rating median(4.75), lower than the 25th percentile(4), and lower than the 75th percentile(5)."
    ## 
    ## $data.rating_stat2
    ## [1] "This blush's rating is 5 which is higher than the overall blush rating mean (4.42), higher than the rating median(4.75), higher than the 25th percentile(4), and equal to the 75th percentile(5)."
    ## 
    ## $data.rating_stat3
    ## [1] "No rating"
    ## 
    ## $data.rating_stat4
    ## [1] "This blush's rating is 5 which is higher than the overall blush rating mean (4.42), higher than the rating median(4.75), higher than the 25th percentile(4), and equal to the 75th percentile(5)."
    ## 
    ## $data.rating_stat5
    ## [1] "This blush's rating is 5 which is higher than the overall blush rating mean (4.42), higher than the rating median(4.75), higher than the 25th percentile(4), and equal to the 75th percentile(5)."
    ## 
    ## $data.rating_stat6
    ## [1] "No rating"
    ## 
    ## $data.rating_stat7
    ## [1] "This blush's rating is 5 which is higher than the overall blush rating mean (4.42), higher than the rating median(4.75), higher than the 25th percentile(4), and equal to the 75th percentile(5)."
    ## 
    ## $data.rating_stat8
    ## [1] "No rating"
    ## 
    ## $data.rating_stat9
    ## [1] "This bronzer's rating is 5 which is higher than the overall bronzer rating mean (4.61), higher than the rating median(4.95), higher than the 25th percentile(4.38), and equal to the 75th percentile(5)."
    ## 
    ## $data.rating_stat10
    ## [1] "No rating"
    ## 
    ## $data.rating_stat11
    ## [1] "This eyeliner's rating is 2.5 which is lower than the overall eyeliner rating mean (4.23), lower than the rating median(4.3), lower than the 25th percentile(4), and lower than the 75th percentile(4.8)."
    ## 
    ## $data.rating_stat12
    ## [1] "No rating"
    ## 
    ## $data.rating_stat13
    ## [1] "This eyeliner's rating is 3 which is lower than the overall eyeliner rating mean (4.23), lower than the rating median(4.3), lower than the 25th percentile(4), and lower than the 75th percentile(4.8)."
    ## 
    ## $data.rating_stat14
    ## [1] "No rating"
    ## 
    ## $data.rating_stat15
    ## [1] "No rating"
    ## 
    ## $data.rating_stat16
    ## [1] "This eyeliner's rating is 4.5 which is higher than the overall eyeliner rating mean (4.23), higher than the rating median(4.3), higher than the 25th percentile(4), and lower than the 75th percentile(4.8)."
    ## 
    ## $data.rating_stat17
    ## [1] "This eyeliner's rating is 4 which is lower than the overall eyeliner rating mean (4.23), lower than the rating median(4.3), equal to the 25th percentile(4), and lower than the 75th percentile(4.8)."
    ## 
    ## $data.rating_stat18
    ## [1] "No rating"
    ## 
    ## $data.rating_stat19
    ## [1] "No rating"
    ## 
    ## $data.rating_stat20
    ## [1] "This eyeliner's rating is 4.3 which is higher than the overall eyeliner rating mean (4.23), equal to the rating median(4.3), higher than the 25th percentile(4), and lower than the 75th percentile(4.8)."
    ## 
    ## $data.rating_stat21
    ## [1] "No rating"
    ## 
    ## $data.rating_stat22
    ## [1] "This eyeshadow's rating is 4 which is lower than the overall eyeshadow rating mean (4.39), lower than the rating median(4.5), equal to the 25th percentile(4), and lower than the 75th percentile(5)."
    ## 
    ## $data.rating_stat23
    ## [1] "This eyeshadow's rating is 4.6 which is higher than the overall eyeshadow rating mean (4.39), higher than the rating median(4.5), higher than the 25th percentile(4), and lower than the 75th percentile(5)."
    ## 
    ## $data.rating_stat24
    ## [1] "This eyeshadow's rating is 4 which is lower than the overall eyeshadow rating mean (4.39), lower than the rating median(4.5), equal to the 25th percentile(4), and lower than the 75th percentile(5)."
    ## 
    ## $data.rating_stat25
    ## [1] "This eyeshadow's rating is 5 which is higher than the overall eyeshadow rating mean (4.39), higher than the rating median(4.5), higher than the 25th percentile(4), and equal to the 75th percentile(5)."
    ## 
    ## $data.rating_stat26
    ## [1] "This eyeshadow's rating is 5 which is higher than the overall eyeshadow rating mean (4.39), higher than the rating median(4.5), higher than the 25th percentile(4), and equal to the 75th percentile(5)."
    ## 
    ## $data.rating_stat27
    ## [1] "This foundation's rating is 4 which is lower than the overall foundation rating mean (4.22), lower than the rating median(4.3), equal to the 25th percentile(4), and lower than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat28
    ## [1] "No rating"
    ## 
    ## $data.rating_stat29
    ## [1] "This foundation's rating is 4 which is lower than the overall foundation rating mean (4.22), lower than the rating median(4.3), equal to the 25th percentile(4), and lower than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat30
    ## [1] "This foundation's rating is 5 which is higher than the overall foundation rating mean (4.22), higher than the rating median(4.3), higher than the 25th percentile(4), and higher than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat31
    ## [1] "This foundation's rating is 5 which is higher than the overall foundation rating mean (4.22), higher than the rating median(4.3), higher than the 25th percentile(4), and higher than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat32
    ## [1] "This foundation's rating is 5 which is higher than the overall foundation rating mean (4.22), higher than the rating median(4.3), higher than the 25th percentile(4), and higher than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat33
    ## [1] "No rating"
    ## 
    ## $data.rating_stat34
    ## [1] "No rating"
    ## 
    ## $data.rating_stat35
    ## [1] "This foundation's rating is 2 which is lower than the overall foundation rating mean (4.22), lower than the rating median(4.3), lower than the 25th percentile(4), and lower than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat36
    ## [1] "This foundation's rating is 5 which is higher than the overall foundation rating mean (4.22), higher than the rating median(4.3), higher than the 25th percentile(4), and higher than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat37
    ## [1] "No rating"
    ## 
    ## $data.rating_stat38
    ## [1] "No rating"
    ## 
    ## $data.rating_stat39
    ## [1] "This lipstick's rating is 5 which is higher than the overall lipstick rating mean (4.43), higher than the rating median(4.5), higher than the 25th percentile(4.07), and higher than the 75th percentile(4.93)."
    ## 
    ## $data.rating_stat40
    ## [1] "No rating"
    ## 
    ## $data.rating_stat41
    ## [1] "No rating"
    ## 
    ## $data.rating_stat42
    ## [1] "No rating"
    ## 
    ## $data.rating_stat43
    ## [1] "No rating"
    ## 
    ## $data.rating_stat44
    ## [1] "No rating"
    ## 
    ## $data.rating_stat45
    ## [1] "This mascara's rating is 5 which is higher than the overall mascara rating mean (4.28), higher than the rating median(4.3), higher than the 25th percentile(4), and higher than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat46
    ## [1] "This mascara's rating is 4.8 which is higher than the overall mascara rating mean (4.28), higher than the rating median(4.3), higher than the 25th percentile(4), and higher than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat47
    ## [1] "This mascara's rating is 4.5 which is higher than the overall mascara rating mean (4.28), higher than the rating median(4.3), higher than the 25th percentile(4), and lower than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat48
    ## [1] "This mascara's rating is 4 which is lower than the overall mascara rating mean (4.28), lower than the rating median(4.3), equal to the 25th percentile(4), and lower than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat49
    ## [1] "This mascara's rating is 5 which is higher than the overall mascara rating mean (4.28), higher than the rating median(4.3), higher than the 25th percentile(4), and higher than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat50
    ## [1] "This mascara's rating is 4 which is lower than the overall mascara rating mean (4.28), lower than the rating median(4.3), equal to the 25th percentile(4), and lower than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat51
    ## [1] "This mascara's rating is 5 which is higher than the overall mascara rating mean (4.28), higher than the rating median(4.3), higher than the 25th percentile(4), and higher than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat52
    ## [1] "This mascara's rating is 4.8 which is higher than the overall mascara rating mean (4.28), higher than the rating median(4.3), higher than the 25th percentile(4), and higher than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat53
    ## [1] "This mascara's rating is 4.8 which is higher than the overall mascara rating mean (4.28), higher than the rating median(4.3), higher than the 25th percentile(4), and higher than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat54
    ## [1] "No rating"
    ## 
    ## $data.description1
    ## [1] "Want great bone structure? Instant Cheekbones Contouring Blush lets you create them instantly and easily. Each compact contains three expertly coordinated shades that add natural-looking contour, so you can look like you were born with great cheekbones, anytime!Features:​3 Complementary skin-flushed shades contour your cheekbonesLightweightDermatologically testedBlendable"
    ## 
    ## $data.description2
    ## [1] "Blush's basic rules—it has to be easy, and it has to look natural. That's Cheekers. Each portable mini-compact gives you a sheer blush that goes on to stay true, stay for hours. It's the way to glow!Features: Stays for hoursEasy and naturalDermatologically-tested"
    ## 
    ## $data.description3
    ## [1] "Complete your Clean® look with a flush of lightweight, blendable color. This versatile 3-in-1 multi-shade compact lets you customize your color to create a radiant effect. Use just one color or all three!Features: 3-in-1 multi-shade compactLightweight, blendable flush of colorClean and natural lookHow to Apply:  Begin with the contour, or darkest, shade in your palette and apply to the hollow space below your cheekbones.Apply the medium shade to the apples of your cheeks and sweep upwards toward the hairline.For a youthful glow, highlight the tops of your cheeks with the lightest shade.                            "
    ## 
    ## $data.description4
    ## [1] "Never stop blushing with CoverGirl's New truBLEND blush! Features:New marbled baked formulaUltra-blendable and delivers a beautiful, multi-toned result Designed to fit light, medium and deep skin tones alikeHow to Apply: Step 1: Lightly dip brush into blush, then tap or blow off excess powder. Step 2: Next, sweep the blush over and under your cheekbone to create a beautiful, rosy looking flush. "
    ## 
    ## $data.description5
    ## [1] "Blush's basic rules—it has to be easy, and it has to look natural. That's Cheekers. Each portable mini-compact gives you a sheer blush that goes on to stay true, stay for hours. It's the way to glow!Features: Stays for hoursEasy and naturalDermatologically-tested"
    ## 
    ## $data.description6
    ## [1] "Never stop blushing with CoverGirl New truBLEND blush! Features:New marbled baked formulaUltra-blendable and delivers a beautiful, multi-toned result Designed to fit light, medium and deep skin tones alikeHow to Apply: Step 1: Lightly dip brush into blush, then tap or blow off excess powder. Step 2: Next, sweep the blush over and under your cheekbone to create a beautiful, rosy looking flush. "
    ## 
    ## $data.description7
    ## [1] "Complete your Clean® look with a flush of lightweight, blendable color. This versatile 3-in-1 multi-shade compact lets you customize your color to create a radiant effect. Use just one color or all three!Features: 3-in-1 multi-shade compactLightweight, blendable flush of colorClean and natural lookHow to Apply:  Begin with the contour, or darkest, shade in your palette and apply to the hollow space below your cheekbones.Apply the medium shade to the apples of your cheeks and sweep upwards toward the hairline.For a youthful glow, highlight the tops of your cheeks with the lightest shade.                            "
    ## 
    ## $data.description8
    ## [1] "CoverGirl's New truBLEND Bronzer has a marble-baked formula that blends instantly with skin making bronzer lines a thing of the past.Features:Baked blend of colors for the perfect glowBaked to get the perfect bronze for every skin toneBlends instantly, no bronzer linesSuitable for sensitive skin How to Apply: Step 1: Lightly dip brush into bronzer, being careful to tap or blow off excess powder. Step 2: Next, sweep the bronzer over cheeks and temples to create a warm, even, sun-kissed glow. "
    ## 
    ## $data.description9
    ## [1] "Flaunt the coveted beach-bronze glow in one simple step. This versatile 3-in-1 multi-shade compact lets you customize your color to create a radiant effect. Use just one color or all three!Features: Beautiful bronze glow in just one stepBlendable, multi-shade compactClean and natural lookHow to Apply: Dip powder brush in bronzer and tap off any excess.Sweep bronzer on your cheeks and temples.Using a lighter touch, sweep bronzer where the sun naturally hits your face, including your nose, chin, and forehead.                            "
    ## 
    ## $data.description10
    ## [1] "The CoverGirl Ink It! By Perfect Point Plus gives you vibrant, high impact color that glides on smoothly with pen-like precision for intense eye looks that last all day.Features:Crease and fade resistantSelf-sharpening pencilWaterproofApplication Tips: Start at the inner corner of your eye and draw a line close to your lash line."
    ## 
    ## $data.description11
    ## [1] "Get inked — without the commitment! Ink It! colors glide on smoothly and easily without skipping, dragging, or pulling. The bright, bold shades last all day long.Features: Bright, bold colors for new looks Lasts all day long ­­— no smudging or creasingExpert precision applicationTwist up design"
    ## 
    ## $data.description12
    ## [1] "CoverGirl Trunaked Waterproof Eyeliner Duo takes eyes from subtle to smokey in no time! Features:Creamy waterproof formula Blends just as beautifully as it stays put Partner this twin tip eyeliner pencil with the gorgeous neutrals in the trunaked palettes to create endless easy, breezy, beautiful possibilitiesHow to Apply: Apply liner along your top and bottom lashes for the full trunaked effect."
    ## 
    ## $data.description13
    ## [1] "Bring sexy smolder to eyes & brows.  Soft powder adheres to brows and defines eyes.Features:2-in-1 brow & liner powderVelvety powder fills in browsUse also to softly line & define eyesDirections: Twist open and pull applicator up and tap lightly on tube side to remove excess powder. Lightly brush over eyebrows until desired effect is reached. Also use to softly line & define eyesIngredients: Boron Nitride, Dimethicone, Silica, Polyethylene, Pentylene Glycol, Pumice, Kaolin, Dimethiconol, Phenoxyethanol, Ethylhexylglycerin, Dehydroacetic Acid, Copernicia Cerifera (Carnauba) Wax/Cire De Carnauba, May Contain/Peut Contenir: Titanium Dioxide, Iron Oxides, Mica"
    ## 
    ## $data.description14
    ## [1] "CoverGirl Trunaked Waterproof Eyeliner Duo takes eyes from subtle to smokey in no time! Features:Creamy waterproof formula Blends just as beautifully as it stays put Partner this twin tip eyeliner pencil with the gorgeous neutrals in the trunaked palettes to create endless easy, breezy, beautiful possibilitiesHow to Apply: Apply liner along your top and bottom lashes for the full trunaked effect."
    ## 
    ## $data.description15
    ## [1] "This value pack contains a mascara that gives exceptional lash fullness that is soft - even to the touch and a self-sharpening eye liner for a perfect point every time.Features:Full Lash Bloom Mascara - very blackPerfect Point Plus Eye Liner - black onyx Soft, full lashes that are never stiff or brittleEye liner has soft smudger tip for expert blendingSuitable for contact lens wearersDirections: Apply liner along bottom and top lash lines, smudge using soft tip tool. Finish by brushing upper and lower lashes generously with up to 2 coats of mascara."
    ## 
    ## $data.description16
    ## [1] "With liquiline blast, you'll get all the intensity and staying power of a liquid with ease and blendability of a pencil. Use the liner as-is for intense definition or smoke-it-out with the built-in smudger end for a subtle, smoky look.Features: Liquid-like formula + the ease of a pencil applicatorSmudger tip lets you customize your intensity"
    ## 
    ## $data.description17
    ## [1] "The pencil gives you control and versatility. Want a precise line? It glides on easily. Want to soften a bit? There's a soft smudger tip. Best of all? No sharpening necessary, it's got a specially designed self-sharpener inside. So you always get the point—perfectly.Features: Specially-designed self-sharpenerSoft smudger tip for great blendingGlides on easilyOphthalmologically-tested"
    ## 
    ## $data.description18
    ## [1] "Bring sexy smolder to eyes & brows.  Soft powder adheres to brows and defines eyes.Features:2-in-1 brow & liner powderVelvety powder fills in browsUse also to softly line & define eyesDirections: Twist open and pull applicator up and tap lightly on tube side to remove excess powder. Lightly brush over eyebrows until desired effect is reached. Also use to softly line & define eyesIngredients: Boron Nitride, Dimethicone, Silica, Polyethylene, Pentylene Glycol, Pumice, Kaolin, Dimethiconol, Phenoxyethanol, Ethylhexylglycerin, Dehydroacetic Acid, Copernicia Cerifera (Carnauba) Wax/Cire De Carnauba, May Contain/Peut Contenir: Titanium Dioxide, Iron Oxides, Mica"
    ## 
    ## $data.description19
    ## [1] "Try CoverGirl Intensify Me! Liquid Liner and get the control of a pencil with the intensity of a liquid eyeliner for beautifully defined eyes every time. The all-new paddle-shaped tip gives you two looks in one—thick and dramatic or thin and elegant. The intense black formula glides on smoothly and dries quickly for instant drama. Clinically tested, suitable for sensitive eyes.Directions: Stand 6 inches away from a mirror and tilt your chin up, so that when you look down your nose into the mirror, you can see your entire eyelid. Dab concealer or dust translucent powder on your upper lids. Blend well with a sponge or lightly pat with a fingertip to cover veins and prevent creasing.Before applying liner, determine your eye shape. If eyes are close-set or round, line only the outer corners. Otherwise, line the entire upper lid.Gently pull your eyelid taut at an upward angle. Beginning at the inner corner, draw small, connected dashes along the top lashes. Be sure to stay close to the roots so there is no skin visible between the lashes and the liner. To make eyes look bigger, extend the liner slightly beyond the outer corners.Use a cotton swab to fix mistakes. For larger messes, dip it in makeup remover."
    ## 
    ## $data.description20
    ## [1] "Compact, easy to hold high precision liquid eyeliner pen has a smudgeproof, fast-drying formula that glides on to create fine or bold lines without fuss, for looks that last all day.Features: ExactTip lets you create bold or fine linesCompact, easy-to-holdFast-dryingSmudge-resistant, lasts all daySuitable for sensitive eyes "
    ## 
    ## $data.description21
    ## [1] "Get inked — without the commitment! Ink It! colors glide on smoothly and easily without skipping, dragging, or pulling. The bright, bold shades last all day long.Features: Bright, bold colors for new looks Lasts all day long ­­— no smudging or creasingExpert precision applicationTwist up design"
    ## 
    ## $data.description22
    ## [1] "This collection of great shades is handpicked by CoverGirl's makeup pros and designed to make your eye look go from day to night. They blend effortlessly to bring your eyes out beautifully, without overshadowing. Experiment using them together or as single shades.Features:Matte, pearl, and sparkle shadesHandpicked by CoverGirl's makeup pros to make your eye look go from day to nightBlends easily without overshadowingTips for Use:                                                                                                                                                                              Dust the most neutral shadow in your 3-Kit palette from your lash line to brow line. Blend the second-darkest shade along the crease of your eyes. Run the darkest shade along your upper and lower lash lines. Finish the look with two coats of mascara.                                                                                                         "
    ## 
    ## $data.description23
    ## [1] "This collection of great shades is handpicked by CoverGirl's makeup pros and designed to make your eye look go from day to night. They blend effortlessly to bring your eyes out beautifully, without overshadowing. Experiment using them together or as single shades.Features:Matte, pearl, and sparkle shadesHandpicked by CoverGirl's makeup pros to make your eye look go from day to nightBlends easily without overshadowingTips for Use:                                                                                       Dust the second-lightest shadow in your 4-Kit palette from your lash line to brow line Blend the second-darkest shade along the crease of your eyes Run the darkest shade along your upper and lower lash lines Blend and highlight the lid and brow bone with the lightest shade                                                     "
    ## 
    ## $data.description24
    ## [1] "Created with advanced, stay-put formulas, these collections of great shades are hand-picked by our make-up pros. Each one lets you mix and mingle, or just use a single shadow. They blend effortlessly to bring your eyes out beautifully, without overshadowing."
    ## 
    ## $data.description25
    ## [1] "CoverGirl Trunaked Eyeshadow Palettes feature universally flattering, neutral shades that are made to be blended together, so you can mix and match colors and always look flawless. Features:Each eye shadow palette comes with 8 shadesEach shade is highly pigmentedEndless possibilities for neutral daytime to dramatic nighttime looks"
    ## 
    ## $data.description26
    ## [1] "A simple, single shade with an advanced, stay-put formula!Mix and match all the CoverGirl Eye Enhancers 1-Kit Shadows the way you want to create your own combos with matte, pearly or sparkly shades! They blend easily to let the natural beauty of your eyes come through."
    ## 
    ## $data.description27
    ## [1] "CoverGirl Outlast Stay Fabulous 3-in-1 foundation is an all-in-one foundation with a long-lasting formula that fuses primer, concealer, and foundation in one easy step! The 3-in-1 formula provides a flawless look that lasts all day long.Features: All day, longwear formulaPrimer, concealer, and foundation in oneSmooth, flawless finishFoundation + Sunscreen SPF 20"
    ## 
    ## $data.description28
    ## [1] "CoverGirl Outlast Stay Fabulous 3-in-1 foundation is an all-in-one foundation with a long-lasting formula that fuses primer, concealer, and foundation in one easy step! The 3-in-1 formula provides a flawless look that lasts all day long.Features: All day, longwear formulaPrimer, concealer, and foundation in oneSmooth, flawless finishFoundation + Sunscreen SPF 20"
    ## 
    ## $data.description29
    ## [1] "With CoverGirl's Clean Oil Control Makeup, you'll get all the shine control you want – without the drying coverage you don't. The Clean foundation with gentle oil absorbing powders and skin conditioners, help prevent dryness. Best of all, it won't clog pores, for a clean, shine-free face, hour after hour!Features: Oil and fragrance freeNon-comedogenic (won’'t clog pores)Dermatologically-testedWon’'t clog pores"
    ## 
    ## $data.description30
    ## [1] "Take 5 years off the look of your skin.* This smooth liquid foundation with Olay ingredients blends into the skin, covering fine lines and wrinkles, for radiant, beautiful coverage that actually helps give you a youthful looking appearance.                                 * Tested among women 35-65.Features:Protects with SPF 10Suitable for sensitive skinDermatologically-testedNon-comedogenic (won't clog pores)Oil-free\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t  \t\t"
    ## 
    ## $data.description31
    ## [1] "This breakthrough formula with Olay Regenerist Serum and SPF 22 stays suspended over fine lines and wrinkles, unlike the leading anti-aging department store foundation that can gather in wrinkles and make skin look older. Clinically shown to provide significant improvement in skin condition in just four weeks.Features: SPF 22Stays suspended over fine lines and wrinklesEven coverageDermatologically-tested"
    ## 
    ## $data.description32
    ## [1] "Clean Makeup’s formula lets your skin breathe, goes on easily and blends perfectly, so the world doesn't see makeup, just the look of great skin. The sensitive skin formula is fragrance and oil free, and is suitable for sensitive skin. Features: Light coverage, not heavy or cakeyNon-comedogenic (won't clog pores)Dermatologically tested"
    ## 
    ## $data.description33
    ## [1] "CoverGirl Outlast Stay Luminous Foundation is perfect for achievinga dewy finish and a subtle glow. It is oil-free, with a non-greasy formula gives your skin a natural luminosity that lasts all day! This all day foundation hydrates skin while providing flawless coverage.Features: Oil-freeLong lastingGive your skin a natural glowing lookDirections: Apply primer first to help your look last all day.Dot foundation on forehead, cheeks and chin. Blend using fingertips, or a makeup sponge if desired.Use a Pressed Powder to finish and voila! Your look is ready to last."
    ## 
    ## $data.description34
    ## [1] "Smooth, even coverage makes imperfections virtually vanish before your eyes, while the vitamin-enriched formula helps direct moisture to your skin’s surface, improving your skin’s condition over time. This liquid foundation glides on effortlessly for lightweight coverage that lasts for hours.Features:HypoallergenicFragrance FreeOil-FreeWon't clog pores"
    ## 
    ## $data.description35
    ## [1] "CoverGirl Ready, Set Gorgeous foundation provides a flawless, natural look that lasts all day in one simple step!Features:Blends easily to even skin tone for a flawless lookShine free look lasts all dayOil free, won't clog poresApplication Tips: Dot foundation on forehead, cheeks and chin. Blend using fingertips or a makeup sponge. Use with any COVERGIRL Pressed Powder to help your look last."
    ## 
    ## $data.description36
    ## [1] "Get the beauty of a liquid, a powder and concealer in one makeup! Ultimate Finish glides on weightlessly to cover and conceal where you need it, then dries to the soft finish of a powder. For an ultimate natural look—beautiful and wondrous. It's all you need—all in one!Features: Oil and fragrance freeHypoallergenicDermatologically-tested"
    ## 
    ## $data.description37
    ## [1] "High conditioning lip liner. Created to work perfectly with Lip Perfection Lipstick, CoverGirl's lip liner formula contains over 60% skin conditioners, so you can line and define with conditioning colour.Features: Shades pair perfectly with Lip Perfection LipstickContains Vitamin EPencil requires sharpener"
    ## 
    ## $data.description38
    ## [1] "CoverGirl Colorlicious Lipstick gives you rich, satisfying color in shades you can't help but crave!Features: Deliciously rich color infused with shea butterFull coverage color, high shine intensityNatural butters help keep lips suppleLasting colourIngredients: Ricinus Communis (Castor) Seed Oil, Isopropyl Isostearate, Mica, Ethylhexyl Hydroxystearate, Acetylated Lanolin, Ozokerite, Euphorbia Cerifera (Candelilla) Wax/Cire De Candelilla, Paraffin, Squalane, Copernicia Cerifera (Carnauba) Wax/Cire De Carnauba, Cetyl Lactate, Cetyl Alcohol, Fragrance/Parfum, Ascorbyl Palmitate, Tocopheryl Acetate, Maltodextrin, Camellia Sinensis Leaf Extract, Butyrospermum Parkii (Shea) Butter, Silk Powder/Poudre De Soie, Persea Gratissima (Avocado) Oil, Cocos Nucifera (Coconut) Oil, Aloe Barbadensis Leaf Extract, Propylparaben, Hydrogenated Vegetable Oil, Retinyl Palmitate, Some Shades Also Contain/Certaines Teintes Contiennent Aussi: Lauroyl Lysine, Synthetic Fluorphlogopite, May Contain/Peut Contenir: Titanium Dioxide, Red 7 Lake, Yellow 5 Lake, Red 6 Lake, Iron Oxides, Blue 1 Lake, Carmine, Red 27 Lake, Yellow 6 Lake, Orange 5, Red 27"
    ## 
    ## $data.description39
    ## [1] "CoverGirls very popular longwear lip color just got better! The advanced formula with resilient, brilliant color and new applicator gives you up to 24 hours of color in just 2 easy, breezy steps.Features:Lasts up to 24 hoursBeautiful shades that are food proof and transfer resistant all day!Moisturizing topcoat keeps lips feeling moist, soft and smoothApplication Tips: Apply color to clean lips. Let dry 60 seconds. Apply moisturizing topcoat. Reapply topcoat like a balm throughout the day to refresh your look and feel."
    ## 
    ## $data.description40
    ## [1] "CoverGirl Outlast Lipcolor Moisturizing Clear Topcoat is a moisturizing lip gloss that keeps lips feeling moist, soft & smooth.Features: Moisturizing formulaSafe for sensitive skinFragrance freeIngredients: Sucrose Polycottonseedate, Ozokerite, Beeswax/ Cire D'Abeille, Tocopheryl Acetate, Tocopherol, Propyl Gallate, Propylparaben, Acetyl Glucosamine, Cocos Nucifera (Coconut Oil), Aloe Barbadensis Leaf Extract, Theobroma Cacao (Cocoa) Seed Butter, Butyrospermum Parkii (Shea Butter), Sodium Saccharin, Flavor/Aroma, Eu Inci, Sucrose Polycottonseedate, Ozokerite, Cera Alba, Tocopheryl Acetate, Tocopherol, Propylparaben, Propyl Gallate, Acetyl Glucosamine, Cocos Nucifera (Coconut Oil), Aloe Barbadensis Leaf Extract, Theobroma Cacao (Cocoa) Seed Butter, Butyrospermum Parkii (Shea Butter), Sodium Saccharin, Aroma"
    ## 
    ## $data.description41
    ## [1] "With CoverGirl Outlast Longwear Lipstick you get both moisture and colour! No need to choose!This long lasting lipstick doesn’t flake or crumble because it’s super-powered with moisture. It'll stays super fresh and super flexible all day, leaving you with a light weight but very pigmented lip look."
    ## 
    ## $data.description42
    ## [1] "Volumize, lengthen and define like a pro! CoverGirl Professional 3-in-1 mascara for all day beautiful lashes. The curved brush finds lashes you didn't know you had, for a volumized, long, defined lash look without the clumps or blobs.Features:Volume + length + definition in one easy stepHypoallergenic and suitable for contact lens wearersLash volume without clumpsApplication Tips: Hold brush as close as possible to base of lashes, and wiggle it as you move it up through lashes."
    ## 
    ## $data.description43
    ## [1] "Volumize, lengthen and define like a pro! CoverGirl Professional 3-in-1 mascara for all day beautiful lashes. The curved brush finds lashes you didn't know you had, for a volumized, long, defined lash look without the clumps or blobs.Features:Volume + length + definition in one easy stepHypoallergenic and suitable for contact lens wearersLash volume without clumpsApplication Tips: Hold brush as close as possible to base of lashes, and wiggle it as you move it up through lashes."
    ## 
    ## $data.description44
    ## [1] "Volumize, lengthen and define like a pro! CoverGirl Professional 3-in-1 mascara for all day beautiful lashes.Features:Volume + length + definition in one easy stepHypoallergenic and suitable for contact lens wearersWaterproofApplication Tips: Hold brush as close as possible to base of lashes, and wiggle it as you move it up through lashes."
    ## 
    ## $data.description45
    ## [1] "Get beautiful, thick, high-impact lashes with COVERGIRL professional super thick lash mascara! The brush separates, defines, and reaches those little lashes so you get bold, defined lashes so easy, so beautiful!Features:Bold, defined volumeSeparates and defines lashesLasts all dayApplication Tips: Hold brush as close as possible to base of lashes, and wiggle it as you move it up through lashes."
    ## 
    ## $data.description46
    ## [1] "CoverGirl LashBlast Fusion Water Resistant Mascara has a fiberstrech lengthening formula and super-volumizing brush that thickens and extends each individual lash for high volume, length and perfect separation rain or shine.Features:Super volumizing and buildable fiber-strength lengtheningThickens and extends the look of your bare lashesWater resistant"
    ## 
    ## $data.description47
    ## [1] "A breakthrough in no-clump mascara! Get 200% more volume and zero clumps. Features an innovative double-sided brush with lash-loading and clump-combing zones to crush clumps.Features: 200% more volume, zero clumpsInnovative curved brushSuper-volumized, beautifully separated lashes"
    ## 
    ## $data.description48
    ## [1] "For natural beautiful looking lashes! The CoverGirl clear gel formula and curved brush separates lashes while conditioning. It helps tame unruly brows too!Features:Natural looking lashesBeautifully tamed browsLasts all dayApplication Tips: Hold brush as close as possible to base of lashes, and wiggle it as you move it up through lashes."
    ## 
    ## $data.description49
    ## [1] "A breakthrough in no-clump mascara! Get 200% more volume and zero clumps. Features an innovative double-sided brush with lash-loading and clump-combing zones to crush clumps.Features: 200% more volume, zero clumpsInnovative curved brushSuper-volumized, beautifully separated lashesWater resistant"
    ## 
    ## $data.description50
    ## [1] "With CoverGirl Full Lash Bloom Mascara, get exceptional lash fullness that's soft - even to the touch. With CoverGirl's petal-shaped brush and mousse formula with natural beeswax, lashes are never stiff or brittle. Perfect for everyday wear, Full Lash Bloom Mascara is also safe for contact lens wearers and is ophthalmologically tested. So bring out every last lash, and embrace you in full bloom!Features: Gives soft, full lashes that are never stiff or brittle.Petal shaped brush and soft, mousse formula, with natural beeswax, brings out every lashOphthalmologically tested and suitable for contact lens wearersIngredients: Water/Eau, Beeswax/Cire D'Abeille, Cetearyl Alcohol, Shellac, Copernicia Cerifera (Carnauba) Wax/Cire De Carnauba, Ceteareth-20, Glycerin, Tapioca Starch, Lecithin, Ethylhexylglycerin, Alcohol Denat., Phenoxyethanol, Benzyl Alcohol, Xanthan Gum, Ammonium Hydroxide, Sodium Gluconate May Contain: Titanium Dioxide, Iron Oxides"
    ## 
    ## $data.description51
    ## [1] "Get a blast of lush, volumized lashes! LashBlast’s patented volume-boosting formula and patent-pending brush are designed to max-out each and every lash, leaving you with the ultimate big lash look.Features: Instant volume and fullnessSmudge and smear-proofHypoallergenic and suitable for contact lens wearers"
    ## 
    ## $data.description52
    ## [1] "Get a blast of lush, volumized lashes! LashBlast’s patented volume-boosting formula and patent-pending brush are designed to max-out each and every lash, leaving you with the ultimate big lash look.Features: Instant volume and fullnessSmudge and smear-proofHypoallergenic and suitable for contact lens wearers"
    ## 
    ## $data.description53
    ## [1] "Experience COVERGIRL's first ever volume + length mascara!The fiberstretch formula and oversized brush make every little lash bigger, fuller, longer -looking and more dramatic.* \t\t\t\t\t\t\t                            \t                                *vs bare lashesFeatures: Super-Volumizing + Buildable fiber-strength lengtheningThickens and extends the look of your natural lashes"
    ## 
    ## $data.description54
    ## [1] "Dare to go topless with CoverGirl Outlast Stay Brilliant Nail Gloss. New high-gloss nail polish formula that combines a base coat, high-gloss color, and a chip-resistant top coat in one simple step. Features:High gloss colourLasts as long as one week3-in-1 formula - base coat, colour, and top coatQuick, easy and convenientHow to Apply:Step 1: File nails to your desired length and shape.Step 2: Brush your favorite nail color on top of each nail. Allow nail polish to fully dry.Step 3: Apply a second coat for deeper pigment."
    ## 
    ## $data.image_link1
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/5361038d5de79bedaca6c64b48271161_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link2
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/1d1668b02148a8823eb19c1ce4157a4e_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link3
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/48492123aba0c6d096945d57a070361d_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link4
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/d8969d390fd94976aee71df780f512f2_ra,w158,h184_pa,w158,h184.png"
    ## 
    ## $data.image_link5
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/d095a226cf2698dba1eba4c6e2ce9896_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link6
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/0b8787d62ced45700c0693b869645542_ra,w158,h184_pa,w158,h184.png"
    ## 
    ## $data.image_link7
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/bb0b0c2057205b8e39823c5c7fc6af45_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link8
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/60f9f4f29be5221ff70cf20fabc03564_ra,w158,h184_pa,w158,h184.png"
    ## 
    ## $data.image_link9
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/4e90929a9c24a51c3db3988b205f3b43_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link10
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/5b4b8ad9a7d0264fa6b66d4027aa0f7e_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link11
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/e4763257a2290da070bd300212d16db8_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link12
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/d21a214b11528337f27647cbbd93de6b_ra,w158,h184_pa,w158,h184.png"
    ## 
    ## $data.image_link13
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/a7598160bc46504c98c26a241e3047e2_ra,w158,h184_pa,w158,h184.png"
    ## 
    ## $data.image_link14
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/92d2bd2c93f465c5565009f3f604fae6_ra,w158,h184_pa,w158,h184.png"
    ## 
    ## $data.image_link15
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/72bab061829b3185368d8dcb24a6a630_ra,w158,h184_pa,w158,h184.png"
    ## 
    ## $data.image_link16
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/0e4882c00b6639844d9168d71f43c3ed_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link17
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/74749bda64dcfe89dfdbf3f2f248cd3a_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link18
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/49400a1459a15523535e61d0db29e57d_ra,w158,h184_pa,w158,h184.png"
    ## 
    ## $data.image_link19
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/28f0f546d02a7c4330b4d186d42e3c32_ra,w158,h184_pa,w158,h184.jpg"
    ## 
    ## $data.image_link20
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/f56b44752218960e8ff15d1364510544_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link21
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/db7299d1c75f3e217b911e9a7be8783c_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link22
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/e48da5acde28bd3b1bcc2bff5b9d4a56_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link23
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/7d13530ed0a2ec9edc3736dcc0944a6b_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link24
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/197ae5defd213aa7967e20756d370be8_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link25
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/e8be715d17f0bcc25ffe81f4286ffac0_ra,w158,h184_pa,w158,h184.png"
    ## 
    ## $data.image_link26
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/e1e4b13a264fe9a7cbccaea373c24d9d_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link27
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/f047b24948adf8c9d5f6b795db107920_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link28
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/5b6bdfe942b6ffccbd2ca3f37e500744_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link29
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/700e432a504f4784792e84d498c982b7_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link30
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/9b13eee88cc626be90db443abb6e8bb9_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link31
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/fd47eaa9241a010e48fc32cf4611d772_ra,w158,h184_pa,w158,h184.png"
    ## 
    ## $data.image_link32
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/19422d63bf6e51c128bc2fa457fa61dc_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link33
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/b074403f4a1c12a7920199e4d8fabe78_ra,w158,h184_pa,w158,h184.jpg"
    ## 
    ## $data.image_link34
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/edd3139c31a9f0561b93d1b68b21a442_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link35
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/414eb9180f66dd8a75ddd34b307e3a36_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link36
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/1aa7e9e399d24c2a747137df51026ed2_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link37
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/9b00f1bed71b9d46399663a8ef364a9a_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link38
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/c88809fbffe05fb9594103e635387152_ra,w158,h184_pa,w158,h184.jpg"
    ## 
    ## $data.image_link39
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/be04329866e96a0077545d230489d7a9_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link40
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/00bee78599bf386be435237a1515fdb7_ra,w158,h184_pa,w158,h184.jpg"
    ## 
    ## $data.image_link41
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/2d46e82f21b11f658a4378abcbd1c31b_ra,w158,h184_pa,w158,h184.png"
    ## 
    ## $data.image_link42
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/c28f5756cc3f81f36baebfd753207c58_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link43
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/c7fe159f81fff4e0b8269782b20048ac_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link44
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/d44d5338e9dfa2b5234a02d25af64a26_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link45
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/cb7bdcebdc593ba34c0f40b41a3ff44c_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link46
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/e4bd2eaff5e2b0c9c8c29ecd86d0c7e6_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link47
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/df2f212e8449edcaff4876f592c4311e_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link48
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/fa655a2751adc3d38050d08325f2d97f_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link49
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/b02f002545c5dc18be53297faaf04b7a_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link50
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/bd505b8626119f31924057a086e542bf_ra,w158,h184_pa,w158,h184.jpg"
    ## 
    ## $data.image_link51
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/527f52aaaba663227d70de7c0a34ff80_ra,w158,h184_pa,w158,h184.jpg"
    ## 
    ## $data.image_link52
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/18790b1fce6ddd2c7c20e73ce964eadd_ra,w158,h184_pa,w158,h184.jpg"
    ## 
    ## $data.image_link53
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/fcd915ee80b8b9cfaa71225020b8df3b_ra,w158,h184_pa,w158,h184.jpg"
    ## 
    ## $data.image_link54
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/ad5297ce5c35c04daa3c85b6906aaedc_ra,w158,h184_pa,w158,h184.png"
    ## 
    ## $data.Cosmetic_Brand1
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand2
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand3
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand4
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand5
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand6
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand7
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand8
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand9
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand10
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand11
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand12
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand13
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand14
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand15
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand16
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand17
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand18
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand19
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand20
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand21
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand22
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand23
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand24
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand25
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand26
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand27
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand28
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand29
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand30
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand31
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand32
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand33
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand34
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand35
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand36
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand37
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand38
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand39
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand40
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand41
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand42
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand43
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand44
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand45
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand46
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand47
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand48
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand49
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand50
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand51
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand52
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand53
    ## [1] 13
    ## 
    ## $data.Cosmetic_Brand54
    ## [1] 13
    ## 
    ## $data.Cosmetic_Type1
    ## [1] 1
    ## 
    ## $data.Cosmetic_Type2
    ## [1] 1
    ## 
    ## $data.Cosmetic_Type3
    ## [1] 1
    ## 
    ## $data.Cosmetic_Type4
    ## [1] 1
    ## 
    ## $data.Cosmetic_Type5
    ## [1] 1
    ## 
    ## $data.Cosmetic_Type6
    ## [1] 1
    ## 
    ## $data.Cosmetic_Type7
    ## [1] 1
    ## 
    ## $data.Cosmetic_Type8
    ## [1] 2
    ## 
    ## $data.Cosmetic_Type9
    ## [1] 2
    ## 
    ## $data.Cosmetic_Type10
    ## [1] 3
    ## 
    ## $data.Cosmetic_Type11
    ## [1] 3
    ## 
    ## $data.Cosmetic_Type12
    ## [1] 3
    ## 
    ## $data.Cosmetic_Type13
    ## [1] 3
    ## 
    ## $data.Cosmetic_Type14
    ## [1] 3
    ## 
    ## $data.Cosmetic_Type15
    ## [1] 3
    ## 
    ## $data.Cosmetic_Type16
    ## [1] 3
    ## 
    ## $data.Cosmetic_Type17
    ## [1] 3
    ## 
    ## $data.Cosmetic_Type18
    ## [1] 3
    ## 
    ## $data.Cosmetic_Type19
    ## [1] 3
    ## 
    ## $data.Cosmetic_Type20
    ## [1] 3
    ## 
    ## $data.Cosmetic_Type21
    ## [1] 3
    ## 
    ## $data.Cosmetic_Type22
    ## [1] 4
    ## 
    ## $data.Cosmetic_Type23
    ## [1] 4
    ## 
    ## $data.Cosmetic_Type24
    ## [1] 4
    ## 
    ## $data.Cosmetic_Type25
    ## [1] 4
    ## 
    ## $data.Cosmetic_Type26
    ## [1] 4
    ## 
    ## $data.Cosmetic_Type27
    ## [1] 5
    ## 
    ## $data.Cosmetic_Type28
    ## [1] 5
    ## 
    ## $data.Cosmetic_Type29
    ## [1] 5
    ## 
    ## $data.Cosmetic_Type30
    ## [1] 5
    ## 
    ## $data.Cosmetic_Type31
    ## [1] 5
    ## 
    ## $data.Cosmetic_Type32
    ## [1] 5
    ## 
    ## $data.Cosmetic_Type33
    ## [1] 5
    ## 
    ## $data.Cosmetic_Type34
    ## [1] 5
    ## 
    ## $data.Cosmetic_Type35
    ## [1] 5
    ## 
    ## $data.Cosmetic_Type36
    ## [1] 5
    ## 
    ## $data.Cosmetic_Type37
    ## [1] 6
    ## 
    ## $data.Cosmetic_Type38
    ## [1] 7
    ## 
    ## $data.Cosmetic_Type39
    ## [1] 7
    ## 
    ## $data.Cosmetic_Type40
    ## [1] 7
    ## 
    ## $data.Cosmetic_Type41
    ## [1] 7
    ## 
    ## $data.Cosmetic_Type42
    ## [1] 8
    ## 
    ## $data.Cosmetic_Type43
    ## [1] 8
    ## 
    ## $data.Cosmetic_Type44
    ## [1] 8
    ## 
    ## $data.Cosmetic_Type45
    ## [1] 8
    ## 
    ## $data.Cosmetic_Type46
    ## [1] 8
    ## 
    ## $data.Cosmetic_Type47
    ## [1] 8
    ## 
    ## $data.Cosmetic_Type48
    ## [1] 8
    ## 
    ## $data.Cosmetic_Type49
    ## [1] 8
    ## 
    ## $data.Cosmetic_Type50
    ## [1] 8
    ## 
    ## $data.Cosmetic_Type51
    ## [1] 8
    ## 
    ## $data.Cosmetic_Type52
    ## [1] 8
    ## 
    ## $data.Cosmetic_Type53
    ## [1] 8
    ## 
    ## $data.Cosmetic_Type54
    ## [1] 9
    ## 
    ## $layers
    ## geom_point: na.rm = FALSE
    ## stat_identity: na.rm = FALSE
    ## position_jitter 
    ## 
    ## $scales
    ## <ggproto object: Class ScalesList, gg>
    ##     add: function
    ##     clone: function
    ##     find: function
    ##     get_scales: function
    ##     has_scale: function
    ##     input: function
    ##     n: function
    ##     non_position_scales: function
    ##     scales: list
    ##     super:  <ggproto object: Class ScalesList, gg>
    ## 
    ## $mapping.x
    ## <quosure>
    ## expr: ^usd_price
    ## env:  0x0000023dcc029bc8
    ## 
    ## $mapping.y
    ## <quosure>
    ## expr: ^rating
    ## env:  0x0000023dcc029bc8
    ## 
    ## $coordinates
    ## <ggproto object: Class CoordCartesian, Coord, gg>
    ##     aspect: function
    ##     backtransform_range: function
    ##     clip: on
    ##     default: TRUE
    ##     distance: function
    ##     expand: TRUE
    ##     is_free: function
    ##     is_linear: function
    ##     labels: function
    ##     limits: list
    ##     modify_scales: function
    ##     range: function
    ##     render_axis_h: function
    ##     render_axis_v: function
    ##     render_bg: function
    ##     render_fg: function
    ##     setup_data: function
    ##     setup_layout: function
    ##     setup_panel_guides: function
    ##     setup_panel_params: function
    ##     setup_params: function
    ##     train_panel_guides: function
    ##     transform: function
    ##     super:  <ggproto object: Class CoordCartesian, Coord, gg>
    ## 
    ## $facet
    ## <ggproto object: Class FacetNull, Facet, gg>
    ##     compute_layout: function
    ##     draw_back: function
    ##     draw_front: function
    ##     draw_labels: function
    ##     draw_panels: function
    ##     finish_data: function
    ##     init_scales: function
    ##     map_data: function
    ##     params: list
    ##     setup_data: function
    ##     setup_params: function
    ##     shrink: TRUE
    ##     train_scales: function
    ##     vars: function
    ##     super:  <ggproto object: Class FacetNull, Facet, gg>
    ## 
    ## $plot_env
    ## <environment: 0x0000023dcc029bc8>
    ## 
    ## $labels.y
    ## [1] "Rating"
    ## 
    ## $labels.x
    ## [1] "USA Price"
    ## 
    ## $labels.title
    ## [1] "Scatter Plot of the Relationship between Price vs Rating for covergirl"
    ## 
    ## $data.brand1
    ## [1] "l'oreal"
    ## 
    ## $data.brand2
    ## [1] "l'oreal"
    ## 
    ## $data.brand3
    ## [1] "l'oreal"
    ## 
    ## $data.brand4
    ## [1] "l'oreal"
    ## 
    ## $data.brand5
    ## [1] "l'oreal"
    ## 
    ## $data.brand6
    ## [1] "l'oreal"
    ## 
    ## $data.brand7
    ## [1] "l'oreal"
    ## 
    ## $data.brand8
    ## [1] "l'oreal"
    ## 
    ## $data.brand9
    ## [1] "l'oreal"
    ## 
    ## $data.brand10
    ## [1] "l'oreal"
    ## 
    ## $data.brand11
    ## [1] "l'oreal"
    ## 
    ## $data.brand12
    ## [1] "l'oreal"
    ## 
    ## $data.brand13
    ## [1] "l'oreal"
    ## 
    ## $data.brand14
    ## [1] "l'oreal"
    ## 
    ## $data.brand15
    ## [1] "l'oreal"
    ## 
    ## $data.brand16
    ## [1] "l'oreal"
    ## 
    ## $data.brand17
    ## [1] "l'oreal"
    ## 
    ## $data.brand18
    ## [1] "l'oreal"
    ## 
    ## $data.brand19
    ## [1] "l'oreal"
    ## 
    ## $data.brand20
    ## [1] "l'oreal"
    ## 
    ## $data.brand21
    ## [1] "l'oreal"
    ## 
    ## $data.brand22
    ## [1] "l'oreal"
    ## 
    ## $data.brand23
    ## [1] "l'oreal"
    ## 
    ## $data.brand24
    ## [1] "l'oreal"
    ## 
    ## $data.brand25
    ## [1] "l'oreal"
    ## 
    ## $data.brand26
    ## [1] "l'oreal"
    ## 
    ## $data.brand27
    ## [1] "l'oreal"
    ## 
    ## $data.brand28
    ## [1] "l'oreal"
    ## 
    ## $data.brand29
    ## [1] "l'oreal"
    ## 
    ## $data.brand30
    ## [1] "l'oreal"
    ## 
    ## $data.brand31
    ## [1] "l'oreal"
    ## 
    ## $data.brand32
    ## [1] "l'oreal"
    ## 
    ## $data.brand33
    ## [1] "l'oreal"
    ## 
    ## $data.brand34
    ## [1] "l'oreal"
    ## 
    ## $data.brand35
    ## [1] "l'oreal"
    ## 
    ## $data.brand36
    ## [1] "l'oreal"
    ## 
    ## $data.brand37
    ## [1] "l'oreal"
    ## 
    ## $data.brand38
    ## [1] "l'oreal"
    ## 
    ## $data.brand39
    ## [1] "l'oreal"
    ## 
    ## $data.brand40
    ## [1] "l'oreal"
    ## 
    ## $data.brand41
    ## [1] "l'oreal"
    ## 
    ## $data.brand42
    ## [1] "l'oreal"
    ## 
    ## $data.brand43
    ## [1] "l'oreal"
    ## 
    ## $data.brand44
    ## [1] "l'oreal"
    ## 
    ## $data.brand45
    ## [1] "l'oreal"
    ## 
    ## $data.brand46
    ## [1] "l'oreal"
    ## 
    ## $data.name1
    ## [1] "L'Oreal Paris Visible Lift Blur Blush "
    ## 
    ## $data.name2
    ## [1] "L'Oreal Paris True Match"
    ## 
    ## $data.name3
    ## [1] "L'Oreal Paris Infallible Pro-Contour Palette in Medium "
    ## 
    ## $data.name4
    ## [1] "L'Oreal Paris Infallible Silkissime Eyeliner"
    ## 
    ## $data.name5
    ## [1] "L'Oreal Paris Extra-Intense Liquid Pencil Eyeliner"
    ## 
    ## $data.name6
    ## [1] "L'Oreal Infallible Smokissime Powder Eyeliner Pen"
    ## 
    ## $data.name7
    ## [1] "L'Oreal Paris Infallible Never Fail Eye Liner"
    ## 
    ## $data.name8
    ## [1] "L'Oreal Infallible Super Slim 12H Liner"
    ## 
    ## $data.name9
    ## [1] "L'Oreal Paris Colour Riche Le Khol Eyeliner"
    ## 
    ## $data.name10
    ## [1] "L'Oreal Paris Infallible Black Velvet Liner"
    ## 
    ## $data.name11
    ## [1] "L'Oreal Telescopic Explosion Waterproof Eyeliner Black"
    ## 
    ## $data.name12
    ## [1] "L'Oreal Paris Lineur Intense Felt Tip Liquid"
    ## 
    ## $data.name13
    ## [1] "L'Oreal Colour Riche La Palette Nude in Nude 02"
    ## 
    ## $data.name14
    ## [1] "L'Oreal Paris True Match Lumi Cushion Foundation"
    ## 
    ## $data.name15
    ## [1] "L'Oreal Paris True Match Lumi Healthy Luminous Foundation"
    ## 
    ## $data.name16
    ## [1] "L'Oreal Paris Infallible Pro-Matte Foundation"
    ## 
    ## $data.name17
    ## [1] "L'Oreal Visible Lift Serum Absolute"
    ## 
    ## $data.name18
    ## [1] "L'Oreal Paris True Match Super Blendable Makeup "
    ## 
    ## $data.name19
    ## [1] "L'Oreal Paris Visible Lift Blur Foundation "
    ## 
    ## $data.name20
    ## [1] "L'Oreal Paris True Match Lumi Glow Liquid Foundation"
    ## 
    ## $data.name21
    ## [1] "L'Oreal Paris Colour Riche Lip Liner"
    ## 
    ## $data.name22
    ## [1] "L'Oreal Paris Infallible Never Fail Lip Liner"
    ## 
    ## $data.name23
    ## [1] "L'Oreal Paris Colour Riche Collection Exclusive Pure Reds"
    ## 
    ## $data.name24
    ## [1] "L'Oreal Color Riche Extraordinaire Lip Color"
    ## 
    ## $data.name25
    ## [1] "L'Oreal Paris Colour Riche Crystal Shine"
    ## 
    ## $data.name26
    ## [1] "L'Oreal Paris Colour Riche Lipstick"
    ## 
    ## $data.name27
    ## [1] "L'Oreal Infallible Lipstick "
    ## 
    ## $data.name28
    ## [1] "L'Oreal Paris Colour Riche Le Matte & La Lacque Lip Colour"
    ## 
    ## $data.name29
    ## [1] "L'Oreal Paris Colour Riche Collection Exclusive Lip Colour "
    ## 
    ## $data.name30
    ## [1] "L'Oreal Paris Voluminous Butterfly Midnight Black Mascara"
    ## 
    ## $data.name31
    ## [1] "L'Oreal Paris Voluminous Butterfly Mascara "
    ## 
    ## $data.name32
    ## [1] "L'Oreal Paris Telescopic Mascara"
    ## 
    ## $data.name33
    ## [1] "L'Oreal Paris Voluminous Volume Building Mascara"
    ## 
    ## $data.name34
    ## [1] "L'Oreal Paris Voluminous Million Lashes Excess Mascara "
    ## 
    ## $data.name35
    ## [1] "L'Oreal Paris Extra Volume Collagen Mascara"
    ## 
    ## $data.name36
    ## [1] "L'Oreal Paris Voluminous Million Lashes Mascara"
    ## 
    ## $data.name37
    ## [1] "L'Oreal Paris Double Extend Beauty Tubes"
    ## 
    ## $data.name38
    ## [1] "L'Oreal Infallible Nail Polish in Keep Magenta"
    ## 
    ## $data.name39
    ## [1] "L'Oreal Infallible Nail Polish in Always a Lady"
    ## 
    ## $data.name40
    ## [1] "L'Oreal Infallible Nail Polish in Iconic Indigo "
    ## 
    ## $data.name41
    ## [1] "L'Oreal Infallible Nail Polish in Ocean Infini"
    ## 
    ## $data.name42
    ## [1] "L'Oreal Infallible Nail Polish in Petale Revival"
    ## 
    ## $data.name43
    ## [1] "L'Oreal Infallible Nail Polish in Irresistible Bonbon"
    ## 
    ## $data.name44
    ## [1] "L'Oreal Colour Riche Gold Dust Nail Colour"
    ## 
    ## $data.name45
    ## [1] "L'Oreal Extraordinaire Gel Lacque Nail Colour"
    ## 
    ## $data.name46
    ## [1] "L'Oreal Paris Colour Riche Collection Exclusive Nail Colour"
    ## 
    ## $data.product_type1
    ## [1] "blush"
    ## 
    ## $data.product_type2
    ## [1] "blush"
    ## 
    ## $data.product_type3
    ## [1] "bronzer"
    ## 
    ## $data.product_type4
    ## [1] "eyeliner"
    ## 
    ## $data.product_type5
    ## [1] "eyeliner"
    ## 
    ## $data.product_type6
    ## [1] "eyeliner"
    ## 
    ## $data.product_type7
    ## [1] "eyeliner"
    ## 
    ## $data.product_type8
    ## [1] "eyeliner"
    ## 
    ## $data.product_type9
    ## [1] "eyeliner"
    ## 
    ## $data.product_type10
    ## [1] "eyeliner"
    ## 
    ## $data.product_type11
    ## [1] "eyeliner"
    ## 
    ## $data.product_type12
    ## [1] "eyeliner"
    ## 
    ## $data.product_type13
    ## [1] "eyeshadow"
    ## 
    ## $data.product_type14
    ## [1] "foundation"
    ## 
    ## $data.product_type15
    ## [1] "foundation"
    ## 
    ## $data.product_type16
    ## [1] "foundation"
    ## 
    ## $data.product_type17
    ## [1] "foundation"
    ## 
    ## $data.product_type18
    ## [1] "foundation"
    ## 
    ## $data.product_type19
    ## [1] "foundation"
    ## 
    ## $data.product_type20
    ## [1] "foundation"
    ## 
    ## $data.product_type21
    ## [1] "lip_liner"
    ## 
    ## $data.product_type22
    ## [1] "lip_liner"
    ## 
    ## $data.product_type23
    ## [1] "lipstick"
    ## 
    ## $data.product_type24
    ## [1] "lipstick"
    ## 
    ## $data.product_type25
    ## [1] "lipstick"
    ## 
    ## $data.product_type26
    ## [1] "lipstick"
    ## 
    ## $data.product_type27
    ## [1] "lipstick"
    ## 
    ## $data.product_type28
    ## [1] "lipstick"
    ## 
    ## $data.product_type29
    ## [1] "lipstick"
    ## 
    ## $data.product_type30
    ## [1] "mascara"
    ## 
    ## $data.product_type31
    ## [1] "mascara"
    ## 
    ## $data.product_type32
    ## [1] "mascara"
    ## 
    ## $data.product_type33
    ## [1] "mascara"
    ## 
    ## $data.product_type34
    ## [1] "mascara"
    ## 
    ## $data.product_type35
    ## [1] "mascara"
    ## 
    ## $data.product_type36
    ## [1] "mascara"
    ## 
    ## $data.product_type37
    ## [1] "mascara"
    ## 
    ## $data.product_type38
    ## [1] "nail_polish"
    ## 
    ## $data.product_type39
    ## [1] "nail_polish"
    ## 
    ## $data.product_type40
    ## [1] "nail_polish"
    ## 
    ## $data.product_type41
    ## [1] "nail_polish"
    ## 
    ## $data.product_type42
    ## [1] "nail_polish"
    ## 
    ## $data.product_type43
    ## [1] "nail_polish"
    ## 
    ## $data.product_type44
    ## [1] "nail_polish"
    ## 
    ## $data.product_type45
    ## [1] "nail_polish"
    ## 
    ## $data.product_type46
    ## [1] "nail_polish"
    ## 
    ## $data.usd_price1
    ## [1] 15.99
    ## 
    ## $data.usd_price2
    ## [1] 16.49
    ## 
    ## $data.usd_price3
    ## [1] 19.99
    ## 
    ## $data.usd_price4
    ## [1] 11.99
    ## 
    ## $data.usd_price5
    ## [1] 10.99
    ## 
    ## $data.usd_price6
    ## [1] 12.99
    ## 
    ## $data.usd_price7
    ## [1] 12.99
    ## 
    ## $data.usd_price8
    ## [1] 11.99
    ## 
    ## $data.usd_price9
    ## [1] 9.29
    ## 
    ## $data.usd_price10
    ## [1] 13.99
    ## 
    ## $data.usd_price11
    ## [1] 12.99
    ## 
    ## $data.usd_price12
    ## [1] 10.99
    ## 
    ## $data.usd_price13
    ## [1] 29.99
    ## 
    ## $data.usd_price14
    ## [1] 26.99
    ## 
    ## $data.usd_price15
    ## [1] 20.99
    ## 
    ## $data.usd_price16
    ## [1] 19.99
    ## 
    ## $data.usd_price17
    ## [1] 20.49
    ## 
    ## $data.usd_price18
    ## [1] 18.79
    ## 
    ## $data.usd_price19
    ## [1] 18.99
    ## 
    ## $data.usd_price20
    ## [1] 16.99
    ## 
    ## $data.usd_price21
    ## [1] 11.29
    ## 
    ## $data.usd_price22
    ## [1] 11.99
    ## 
    ## $data.usd_price23
    ## [1] 10.99
    ## 
    ## $data.usd_price24
    ## [1] 13.96
    ## 
    ## $data.usd_price25
    ## [1] 10.99
    ## 
    ## $data.usd_price26
    ## [1] 10.99
    ## 
    ## $data.usd_price27
    ## [1] 13.99
    ## 
    ## $data.usd_price28
    ## [1] 11.49
    ## 
    ## $data.usd_price29
    ## [1] 10.99
    ## 
    ## $data.usd_price30
    ## [1] 13.49
    ## 
    ## $data.usd_price31
    ## [1] 13.49
    ## 
    ## $data.usd_price32
    ## [1] 13.79
    ## 
    ## $data.usd_price33
    ## [1] 9.99
    ## 
    ## $data.usd_price34
    ## [1] 13.99
    ## 
    ## $data.usd_price35
    ## [1] 13.99
    ## 
    ## $data.usd_price36
    ## [1] 13.29
    ## 
    ## $data.usd_price37
    ## [1] 14.99
    ## 
    ## $data.usd_price38
    ## [1] 10.99
    ## 
    ## $data.usd_price39
    ## [1] 10.99
    ## 
    ## $data.usd_price40
    ## [1] 10.99
    ## 
    ## $data.usd_price41
    ## [1] 10.99
    ## 
    ## $data.usd_price42
    ## [1] 10.99
    ## 
    ## $data.usd_price43
    ## [1] 10.99
    ## 
    ## $data.usd_price44
    ## [1] 7.79
    ## 
    ## $data.usd_price45
    ## [1] 8.99
    ## 
    ## $data.usd_price46
    ## [1] 7.79
    ## 
    ## $data.price_stat1
    ## [1] "This blush's price is $15.99 which is lower than the overall blush price mean (18.49), higher than the price median(15.25), higher than the 25th percentile(9.99), and lower than the 75th percentile(24.12)."
    ## 
    ## $data.price_stat2
    ## [1] "This blush's price is $16.49 which is lower than the overall blush price mean (18.49), higher than the price median(15.25), higher than the 25th percentile(9.99), and lower than the 75th percentile(24.12)."
    ## 
    ## $data.price_stat3
    ## [1] "This bronzer's price is $19.99 which is lower than the overall bronzer price mean (23.54), lower than the price median(20.99), higher than the 25th percentile(11.24), and lower than the 75th percentile(32)."
    ## 
    ## $data.price_stat4
    ## [1] "This eyeliner's price is $11.99 which is lower than the overall eyeliner price mean (13.19), higher than the price median(11), higher than the 25th percentile(8), and lower than the 75th percentile(16.99)."
    ## 
    ## $data.price_stat5
    ## [1] "This eyeliner's price is $10.99 which is lower than the overall eyeliner price mean (13.19), lower than the price median(11), higher than the 25th percentile(8), and lower than the 75th percentile(16.99)."
    ## 
    ## $data.price_stat6
    ## [1] "This eyeliner's price is $12.99 which is lower than the overall eyeliner price mean (13.19), higher than the price median(11), higher than the 25th percentile(8), and lower than the 75th percentile(16.99)."
    ## 
    ## $data.price_stat7
    ## [1] "This eyeliner's price is $12.99 which is lower than the overall eyeliner price mean (13.19), higher than the price median(11), higher than the 25th percentile(8), and lower than the 75th percentile(16.99)."
    ## 
    ## $data.price_stat8
    ## [1] "This eyeliner's price is $11.99 which is lower than the overall eyeliner price mean (13.19), higher than the price median(11), higher than the 25th percentile(8), and lower than the 75th percentile(16.99)."
    ## 
    ## $data.price_stat9
    ## [1] "This eyeliner's price is $9.29 which is lower than the overall eyeliner price mean (13.19), lower than the price median(11), higher than the 25th percentile(8), and lower than the 75th percentile(16.99)."
    ## 
    ## $data.price_stat10
    ## [1] "This eyeliner's price is $13.99 which is higher than the overall eyeliner price mean (13.19), higher than the price median(11), higher than the 25th percentile(8), and lower than the 75th percentile(16.99)."
    ## 
    ## $data.price_stat11
    ## [1] "This eyeliner's price is $12.99 which is lower than the overall eyeliner price mean (13.19), higher than the price median(11), higher than the 25th percentile(8), and lower than the 75th percentile(16.99)."
    ## 
    ## $data.price_stat12
    ## [1] "This eyeliner's price is $10.99 which is lower than the overall eyeliner price mean (13.19), lower than the price median(11), higher than the 25th percentile(8), and lower than the 75th percentile(16.99)."
    ## 
    ## $data.price_stat13
    ## [1] "This eyeshadow's price is $29.99 which is higher than the overall eyeshadow price mean (22.15), higher than the price median(17.74), higher than the 25th percentile(9.98), and higher than the 75th percentile(28)."
    ## 
    ## $data.price_stat14
    ## [1] "This foundation's price is $26.99 which is higher than the overall foundation price mean (21.41), higher than the price median(19.99), higher than the 25th percentile(12), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat15
    ## [1] "This foundation's price is $20.99 which is lower than the overall foundation price mean (21.41), higher than the price median(19.99), higher than the 25th percentile(12), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat16
    ## [1] "This foundation's price is $19.99 which is lower than the overall foundation price mean (21.41), equal to the price median(19.99), higher than the 25th percentile(12), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat17
    ## [1] "This foundation's price is $20.49 which is lower than the overall foundation price mean (21.41), higher than the price median(19.99), higher than the 25th percentile(12), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat18
    ## [1] "This foundation's price is $18.79 which is lower than the overall foundation price mean (21.41), lower than the price median(19.99), higher than the 25th percentile(12), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat19
    ## [1] "This foundation's price is $18.99 which is lower than the overall foundation price mean (21.41), lower than the price median(19.99), higher than the 25th percentile(12), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat20
    ## [1] "This foundation's price is $16.99 which is lower than the overall foundation price mean (21.41), lower than the price median(19.99), higher than the 25th percentile(12), and lower than the 75th percentile(28)."
    ## 
    ## $data.price_stat21
    ## [1] "This lip_liner's price is $11.29 which is higher than the overall lip_liner price mean (10.16), higher than the price median(9.99), higher than the 25th percentile(4.99), and lower than the 75th percentile(12.99)."
    ## 
    ## $data.price_stat22
    ## [1] "This lip_liner's price is $11.99 which is higher than the overall lip_liner price mean (10.16), higher than the price median(9.99), higher than the 25th percentile(4.99), and lower than the 75th percentile(12.99)."
    ## 
    ## $data.price_stat23
    ## [1] "This lipstick's price is $10.99 which is lower than the overall lipstick price mean (15.95), lower than the price median(12), higher than the 25th percentile(8), and lower than the 75th percentile(21)."
    ## 
    ## $data.price_stat24
    ## [1] "This lipstick's price is $13.96 which is lower than the overall lipstick price mean (15.95), higher than the price median(12), higher than the 25th percentile(8), and lower than the 75th percentile(21)."
    ## 
    ## $data.price_stat25
    ## [1] "This lipstick's price is $10.99 which is lower than the overall lipstick price mean (15.95), lower than the price median(12), higher than the 25th percentile(8), and lower than the 75th percentile(21)."
    ## 
    ## $data.price_stat26
    ## [1] "This lipstick's price is $10.99 which is lower than the overall lipstick price mean (15.95), lower than the price median(12), higher than the 25th percentile(8), and lower than the 75th percentile(21)."
    ## 
    ## $data.price_stat27
    ## [1] "This lipstick's price is $13.99 which is lower than the overall lipstick price mean (15.95), higher than the price median(12), higher than the 25th percentile(8), and lower than the 75th percentile(21)."
    ## 
    ## $data.price_stat28
    ## [1] "This lipstick's price is $11.49 which is lower than the overall lipstick price mean (15.95), lower than the price median(12), higher than the 25th percentile(8), and lower than the 75th percentile(21)."
    ## 
    ## $data.price_stat29
    ## [1] "This lipstick's price is $10.99 which is lower than the overall lipstick price mean (15.95), lower than the price median(12), higher than the 25th percentile(8), and lower than the 75th percentile(21)."
    ## 
    ## $data.price_stat30
    ## [1] "This mascara's price is $13.49 which is lower than the overall mascara price mean (15.39), higher than the price median(12.99), higher than the 25th percentile(9), and lower than the 75th percentile(22)."
    ## 
    ## $data.price_stat31
    ## [1] "This mascara's price is $13.49 which is lower than the overall mascara price mean (15.39), higher than the price median(12.99), higher than the 25th percentile(9), and lower than the 75th percentile(22)."
    ## 
    ## $data.price_stat32
    ## [1] "This mascara's price is $13.79 which is lower than the overall mascara price mean (15.39), higher than the price median(12.99), higher than the 25th percentile(9), and lower than the 75th percentile(22)."
    ## 
    ## $data.price_stat33
    ## [1] "This mascara's price is $9.99 which is lower than the overall mascara price mean (15.39), lower than the price median(12.99), higher than the 25th percentile(9), and lower than the 75th percentile(22)."
    ## 
    ## $data.price_stat34
    ## [1] "This mascara's price is $13.99 which is lower than the overall mascara price mean (15.39), higher than the price median(12.99), higher than the 25th percentile(9), and lower than the 75th percentile(22)."
    ## 
    ## $data.price_stat35
    ## [1] "This mascara's price is $13.99 which is lower than the overall mascara price mean (15.39), higher than the price median(12.99), higher than the 25th percentile(9), and lower than the 75th percentile(22)."
    ## 
    ## $data.price_stat36
    ## [1] "This mascara's price is $13.29 which is lower than the overall mascara price mean (15.39), higher than the price median(12.99), higher than the 25th percentile(9), and lower than the 75th percentile(22)."
    ## 
    ## $data.price_stat37
    ## [1] "This mascara's price is $14.99 which is lower than the overall mascara price mean (15.39), higher than the price median(12.99), higher than the 25th percentile(9), and lower than the 75th percentile(22)."
    ## 
    ## $data.price_stat38
    ## [1] "This nail_polish's price is $10.99 which is lower than the overall nail_polish price mean (14.1), equal to the price median(10.99), higher than the 25th percentile(8.14), and lower than the 75th percentile(22.24)."
    ## 
    ## $data.price_stat39
    ## [1] "This nail_polish's price is $10.99 which is lower than the overall nail_polish price mean (14.1), equal to the price median(10.99), higher than the 25th percentile(8.14), and lower than the 75th percentile(22.24)."
    ## 
    ## $data.price_stat40
    ## [1] "This nail_polish's price is $10.99 which is lower than the overall nail_polish price mean (14.1), equal to the price median(10.99), higher than the 25th percentile(8.14), and lower than the 75th percentile(22.24)."
    ## 
    ## $data.price_stat41
    ## [1] "This nail_polish's price is $10.99 which is lower than the overall nail_polish price mean (14.1), equal to the price median(10.99), higher than the 25th percentile(8.14), and lower than the 75th percentile(22.24)."
    ## 
    ## $data.price_stat42
    ## [1] "This nail_polish's price is $10.99 which is lower than the overall nail_polish price mean (14.1), equal to the price median(10.99), higher than the 25th percentile(8.14), and lower than the 75th percentile(22.24)."
    ## 
    ## $data.price_stat43
    ## [1] "This nail_polish's price is $10.99 which is lower than the overall nail_polish price mean (14.1), equal to the price median(10.99), higher than the 25th percentile(8.14), and lower than the 75th percentile(22.24)."
    ## 
    ## $data.price_stat44
    ## [1] "This nail_polish's price is $7.79 which is lower than the overall nail_polish price mean (14.1), lower than the price median(10.99), lower than the 25th percentile(8.14), and lower than the 75th percentile(22.24)."
    ## 
    ## $data.price_stat45
    ## [1] "This nail_polish's price is $8.99 which is lower than the overall nail_polish price mean (14.1), lower than the price median(10.99), higher than the 25th percentile(8.14), and lower than the 75th percentile(22.24)."
    ## 
    ## $data.price_stat46
    ## [1] "This nail_polish's price is $7.79 which is lower than the overall nail_polish price mean (14.1), lower than the price median(10.99), lower than the 25th percentile(8.14), and lower than the 75th percentile(22.24)."
    ## 
    ## $data.rating1
    ## [1] NA
    ## 
    ## $data.rating2
    ## [1] 4.8
    ## 
    ## $data.rating3
    ## [1] NA
    ## 
    ## $data.rating4
    ## [1] 5
    ## 
    ## $data.rating5
    ## [1] 4.3
    ## 
    ## $data.rating6
    ## [1] 2.5
    ## 
    ## $data.rating7
    ## [1] 3.5
    ## 
    ## $data.rating8
    ## [1] NA
    ## 
    ## $data.rating9
    ## [1] 4.5
    ## 
    ## $data.rating10
    ## [1] NA
    ## 
    ## $data.rating11
    ## [1] 4.2
    ## 
    ## $data.rating12
    ## [1] 4.3
    ## 
    ## $data.rating13
    ## [1] 4
    ## 
    ## $data.rating14
    ## [1] 4
    ## 
    ## $data.rating15
    ## [1] 5
    ## 
    ## $data.rating16
    ## [1] 4.3
    ## 
    ## $data.rating17
    ## [1] 4.3
    ## 
    ## $data.rating18
    ## [1] 4.1
    ## 
    ## $data.rating19
    ## [1] NA
    ## 
    ## $data.rating20
    ## [1] 3
    ## 
    ## $data.rating21
    ## [1] NA
    ## 
    ## $data.rating22
    ## [1] 4.2
    ## 
    ## $data.rating23
    ## [1] 5
    ## 
    ## $data.rating24
    ## [1] 4
    ## 
    ## $data.rating25
    ## [1] 5
    ## 
    ## $data.rating26
    ## [1] 4.6
    ## 
    ## $data.rating27
    ## [1] 5
    ## 
    ## $data.rating28
    ## [1] NA
    ## 
    ## $data.rating29
    ## [1] 4.7
    ## 
    ## $data.rating30
    ## [1] 4.7
    ## 
    ## $data.rating31
    ## [1] NA
    ## 
    ## $data.rating32
    ## [1] 4.7
    ## 
    ## $data.rating33
    ## [1] 2.5
    ## 
    ## $data.rating34
    ## [1] 4.5
    ## 
    ## $data.rating35
    ## [1] 4.8
    ## 
    ## $data.rating36
    ## [1] 4.6
    ## 
    ## $data.rating37
    ## [1] 4.6
    ## 
    ## $data.rating38
    ## [1] NA
    ## 
    ## $data.rating39
    ## [1] NA
    ## 
    ## $data.rating40
    ## [1] NA
    ## 
    ## $data.rating41
    ## [1] NA
    ## 
    ## $data.rating42
    ## [1] 5
    ## 
    ## $data.rating43
    ## [1] NA
    ## 
    ## $data.rating44
    ## [1] NA
    ## 
    ## $data.rating45
    ## [1] NA
    ## 
    ## $data.rating46
    ## [1] NA
    ## 
    ## $data.rating_stat1
    ## [1] "No rating"
    ## 
    ## $data.rating_stat2
    ## [1] "This blush's rating is 4.8 which is higher than the overall blush rating mean (4.42), higher than the rating median(4.75), higher than the 25th percentile(4), and lower than the 75th percentile(5)."
    ## 
    ## $data.rating_stat3
    ## [1] "No rating"
    ## 
    ## $data.rating_stat4
    ## [1] "This eyeliner's rating is 5 which is higher than the overall eyeliner rating mean (4.23), higher than the rating median(4.3), higher than the 25th percentile(4), and higher than the 75th percentile(4.8)."
    ## 
    ## $data.rating_stat5
    ## [1] "This eyeliner's rating is 4.3 which is higher than the overall eyeliner rating mean (4.23), equal to the rating median(4.3), higher than the 25th percentile(4), and lower than the 75th percentile(4.8)."
    ## 
    ## $data.rating_stat6
    ## [1] "This eyeliner's rating is 2.5 which is lower than the overall eyeliner rating mean (4.23), lower than the rating median(4.3), lower than the 25th percentile(4), and lower than the 75th percentile(4.8)."
    ## 
    ## $data.rating_stat7
    ## [1] "This eyeliner's rating is 3.5 which is lower than the overall eyeliner rating mean (4.23), lower than the rating median(4.3), lower than the 25th percentile(4), and lower than the 75th percentile(4.8)."
    ## 
    ## $data.rating_stat8
    ## [1] "No rating"
    ## 
    ## $data.rating_stat9
    ## [1] "This eyeliner's rating is 4.5 which is higher than the overall eyeliner rating mean (4.23), higher than the rating median(4.3), higher than the 25th percentile(4), and lower than the 75th percentile(4.8)."
    ## 
    ## $data.rating_stat10
    ## [1] "No rating"
    ## 
    ## $data.rating_stat11
    ## [1] "This eyeliner's rating is 4.2 which is lower than the overall eyeliner rating mean (4.23), lower than the rating median(4.3), higher than the 25th percentile(4), and lower than the 75th percentile(4.8)."
    ## 
    ## $data.rating_stat12
    ## [1] "This eyeliner's rating is 4.3 which is higher than the overall eyeliner rating mean (4.23), equal to the rating median(4.3), higher than the 25th percentile(4), and lower than the 75th percentile(4.8)."
    ## 
    ## $data.rating_stat13
    ## [1] "This eyeshadow's rating is 4 which is lower than the overall eyeshadow rating mean (4.39), lower than the rating median(4.5), equal to the 25th percentile(4), and lower than the 75th percentile(5)."
    ## 
    ## $data.rating_stat14
    ## [1] "This foundation's rating is 4 which is lower than the overall foundation rating mean (4.22), lower than the rating median(4.3), equal to the 25th percentile(4), and lower than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat15
    ## [1] "This foundation's rating is 5 which is higher than the overall foundation rating mean (4.22), higher than the rating median(4.3), higher than the 25th percentile(4), and higher than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat16
    ## [1] "This foundation's rating is 4.3 which is higher than the overall foundation rating mean (4.22), equal to the rating median(4.3), higher than the 25th percentile(4), and lower than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat17
    ## [1] "This foundation's rating is 4.3 which is higher than the overall foundation rating mean (4.22), equal to the rating median(4.3), higher than the 25th percentile(4), and lower than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat18
    ## [1] "This foundation's rating is 4.1 which is lower than the overall foundation rating mean (4.22), lower than the rating median(4.3), higher than the 25th percentile(4), and lower than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat19
    ## [1] "No rating"
    ## 
    ## $data.rating_stat20
    ## [1] "This foundation's rating is 3 which is lower than the overall foundation rating mean (4.22), lower than the rating median(4.3), lower than the 25th percentile(4), and lower than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat21
    ## [1] "No rating"
    ## 
    ## $data.rating_stat22
    ## [1] "This lip_liner's rating is 4.2 which is lower than the overall lip_liner rating mean (4.36), lower than the rating median(4.5), higher than the 25th percentile(3.95), and lower than the 75th percentile(5)."
    ## 
    ## $data.rating_stat23
    ## [1] "This lipstick's rating is 5 which is higher than the overall lipstick rating mean (4.43), higher than the rating median(4.5), higher than the 25th percentile(4.07), and higher than the 75th percentile(4.93)."
    ## 
    ## $data.rating_stat24
    ## [1] "This lipstick's rating is 4 which is lower than the overall lipstick rating mean (4.43), lower than the rating median(4.5), lower than the 25th percentile(4.07), and lower than the 75th percentile(4.93)."
    ## 
    ## $data.rating_stat25
    ## [1] "This lipstick's rating is 5 which is higher than the overall lipstick rating mean (4.43), higher than the rating median(4.5), higher than the 25th percentile(4.07), and higher than the 75th percentile(4.93)."
    ## 
    ## $data.rating_stat26
    ## [1] "This lipstick's rating is 4.6 which is higher than the overall lipstick rating mean (4.43), higher than the rating median(4.5), higher than the 25th percentile(4.07), and lower than the 75th percentile(4.93)."
    ## 
    ## $data.rating_stat27
    ## [1] "This lipstick's rating is 5 which is higher than the overall lipstick rating mean (4.43), higher than the rating median(4.5), higher than the 25th percentile(4.07), and higher than the 75th percentile(4.93)."
    ## 
    ## $data.rating_stat28
    ## [1] "No rating"
    ## 
    ## $data.rating_stat29
    ## [1] "This lipstick's rating is 4.7 which is higher than the overall lipstick rating mean (4.43), higher than the rating median(4.5), higher than the 25th percentile(4.07), and lower than the 75th percentile(4.93)."
    ## 
    ## $data.rating_stat30
    ## [1] "This mascara's rating is 4.7 which is higher than the overall mascara rating mean (4.28), higher than the rating median(4.3), higher than the 25th percentile(4), and equal to the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat31
    ## [1] "No rating"
    ## 
    ## $data.rating_stat32
    ## [1] "This mascara's rating is 4.7 which is higher than the overall mascara rating mean (4.28), higher than the rating median(4.3), higher than the 25th percentile(4), and equal to the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat33
    ## [1] "This mascara's rating is 2.5 which is lower than the overall mascara rating mean (4.28), lower than the rating median(4.3), lower than the 25th percentile(4), and lower than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat34
    ## [1] "This mascara's rating is 4.5 which is higher than the overall mascara rating mean (4.28), higher than the rating median(4.3), higher than the 25th percentile(4), and lower than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat35
    ## [1] "This mascara's rating is 4.8 which is higher than the overall mascara rating mean (4.28), higher than the rating median(4.3), higher than the 25th percentile(4), and higher than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat36
    ## [1] "This mascara's rating is 4.6 which is higher than the overall mascara rating mean (4.28), higher than the rating median(4.3), higher than the 25th percentile(4), and lower than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat37
    ## [1] "This mascara's rating is 4.6 which is higher than the overall mascara rating mean (4.28), higher than the rating median(4.3), higher than the 25th percentile(4), and lower than the 75th percentile(4.7)."
    ## 
    ## $data.rating_stat38
    ## [1] "No rating"
    ## 
    ## $data.rating_stat39
    ## [1] "No rating"
    ## 
    ## $data.rating_stat40
    ## [1] "No rating"
    ## 
    ## $data.rating_stat41
    ## [1] "No rating"
    ## 
    ## $data.rating_stat42
    ## [1] "This nail_polish's rating is 5 which is higher than the overall nail_polish rating mean (4.12), higher than the rating median(4.3), higher than the 25th percentile(3.8), and equal to the 75th percentile(5)."
    ## 
    ## $data.rating_stat43
    ## [1] "No rating"
    ## 
    ## $data.rating_stat44
    ## [1] "No rating"
    ## 
    ## $data.rating_stat45
    ## [1] "No rating"
    ## 
    ## $data.rating_stat46
    ## [1] "No rating"
    ## 
    ## $data.description1
    ## [1] "Create an instantly just kissed glow with L'Oreal Paris Visible Lift Blur™ Blush. The cushiony soft texture glides over skin to blur away dullness for instantly radiant looking skin.Ingredients: CYCLOPENTASILOXANE / DIMETHICONECROSSPOLYMER / ALUMINA / DISODIUMSTEAROYL GLUTAMATE / ALUMINUMHYDROXIDE  [+/- MAY CONTAIN: MICA / CI 77891 - TITANIUMDIOXIDE / CI 45410 - RED 28 LAKE / CI73360 - RED 30 / CI 19140 - YELLOW 5LAKE / CI 77491, CI 77492, CI 77499 -IRON OXIDES] "
    ## 
    ## $data.description2
    ## [1] "True Match Blush flawlessly complements your skin's texture and tone and coordinates perfectly with True Match Makeup, Powder, and Concealer.Opti-Match technology creates uniquely true-to-life shades that flawlessly complement your skintone and undertone to perfection, leaving you with the most natural flush of colour imaginable.A unique Opti-BlendTM formula provides a skin-true texture that blends smoothly, never looks chalky or cakey.  Stays fresh and natural looking hour after hour.Oil-free, vitamin-rich, and non-comedogenic."
    ## 
    ## $data.description3
    ## [1] "The L'Oreal Paris Infallible Pro-Contour Palette makes contouring easy. Use the pro-contour brush to enhance and define features for a professional contoured look. Perfectly coordinated highlighting and contouring shades blend easily after applying for a flawless contoured look. Features:Contours in 30 secondsDefinesHighlightsBrush includedHow to Use: Step 1: Use the lighter shade to highlight and create extra dimensions. Step 2: Accentuate contours with the darker shade. Step 3: Blend and buff for a professionally defined look. "
    ## 
    ## $data.description4
    ## [1] "                    L'Oréal Paris introduces Silkissime™ by Infallible® Eyeliner. The formula glides on like silk for a soft and luxurious line with up to 16H of intensity.Features:Rich, silky formulaSmudge-resistant, up to 16H wearOphthalmologist testedSuitable for sensitive eyes and contact lens wearers                "
    ## 
    ## $data.description5
    ## [1] "Now get the intense look and luxurious feel of a liquid liner with the easy application and precision of a pencil.  Extra-Intense Liquid Pencil Eyeliner glides on smoothly and evenly for beautiful definition in a single stroke.  The smudge-resistant formula provides 16 hours of non-stop wear. "
    ## 
    ## $data.description6
    ## [1] "Master the smoky eye in one step - really! Every makeup artist knows: the perfect smoky eye is about the perfect pairing of eyeliner and shadow. Now, L’Oréal Paris introduces the first smoky eye powder liner, blending the best of eyeliner and shadow into a single product. Its Infallible powder formula lasts up to 14 hours while the suede-like applicator is soft enough to expertly blend, with a pointed tip for optimal control."
    ## 
    ## $data.description7
    ## [1] "Enjoy beautifully defined eyes for up to 16 hours! New Infallible Never Fail Eyeliner is L'Oreal's most advanced mechanical liner with superior long-wearing technology. The rich and creamy formula glides on easily and evenly and sets quickly for fade-proof colour. Use the bulit-in smudger and sharper to create versatile eyeliner looks for any occasion."
    ## 
    ## $data.description8
    ## [1] "Enjoy flawlessly clean lines as precise as 0.4 mm with The Super Slim by Infallible® Never FailTM eyeliner. The ultra-fine felt tip gives you total control and delivers the perfect amount of liner. The intense quick dry 12-hour formula glides on smoothly with continuous and even flow. Perfect for creating sleek and sophisticated eyes in one simple stroke!Features:Skip-proofDrag-proofSmudge-proofbrOphhalmologist-testedSuitable for sensitive eyes and contact lens wearers.                                                "
    ## 
    ## $data.description9
    ## [1] "L'Oreal Paris Colour Riche Le Khôl Eyeliner comes in a range of beautiful, wearable shades with a luxurious, intense formula combining oil and wax. This eyeliner softly glides over the eyelid allowing you to achieve a long-lasting and intense colour result."
    ## 
    ## $data.description10
    ## [1] "Black Velvet by Infallible is L'Oreal's first plush velvet eyeliner for the ultimate bold, slick line in one stroke. This intensely vibrant black liner has an easy application felt tip to deliver supreme softness with a satin-like finish. Features:14-hour wearPlush velvet tipSatiny black finishHow to Use: For thinner lines, hold liner so tip is horizontal. For thicker lines, hold liner so tip is vertical. Start by the inner corner and apply across lashline to outer corner of eye. Apply to bottom lid as close to waterline as possible. Connect top and bottom lines at the outer corner of the eyelid.INGREDIENTS:  AQUA / WATER / EAU - BUTYLENE GLYCOL - CI 77266 / BLACK 2 - ACRYLATES COPOLYMER – GLYCERIN - BEHENETH-30 - SODIUM LAURETH-11 CARBOXYLATE – PHENOXYETHANOL - LAURETH-10 - SODIUM LAURETH-12 SULFATE – ALCOHOL - POLYSORBATE 80 - SODIUM DEHYDROACETATE - POTASSIUM SORBATE - C11-15 PARETH-7 -CAPRYLYL GLYCOL "
    ## 
    ## $data.description11
    ## [1] "Now, give your eyes dramatic definition that lasts all day. Telescopic Waterproof precision Liquid Eyeliner is the perfect compagnion to Telescopic Mascara for a precisely defined and intense eye look. The Slanted Precision Felt tip is angled for the most accurate application, and the glide-on formula delivers 16-hour wear thats swim-proof, sweat-proof and smudge-proofAll-day wearGlides on without smudging or smearingSuitable for sensitive eyes and contact lens wearersFragrance-freeOphthalmologist-tested and allergy-tested "
    ## 
    ## $data.description12
    ## [1] "The one liner that does it all, complete with precision applicator tip. Combines the drama of a liquid with the ease of a pen."
    ## 
    ## $data.description13
    ## [1] "Inspired by makeup designers 'Must Have' palettes, the L'Oreal La Palette Nude offers everything you need to master the art of nude eyeshadow. Get Makeup Designer Nude looks curated exclusively for each palette with step by step instructions and live tutorials!Features: 10 highly pigmented shades range from light to dark to flatter every skin tone in 3 shadow finishes: shimmery satin, buttery matte & lustrous sheenEye-designer brush & mirror for effortless application\t\t"
    ## 
    ## $data.description14
    ## [1] "Liquid foundation revolutionized – a fresh luminous glow in a tap. Introducing the new L'Oreal Paris True Match Lumi Cushion, an innovative cushion drenched in liquid foundation. Tap a little for a fresh natural glow, tap on more to build a new level of luminous coverage.Features:Effortless applicationCompletely buildableOn-the-go coverageFormulated with Precise Match Technology to match your skin’s tone and textureAvailable in 8 true-to-skin shadesFinish: fresh & luminousCoverage: sheer to medium, buildable coverageFor all skin types"
    ## 
    ## $data.description15
    ## [1] "Finally, makeup that creates skin so healthy - it's luminous.True Match Lumi Healthy Luminous Makeup combines 3 powerful ingredients that work to improve your complexion the more you wear them. 40% pure water for all-day hydration;Antioxidants and Vitamins C and E for improved clarity and skin tone and;Liquid Light Technology for an instant, luminous glowIsn't it time your met your match?Additional Features: SPF 20Lightweight, buildable coverage that lasts up to 8 hoursFor all skin typesSafe for sensitive skinNon-Comedogenic. Dermatologist tested. Allergy tested. "
    ## 
    ## $data.description16
    ## [1] "Longwear foundation without the compromise. Never masky, never cakey, never dull – Infallible Pro-Matte™ is L’Oréal’s next generation of longwearing makeup.The micro-sponge technology absorbs excess oil and shine to create a dimensional matte finish without the need for a heavy or caked-on look. Use a sponge to buff it out for a sheer and natural look or apply in layers for more complete coverage. "
    ## 
    ## $data.description17
    ## [1] "Introducing a breakthrough in age minimizing makeup.Only Visible Lift Serum Absolute features an age reversing serum of 5 ingredients in 1 luminous makeup. This new makeup instantly evens skin tone with a luxuriously lightweight formula that doesn't settle into lines and wrinkles.In 4 weeks, the potent formula addresses 5 sighs of aging to visibly transform skin to smoother, firmer, brighter, even, flawless.5 proven results :45% more even skin tone72% saw more youthful skin75% reduction inf ine lines and wrinkles79% saw fewer imperfections90% saw smoother skinFinish : LuminousCoverage : MediumSkin Type : All skin types"
    ## 
    ## $data.description18
    ## [1] "True Match Super-Blendable Makeup precisely matches your skin’s tone and texture and coordinates perfectly with True Match Powder, Blush and Concealer. Formulated with Precise Match Technology so you can control coverage and fine-tune it.The ultra-pure formula contains no oils, fragrances, or pore-clogging fillers, so all you see is beautiful, radiant, flawless skin.Features:Finish: NaturalCoverage: Light to mediumSkin Type: For all skin types"
    ## 
    ## $data.description19
    ## [1] "Discover instantly ageless skin. L'Oreal Paris Visible Lift Blur™ features Opti-Blur™ technology with a cushiony soft texture that glides on to blur the look of lines, wrinkles and uneven skin tone in seconds. Skin looks flawlessly smooth, instantly perfected.Ingredients: CYCLOPENTASILOXANE,AQUA, ETHYLHEXYLMETHOXYCINNAMATE,DIMETHICONECROSSPOLYMER,GLYCERIN, ISOTRIDECYLISONONANOATE, PEG-9POLYDIMETHYLSILOXYETHYLDIMETHICONE,DISTEARDIMONIUMHECTORITE,PHENOXYETHANOL,PEG-10 DIMETHICONE,HYDROLYZED WHEATPROTEIN/PVPCROSSPOLYMER,DISODIUM STEAROYLGLUTAMATE,CHLORPHENESIN,METHYLPARABEN,LYCIUM BARBARUMFRUIT EXTRACT,ACRYLATESCOPOLYMER,POTASSIUM SORBATE,ETHYLPARABEN,TOCOPHEROL,ASCORBYL PALMITATE,PANTHENOL, PEG-9,ALUMINUM HYDROXIDE,CITRIC ACID, SODIUMPCA, UREA, TREHALOSE,POLYQUATERNIUM-51,SOLUBLE COLLAGEN,SODIUM HYALURONATE.[+/- : CI 77891, MICA,CI 77492, CI 77491,CI 77499]"
    ## 
    ## $data.description20
    ## [1] "The ultimate in luminosity. L’Oréal Paris introduces its first liquid highlighter specifically crafted to highlight key features or illuminate all-over. Designed in Golden, Rose, and Ice shades that flatter every skintone from warm to neutral to cool. Features:Golden Illuminator enhances peachy or yellow tones in warm skintonesRose Illuminator enhances yellow, peachy, pink or blue tones in neutral skintonesIce Illuminator enhances pink or blue tones in cool skintonesHighlight key features or illuminate all-overLightweight formulaFor every skintone & undertoneINGREDIENTS: AQUA/WATER/EAU, DIMETHICONE, GLYCERIN, ALCOHOL DENAT, SYNTHETIC FLUORPHLOGOPITE, DIMETHICONE/POLYGLYCERIN-3, CROSSPOLYMER, ACRYLAMIDE/SODIUM, ACRYLOYLDIMETHYLTAURATE COPOLYMER, PHENOXYETHANOL, ISOHEXADECANE, CAPRYLYL GLYCOL, POLYSORBATE 80, DIPROPYLENE, GLYCOL TIN OXIDE "
    ## 
    ## $data.description21
    ## [1] "Colour Riche Creamy Anti-Feathering Lip Liners create definition and coordinate perfectly with Colour Riche Lipcolour.  Colour Riche Lip Liner is formulated with ultra-hydrating Omega 3 and protective Vitamin E to keep lips moisturized, soft and supple.  "
    ## 
    ## $data.description22
    ## [1] "L'Oreal Paris Infallible Never Fail Lip Liner is the perfect long-lasting lip liner.No touch-ups. No feathering. No fading! Just a rich and creamy formula that goes on smooth and even, leaving your lips soft and comfortable for hours. "
    ## 
    ## $data.description23
    ## [1] "Inspired by L’Oréal Paris muses, Colour Riche introduces six custom-made, enriching matte red shades tailored to enhance a variety of complexions. Play on a coordinated look by pairing with red nails for an absolute statement.Features:A matte that reveals red's colour intensitySoft intense velvet formulationFormulated with velvet spheres, pure pigments, jojoba oil"
    ## 
    ## $data.description24
    ## [1] "Transform lips from ordinary to extraordinary. Color is richer, lip surface is smoother and shine is magnified. Formulated with precious micro-oils and rich color pigments, Extraordinaire provides the ideal balance of color and care for perfect lips. The unique soft-touch applicator allows for a silky-smooth, gliding application.Directions: Use the wand to apply starting in the center of your upper lip. Work from the center to outer edges of your lips, following the contour of your mouth. Then glide across the entire bottom lip.\t\t"
    ## 
    ## $data.description25
    ## [1] "Crystal Shine is a spectacular L'Oreal Paris innovation in lip colour that reflects light to perfection!Crystal Shine is brilliance redefined: an explosion of colour that combines the glint of metal with the gleam of crystal. Tiny flakes of crystal are covered by a thick layer of metal and trapped within the rich, moist colour and texture of the lipstick.Crystal Shine technology is contained within Colour Riche's new and improved Tri-Active formula of Vitamins A and E, for long lasting comfort.Colour Riche Crystal Shine is available in 10 stunning shades with crystal reflections-get ready to dazzle!"
    ## 
    ## $data.description26
    ## [1] "Indulge in richness beyond compare with L'Oreal's most luxuriously rich color and intensely rich hydration. Colour Riche® Lipcolour is richer, creamier and more moisturizing than ever before. In addition to nourishing ingredients like Omega 3 and Vitamin E, the formula is enriched with Argan Oil to condition and soften lips. Your lips are kept soft, smooth, and ultra-hydrated. With a spectrum of gorgeous shades from bold to nearly nude, Colour Riche® Lipcolour offers a shade that’s perfect for every look and any occasion.\t\t"
    ## 
    ## $data.description27
    ## [1] "L'Oreal Infallible Lipstick is a longwear 2-step lip colour with hyaluronic gel for 24HR triple action.The hyaluron-infused longwear formula provides saturated, vibrant colour for lips that appear smooth and replenished.Features:All day smoothness for colour that never driesAll day hydration for endless comfortAll day resistance for colour that never fades "
    ## 
    ## $data.description28
    ## [1] "Introducing Colour Riche Le Matte & La Lacque, the newest format in lip colour. velvety matte or irresistibly glossy pigment-rich colour, perfectly precise. Use it anywhere, anytime – it’s the newest makeup designer must-have!Features:The sleek, sophisticated pen offers highly pigmented colour Glides on effortlessly in one precise stroke Available in 2 iconic finishes, creamy matte & smooth lacquerEnriched with argan oil for deep hydration and soft feel & vitamin antioxidant for protection"
    ## 
    ## $data.description29
    ## [1] "L'Oreal Paris Collection Exclusive by Colour Riche® introduces a permanent offer to the L'Oreal Paris portfolio. This collection includes six signature nude shades by L'Oreal's six dazzling spokeswomen. The shades are custom-made to match various skin tones and haircolour."
    ## 
    ## $data.description30
    ## [1] "Stretch and extend your lashes like wings!  Introducing Voluminous Butterfly Mascara, one of L'Oreal's most-exciting mascara innovations, yet. Its revolutionary Butterfly Brush™ with asymmetrical lash line shape extends and lifts outer corner lashes to give you a winged-out effect, instantly volumizing lashes at the roots and stretching lashes outward. The unique Cocoon Fibers™ instantly wrap lashes in a soft, delicate veil to create silky, spectacular lashes that flutter."
    ## 
    ## $data.description31
    ## [1] "Stretch and extend your lashes like wings!  Introducing Voluminous Butterfly Mascara, one of L'Oreal's most-exciting mascara innovations, yet. Its revolutionary Butterfly Brush™ with asymmetrical lash line shape extends and lifts outer corner lashes to give you a winged-out effect, instantly volumizing lashes at the roots and stretching lashes outward. The unique Cocoon Fibers™ instantly wrap lashes in a soft, delicate veil to create silky, spectacular lashes that flutter."
    ## 
    ## $data.description32
    ## [1] "Want longer, more defined lashes in a single sweep?L'Oreal Paris introduces amazing new technology that lengthens lashes to the extreme and intensifies them at their base. It will change the way women think about lengthening mascara.It's the 1st spectacular stretch-effect mascara from L'Oreal Paris.The High-Precision Flexible Brush stretches the formula towards infinity for up to 60% longer lashes."
    ## 
    ## $data.description33
    ## [1] "4X Fuller LashesDramatic Volume EffectSeparates & Builds Every LashPatented Volume Maximizing BrushL'Oreal presents Voluminous Volume Building Mascara. Uniquely formulated to resist clumping, soften lashes and build lashes to 4X their natural thickness.Unique maximizing formula quickly thickens and builds lashes in a single application for a full and dramatic lash look.Patented Volume Maximizing Brush thickens lashes evenly, smoothly leaving them soft with virtually no flakes, smudges or clumps.Voluminous with Panthenol and Ceramide-R protects and conditions lashes leaving them supple and soft to the touch, resisting clumping and flaking.Opththalmologist and allergy tested.Suitable for sensitive eyes and contact lens wearers.Clump-resistant.Fragrance-free.All day wear."
    ## 
    ## $data.description34
    ## [1] "L’Oréal Paris introduces Voluminous Million Lashes Excess, which combines bold volume with clean definition for an excessively dramatic lash look.The legendary Millionizer brush is bigger than ever to separate and build each lash evenly from root to tip. The mascara features a built-in, anti-clump wiper that removes surplus mascara for a clean and impactful effect. Plus, the ultra black formula amplifies each individual lash for a dramatic lash effect."
    ## 
    ## $data.description35
    ## [1] "L'Oreal's biggest volume builder! Introducing the latest innovation in mascara technology--Extra Volume Collagen Mascara. It's made with hydra-collagen and enhances the thickness of your lashes for up to 12x more impact without any clumps. Our patented brush is 50% bigger so it builds that incredible volume fast. Your lashes will build extra body and extra impact in just one sweep."
    ## 
    ## $data.description36
    ## [1] "For lashes that look a million times multiplied and perfectly defined, there's only one mascara - Voluminous® Million Lashes™. The built-in Clean Sweep Wiping System wipes the brush free of clumps leaving the perfect amount of mascara for an ultra-volumized clean lash look. The Millionizing Brush evenly thickens lashes from root to tip. The result? Lashes look so multiplied, so clean, it's like seeing millions. Features: Ophthalmologist-tested and allergy-testedSuitable for sensitive eyes an contact len wearersClump-free.Fragrance-free                                                 "
    ## 
    ## $data.description37
    ## [1] "Get the look of salon lash extensions with the new Double Extend Beauty Tubes Mascara! This new lash extension effect mascara visibly lengthens your lashes up to an astonishing 80%.Step 1: Ultra nourishing base coat infused with Ceramide R and D-Panthenol protects and strengthens lashes creating the perfect base for tube application.Step 2: Ultra-lengthening tube top coat sets to form lash-extending beauty tubes that visibly lengthen lashes to amazing new heights. Beauty tubes will not run, clump, flake or smudge and will stay on all-day.Step 3: Beauty tubes remove easily with just warm water – no makeup remover or rubbing required!Clump-free length.Smudge-proof.Flake-proof.Long wear hold.Ophthalmologist and allergy tested.Suitable for sensitive eyes and contact lens wearers.Removes easily with plenty of warm water.Fragrance-free. "
    ## 
    ## $data.description38
    ## [1] "L'Oreal Infallible Nail Polish gives you fantastic gel-like nail colour with a tinted top coat to match!This Infallible nail polish has an exclusive hybrid combo of silicone and Flex-Resin™ that interact with one another to create a unique matrix. The formulas are a combination of film forming polymer, gellifying agents and crystal copolymer designed to bring durability, adhesion to nail and brilliant shine. Provides a smooth, plump, spectacular gel shine manicure that lasts.Features: Up to 12-day wearSpectacular gel shineMatching tinted top coatNo nail damageEasy to remove"
    ## 
    ## $data.description39
    ## [1] "L'Oreal Infallible Nail Polish gives you fantastic gel-like nail colour with a tinted top coat to match!This Infallible nail polish has an exclusive hybrid combo of silicone and Flex-Resin™ that interact with one another to create a unique matrix. The formulas are a combination of film forming polymer, gellifying agents and crystal copolymer designed to bring durability, adhesion to nail and brilliant shine. Provides a smooth, plump, spectacular gel shine manicure that lasts.Features: Up to 12-day wearSpectacular gel shineMatching tinted top coatNo nail damageEasy to remove"
    ## 
    ## $data.description40
    ## [1] "L'Oreal Infallible Nail Polish gives you fantastic gel-like nail colour with a tinted top coat to match!This Infallible nail polish has an exclusive hybrid combo of silicone and Flex-Resin™ that interact with one another to create a unique matrix. The formulas are a combination of film forming polymer, gellifying agents and crystal copolymer designed to bring durability, adhesion to nail and brilliant shine. Provides a smooth, plump, spectacular gel shine manicure that lasts.Features: Up to 12-day wearSpectacular gel shineMatching tinted top coatNo nail damageEasy to remove"
    ## 
    ## $data.description41
    ## [1] "L'Oreal Infallible Nail Polish gives you fantastic gel-like nail colour with a tinted top coat to match!This Infallible nail polish has an exclusive hybrid combo of silicone and Flex-Resin™ that interact with one another to create a unique matrix. The formulas are a combination of film forming polymer, gellifying agents and crystal copolymer designed to bring durability, adhesion to nail and brilliant shine. Provides a smooth, plump, spectacular gel shine manicure that lasts.Features: Up to 12-day wearSpectacular gel shineMatching tinted top coatNo nail damageEasy to remove"
    ## 
    ## $data.description42
    ## [1] "L'Oreal Infallible Nail Polish gives you fantastic gel-like nail colour with a tinted top coat to match!This Infallible nail polish has an exclusive hybrid combo of silicone and Flex-Resin™ that interact with one another to create a unique matrix. The formulas are a combination of film forming polymer, gellifying agents and crystal copolymer designed to bring durability, adhesion to nail and brilliant shine. Provides a smooth, plump, spectacular gel shine manicure that lasts.Features: Up to 12-day wearSpectacular gel shineMatching tinted top coatNo nail damageEasy to remove"
    ## 
    ## $data.description43
    ## [1] "L'Oreal Infallible Nail Polish gives you fantastic gel-like nail colour with a tinted top coat to match!This Infallible nail polish has an exclusive hybrid combo of silicone and Flex-Resin™ that interact with one another to create a unique matrix. The formulas are a combination of film forming polymer, gellifying agents and crystal copolymer designed to bring durability, adhesion to nail and brilliant shine. Provides a smooth, plump, spectacular gel shine manicure that lasts.Features: Up to 12-day wearSpectacular gel shineMatching tinted top coatNo nail damageEasy to remove"
    ## 
    ## $data.description44
    ## [1] "L'Oreal Colour Riche® presents a new luxurious 3D texture for your nails. In a few strokes, achieve a textured nail finish with specks of sparkle for the ultimate chic look."
    ## 
    ## $data.description45
    ## [1] "L’Oréal Paris introduces the Extraordinaire Gel-Lacque 1-2-3 system for perfectly shaped, volumized nails and rich, ultra-glossy colour.This is a three-step nail system: GEL-PRIMER - Apply 1 coat of gel primer to build volume and prepare nails for optimized colour pay-off.GEL-COLOUR - Apply 2 coats of gel colour. Achieve rich, glossy colour and plumped-up nails.GEL-GLAZE - Apply 1 coat of gel glaze. Achieve incredible shine."
    ## 
    ## $data.description46
    ## [1] "Your dream colour is now at your fingertips with Colour Riche® Nail Colour. L'Oreal's formula delivers intense, luxurious colour and up to 10 days of shine. This collection includes signature nude shades by L'Oreal's dazzling spokeswomen. The shades are custom-made to match various skin tones and haircolour."
    ## 
    ## $data.image_link1
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/4099ce5656b195d2ec1b845d2b678e25_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link2
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/e4e4827631b874f898d41a90ab3de5a6_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link3
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/7a31b075cf9c0ae4e6eba9ca61c587a7_ra,w158,h184_pa,w158,h184.png"
    ## 
    ## $data.image_link4
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/a132f78e0b9e4aa4cca37c9f05267897_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link5
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/cf225e409760daf4dea62290990008a6_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link6
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/08f663a61b796cb57fd6aef86dbb5cc1_ra,w158,h184_pa,w158,h184.jpg"
    ## 
    ## $data.image_link7
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/2f7610f4a82cc1c7abb8ddc33ab236f0_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link8
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/c8f4a975e002711a67113fcf53b7dad0_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link9
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/94a08dc5666ab926584801c34140b29e_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link10
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/b6ca6fa1ab5021b55d9ca0ca3ed40437_ra,w158,h184_pa,w158,h184.png"
    ## 
    ## $data.image_link11
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/d8a23eb4d183b3b215d46ef39a6e781b_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link12
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/86cde1cfdb47caee6387cdfc7ac0696e_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link13
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/d6bf791aee472d5cc9d402a4227b10b8_ra,w158,h184_pa,w158,h184.jpg"
    ## 
    ## $data.image_link14
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/0350aac08c86bccd97c2a5ca26b15ae9_ra,w158,h184_pa,w158,h184.jpg"
    ## 
    ## $data.image_link15
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/7d8276b94863d7cff212d0161b5c632e_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link16
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/268da4d7967fad9c153af24ee8d2e026_ra,w158,h184_pa,w158,h184.jpg"
    ## 
    ## $data.image_link17
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/39be9facd53cf124ff38e9a25de09e10_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link18
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/c71a2c6a4f7d41ceb60f068780bcfba5_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link19
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/c207687e567547445338321ae28c9b96_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link20
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/2a38b2c75a247fe6d1038431964dd24c_ra,w158,h184_pa,w158,h184.png"
    ## 
    ## $data.image_link21
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/0d3cb34004a3290519b914ceaa0d67c8_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link22
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/0b1191651b7aef22b11e96f8c7c548d4_ra,w158,h184_pa,w158,h184.png"
    ## 
    ## $data.image_link23
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/5d0df885cb5721b74b32f64884736ce4_ra,w158,h184_pa,w158,h184.jpg"
    ## 
    ## $data.image_link24
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/739bc21253ce772c2b2e7ad40d9d3f44_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link25
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/6709b779907bbb8410a81c3156ac5a1c_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link26
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/5409595e1888807b979036e77ef7e496_ra,w158,h184_pa,w158,h184.jpeg"
    ## 
    ## $data.image_link27
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/8768b6e9ad0890545f5f261980727caa_ra,w158,h184_pa,w158,h184.jpg"
    ## 
    ## $data.image_link28
    ## [1] "https://d3t32hsnjxo7q6.cloudfront.net/i/a368eac7a50f7aa3fcc773b06915eb39_ra,w158,h184_pa,w158,h184.png"
    ## 
    ##  [ reached getOption("max.print") -- omitted 1536 entries ]

I did not observe clear relationship between price and rating from
“covergirl”, “physicians formula”, “maybelline” or “revlon” data. There
might be a week negative relationship between price and rating in
“l’oreal”, which means its cheaper items had better ratings.

## Second call

Next, I am interested in the brand “maybelline” which has many items in
this database with not great ratings. I am calling the API function
again and getting a subset data frame with only “maybelline” cosmetic
products, then converting its product type to factor by calling the
`add_factor` function.

``` r
# call the function and return a subset data frame with only maybelline cosmetic products
mbl <- pick_makeup(makeup_brand = "maybelline", makeup_type = NULL)

mbl_factor <- add_factor(mbl)
```

### Second stacked bar graph

I will use `gglot` and `geom_bar`, assign cosmetic_type as x, also fill
colors based on different product type, to create a stacked bar graph
which presents item count for each product type.

``` r
q <- ggplot(data = mbl_factor, aes(x = Cosmetic_Type, fill = Cosmetic_Type))
  q + geom_bar(alpha = 0.7) +
  labs(x = "Cosmetic Type", y = "Item count", title = "Bar Plot of Item Count for Each Cosmetic Type of maybelline") 
```

![](README_files/figure-gfm/graphics4-1.png)<!-- -->

“maybelline” has items in all 9 product types, since this brand started
up with mascara and foundation, the biggest product type is mascara and
the smallest category is lip liner.

### Rating summary by product type.

After removing records with missing rating, I am going to calculate the
rating mean, standard deviation, variance, median, Q1 and Q3 across
product types by using a `group_by` and a `summarise` function.

``` r
 mbl_factor %>%
            filter(is.na(rating) == FALSE) %>%
                   group_by(Cosmetic_Type) %>%
                   summarise(Mean = mean(rating),  Standard_Deviation = sd(rating), 
                             Variance = var(rating), Median = median(rating), 
                             q1 = quantile(rating, probs = 0.25),
                             q3 = quantile(rating, probs = 0.75))
```

    ## # A tibble: 9 × 7
    ##   Cosmetic_Type  Mean Standard_Deviation Variance Median    q1    q3
    ##   <fct>         <dbl>              <dbl>    <dbl>  <dbl> <dbl> <dbl>
    ## 1 blush          4.77              0.252   0.0633   4.8   4.65  4.9 
    ## 2 bronzer        4.75              0.354   0.125    4.75  4.62  4.88
    ## 3 eyeliner       4.23              0.207   0.0427   4.25  4.05  4.38
    ## 4 eyeshadow      3.76              0.929   0.863    4     3.5   4.4 
    ## 5 foundation     3.88              0.722   0.522    3.9   3.8   4.4 
    ## 6 lip_liner      3.5              NA      NA        3.5   3.5   3.5 
    ## 7 lipstick       4.4               0.849   0.72     4.8   4.2   5   
    ## 8 mascara        4.16              0.241   0.0582   4.1   4     4.35
    ## 9 nail_polish    3.43              0.513   0.263    3.3   3.15  3.65

Surprisingly, the product type has the highest rating is blush instead
of mascara. nail polish items have the lowest rating overall,
eyeshadow’s rating has the widest range while eyeliner’s rating is more
consistent.

### A box plot

Lastly, let’s create a box plot and learn the price distribution across
product type. I will use a `ggplot` function and assign price as y,
product type as x, also color each box according to their product type.
Then I am going to use a `geom_boxplot` function and add boxes onto the
plot.

``` r
  b <- ggplot(data = mbl_factor, aes(y = usd_price, x = Cosmetic_Type, fill=Cosmetic_Type))
    b + geom_boxplot(adjust = 0.5, color="red", alpha=0.5) +
    labs(y = "Price", x="Product Type", title = "Box Plot of Price Center and Spread Across Product Type Groups") 
```

![](README_files/figure-gfm/graphics5-1.png)<!-- -->

With lip liner and nail polish categories have either one item or one
price, I won’t consider their distribution. The price distribution of
blush and eyeliner categories are almost normal; bronzer, foundation and
mascara are left skewed while eyeshadow and lipstick are right skewed.
Brozer has the highest price overall and mascara has the lowest.
Eyeshadow has the biggest price range and listick price is the narrowest
with one outlier.

# Wrap-up

In this vignette, I built several functions to interact with makeup
API’s endpoints. After data was returned, it went through some data
cleaning and manipulation steps to form an analysis-ready data frame.
The second half of this vignette focused on exploratory data analysis
(EDA). I used `table`, `summarize` and `ggplot` functions to generate
contingency table, numerical summaries of makeup price and rating, and
different plots for data visualization.

I found this vignette is actually handy for my future makeup shopping if
they continually update the API with newly released items. I can search
makeup products by brand and type, also compare their price and rating
with the price and rating summaries from the same type products.

I did learn an interesting fact that some cheap items were rated much
better than expensive ones, with this concept, I will save some money
from cutting down high end products.

I hope this vignette will be known by more people especially girls, it
could be helpful!
