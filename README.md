Project 2
================
Vivi Feathers
2023-10-03

- [Overwiew](#overwiew)
- [API Interaction Functions](#api-interaction-functions)

# Overwiew

This vignette is created for exploring makeup items, their brand, name,
price, rating… \# Requirement and Packages.

``` r
library(tidyverse)
library(jsonlite)
```

# API Interaction Functions

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

# create a data cleaning function that removes confusing columns, keep records with non-missing price, removes carriage returns 
# from the description column, convert price to numeric values, standardized them as US dollars according to currency name.
clean_data <- function(df){
             df%>%
             select(brand, name, product_type, price, currency, rating, description, image_link) %>%
             filter (is.na(target$price) == FALSE & target$price != "0.0") %>%
             mutate(description = gsub("[\r\n]", "", description),
                    usd_price = if_else(is.na(currency) == TRUE, round(as.numeric(price)*1, digits =2),
                                   if_else(currency == "GBP", round(as.numeric(price)*1.22, digits =2),
                                        if_else(currency == "USD", round(as.numeric(price)*1, digits =2),
                                             if_else(currency == "CAD", round(as.numeric(price)*0.73, digits =2), 0)))))
}
# clean the raw "target" data frame by calling the "clean_data" function
target_clean <- clean_data(target)
# if makeup_type is missing, after getting cleaned by "clean_data" function and dropping the original price and 
# currency columns, "target_final" is the final data frame that will be returned

if (is.null(makeup_type)){
  target_final <- target_clean %>%
                    select(brand, name, product_type, usd_price, rating, description, image_link)
  return(target_final)
}
# if makeup_type is not missing,create an api url that grabs all the data from the same makeup type, summarize the price
# statistics (mean, median, 25th and 75th percentile) and the rating statistics (mean, median, 25th and 75th percentile).
# compare the price and rating of each observation from the "target_clean" data frame vs the price summary and rating summary
# from the same type product.
else {
  all_type_url <- paste0(api_url,"brand=&product_type=", makeup_type)
  # use `fromJSON` function to get data frame from the "all_type_url" we just set up and name it as "all_type"
  all_type <- fromJSON(all_type_url)
  # clean the raw "all_type" data frame by calling the "clean_data" function
  all_type_clean <- clean_data(all_type)
  #calculate the price statistics (mean, median, 25th and 75th percentile)
  type_price_avg <- mean(all_type_clean$usd_price)
  type_price_q1 <- quantile(all_type_clean$usd_price, probs = 0.25)
  type_price_q3 <- quantile(all_type_clean$usd_price, probs = 0.75)
  type_price_median <- median(all_type_clean$usd_price)

 # remove the missing values from rating, convert it to numeric 
 #and calculate the rating statistics (mean, median, 25th and 75th percentile)
 rate <- na.omit(all_type_clean$rating)%>%
          as.numeric()
 type_rate_avg <- mean(rate)
 type_rate_q1 <- quantile(rate, probs = 0.25)
 type_rate_q3 <- quantile(rate, probs = 0.75)
 type_rate_median <- median(rate)


 target2 <- target_clean %>%
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
  target_final <- target3 %>%
                  select(brand, name, product_type, usd_price, price_stat, rating, rating_stat, description, image_link)
  return(target_final)
  }
 }

# test call

test1 <- pick_makeup(makeup_brand = "dior", makeup_type = NULL)
test1
```

    ## # A tibble: 71 × 7
    ##    brand name                                                           product_type usd_price rating description image_link
    ##    <chr> <chr>                                                          <chr>            <dbl> <lgl>  <chr>       <chr>     
    ##  1 dior  "\n                            Junon\n                       … nail_polish       24.4 NA     Discover t… https://w…
    ##  2 dior  "\n                            Matte\n                       … nail_polish       24.4 NA     Discover t… https://w…
    ##  3 dior  "\n                            Poison Metal\n                … nail_polish       24.4 NA     Discover t… https://w…
    ##  4 dior  "\n                            Jungle Matte\n                … nail_polish       24.4 NA     Discover t… https://w…
    ##  5 dior  "\n                            Miss Satin\n                  … nail_polish       24.4 NA     Discover t… https://w…
    ##  6 dior  "\n                            Mineral\n                     … nail_polish       24.4 NA     Discover t… https://w…
    ##  7 dior  "\n                            Sauvage\n                     … nail_polish       24.4 NA     Discover t… https://w…
    ##  8 dior  "\n                            Diabolo\n                     … nail_polish       24.4 NA     Discover t… https://w…
    ##  9 dior  "\n                            Tease\n                       … nail_polish       24.4 NA     Discover t… https://w…
    ## 10 dior  "\n                            Precious Rocks\n              … nail_polish       25.0 NA     Diorific r… https://w…
    ## # ℹ 61 more rows

``` r
test2 <- pick_makeup(makeup_brand = "nyx", makeup_type = "lipstick")
test2
```

    ## # A tibble: 38 × 9
    ##    brand name                             product_type usd_price price_stat        rating rating_stat description image_link
    ##    <chr> <chr>                            <chr>            <dbl> <chr>              <dbl> <chr>       <chr>       <chr>     
    ##  1 nyx   Luv Out Loud Liquid Lipstick     lipstick           7   This lipstick's …     NA No rating   What quali… https://w…
    ##  2 nyx   V'Amped Up! Lip Top Coat         lipstick           6   This lipstick's …     NA No rating   Cast a spe… https://w…
    ##  3 nyx   Simply Pink Lip Cream            lipstick           6.5 This lipstick's …     NA No rating   Get ready … https://w…
    ##  4 nyx   Simply Red Lip Cream             lipstick           6.5 This lipstick's …     NA No rating   A classic … https://w…
    ##  5 nyx   Simply Nude Lip Cream            lipstick           6.5 This lipstick's …     NA No rating   Bare is be… https://w…
    ##  6 nyx   Macaron Lippies                  lipstick           6.5 This lipstick's …     NA No rating   Fantasize … https://w…
    ##  7 nyx   In Your Element Lipstick - Metal lipstick           9   This lipstick's …     NA No rating   A match ma… https://w…
    ##  8 nyx   Wicked Lippies                   lipstick           6.5 This lipstick's …     NA No rating   Take a wal… https://w…
    ##  9 nyx   Simply Vamp Lip Cream            lipstick           6.5 This lipstick's …     NA No rating   Vamp up yo… https://w…
    ## 10 nyx   In Your Element Lipstick - Water lipstick           9   This lipstick's …     NA No rating   A match ma… https://w…
    ## # ℹ 28 more rows
