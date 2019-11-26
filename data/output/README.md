# Intermidiary files
These are files you can load into your workspace so that you do not need to run all of the stuff leading up to having a clean/featured dataset and test/training set.

There are a series of different versions that can be found in the following folders: 

- **limited_factors** - Factors are lumped according to the following parameters
  - taster_name: 10 + other
  - taster_twitter: 10 + other
  - designation: 15 + other
  - country: 15 + other
  - variety: 15 + other
  - variety.red: 10 + other
  - variety.white: 10 + other
  - province: 15 + other
  - winery: 15 + other
- **more_than_50_obs** - This is calculated based on the frequency that they show up. They are as follows. 
  - taster_name: 15 + other
  - taster_twitter: 14 + other
  - designation: 16 most + other
  - country: 19 + other
  - variety: 76 + other
  - variety.red: + other
  - variety.white: + other
  - province: 73 + other
  - winery: 16 + other
- **more_than_100_obs** -
  - taster_name: 15 + other
  - taster_twitter: 14 + other
  - designation: 16 + other
  - country: 19 + other
  - variety: 76 + other
  - variety.red: ? + other
  - variety.white: ? + other
  - province: 73 + other
  - winery: 16 + other
- **more_than_250_obs** -
  - taster_name: 15 + other
  - taster_twitter: 14 + other
  - designation: 16 + other
  - country: 15 + other
  - variety: 44 + other
  - variety.red: ? + other
  - variety.white: + other
  - province: 52 + other
  - winery: 1 (all other)

## Inside of each of those folders you will find 5 files

- **clean_wine.RData** - This is the fully processed data set including country.map which is for the maps, this data set should be used for any plots.
- **wine_train.RData** - Training set where variables not altered re barts suggestion (ie values are normal not compared to the median)
- **wine_test.RData** - Training set with variables not altered re barts suggestion (ie values are normal not compared to the median)
- **wine_train.bart.RData** - Training set where variables have been altered re barts suggestion (ie values are compared to the median)
- **wine_test.bart.RData** - Training set where variables have been altered re barts suggestion (ie values are compared to the median)

## The avalible variables in the test/training set are
- **price** -
- **points** - 
- **points.category** - 
- **country** -
- **province** - 
- **winery** - 
- **color** -
- **variety** -
- **variety_and_color** - 
- **designation** - 
- **title.n_words** - 
- **title.n_words_per** -
- **title.sentement** -
- **title.n_chars** -
- **title.has_accents** -
- **taster.name** -
- **taster.twitter_handle** - 
- **taster.gender** - 
- **taster.avg_points** -
- **taster.n_reviews** -
- **taster.n_tweets** -
- **taster.n_followers** -

You need to select out either `variety` + `color` or `variety_color` and also either `taster.name` or `taster.twitter_handle`

### You can alter the load calls like this 
`load(here::here("data","output","limited_factors","wine_train.RData"))`
`load(here::here("data","output","limited_factors","wine_test.RData"))`
or 
`load(here::here("data","output","limited_factors","wine_train.bart.RData"))`
`load(here::here("data","output","limited_factors","wine_test.bart.RData"))`

replacing `limited_factors` with the one you want from above listed folders

### Clean data set can be loaded with 

`load(here::here("data","output","limited_factors","clean_wine.RData"))` 

alternativly you can click the broom in environment and then just navigate to correct folder under files then double click the one you want to load. 

 