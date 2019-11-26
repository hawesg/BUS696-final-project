# Intermidiary files
These are files you can load into your workspace so that you do not need to run all of the stuff leading up to having a clean/featured dataset and test/training set.

There are a series of different versions that can be found in the following folders: 

- **limited_factors** - Factors are lumped according to the following parameters
  - taster_name: 10 most common levels (rest are other)
  - taster_twitter: 10 most common levels (rest are other)
  - designation: 15 most common levels (rest are other)
  - country: 15 most common levels (rest are other)
  - variety: 15 most common levels (rest are other)
  - variety.red: 10 most common levels (rest are other)
  - variety.white: 10 most common levels (rest are other)
  - province: 15 most common levels (rest are other)
  - winery: 15 most common levels (rest are other)
- **more_than_50_obs** -
- **more_than_100_obs** -
- **more_than_250_obs** -

## Inside of each of those folders you will find 5 files

- **clean_wine.RData** - This is the fully processed data set including country.map which is for the maps, this data set should be used for any plots.
- **wine_train.RData** - Training set where variables not altered re barts suggestion (ie values are normal not compared to the median)
- **wine_test.RData** - Training set with variables not altered re barts suggestion (ie values are normal not compared to the median)
- **wine_train.bart.RData** - Training set where variables have been altered re barts suggestion (ie values are compared to the median)
- **wine_test.bart.RData** - Training set where variables have been altered re barts suggestion (ie values are compared to the median)