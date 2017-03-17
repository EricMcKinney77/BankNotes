### Motivation

With about 13 billion Euro banknotes in circulation among roughly 340 million people in 19 European countries, counterfeit Euros are unfortunately a common problem for businesses and individuals. About 19.9 million paper Euros (worth about 850,000) were removed from circulation in 2009 [1]. Detection of fake banknotes can be surprisingly difficult, regardless of the measures taken to prevent accurate counterfeiting. The following data was collected by Dr. Volker Lohweg and Dr. Helene Dörksen at Ostwestfalen-Lippe University of Applied Sciences in Lemgo, Germany. Their purpose in gathering the data was the developement of an app that consumers or businesses could use to identify counterfeit Euros.

### The Data

The data was extracted from 1372 images that were taken from genuine and counterfeit Euro banknote-like specimens. For digitization, an industrial camera (usually used for print inspection) was used. A Wavelet Transform tool was used to extract features (the attributes) from the images. The data can be accessed at UC Irvine's public data repository website: http://archive.ics.uci.edu/ml/

### Attribute Information

1372 observations with 5 variables
Variance of Wavelet Transformed Image (continuous) 
Skewness of Wavelet Transformed Image (continuous) 
Kurtosis of Wavelet Transformed Image (continuous) 
Entropy of Image (continuous) 
Counterfeit Banknote (0 = genuine, 1 = counterfeit)

There are no missing values in the data. 762 were genuine, and 610 were counterfeit. We assume that the observations were independent of each other.
