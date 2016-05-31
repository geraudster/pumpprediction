DeepLearning with H2O and R
========================================================
autosize: true
#subtitle: Lightning Talk
author: Géraud Dugé de Bernonville
#job: Java & Big Data Consultant
#license: by-sa

Qui suis-je ?
========================================================

@geraudster

Consultant Java & Big Data @ Valtech

Ateliers R Toulouse Data Science

Compétitions drivendata, kaggle, datascience.net

Deep Learning
========================================================

Réseau de neurone

Deep == Plusieurs couches

Les typologies de réseau
========================================================

Supervisé: 

* Deep Belief Network
* CNN = Convolutional
* RNN = Recurrent

Non supervisé:

* autoencoder

H2O
========================================================

Outil pour l'analyse de données:

* algorithmes de Machine Learning
* deep learning
* distribué, multi-threadé

Open-source

Application avec R
========================================================

Initialisation d'un cluster local:

```r
library(h2o)
localH2O <- h2o.init(min_mem_size = '5G', nthreads = 4)
```

Chargement des données:

```r
trainset.hex <- h2o.uploadFile(path = '~/projects/pumpprediction/trainset_values.csv',
                               destination_frame = 'trainset.hex',
                               sep = ',', header = TRUE)
```

Première version
========================================================

Utilisation des paramètres par défaut (2 couches de 200 neurones):



First Slide
========================================================

For more details on authoring R presentations please visit <https://support.rstudio.com/hc/en-us/articles/200486468>.

- Bullet 1
- Bullet 2
- Bullet 3

Slide With Code
========================================================


```r
summary(cars)
```

```
     speed           dist       
 Min.   : 4.0   Min.   :  2.00  
 1st Qu.:12.0   1st Qu.: 26.00  
 Median :15.0   Median : 36.00  
 Mean   :15.4   Mean   : 42.98  
 3rd Qu.:19.0   3rd Qu.: 56.00  
 Max.   :25.0   Max.   :120.00  
```

Slide With Plot
========================================================

![plot of chunk unnamed-chunk-5](DeepLearning with H2O and R 2-figure/unnamed-chunk-5-1.png)
