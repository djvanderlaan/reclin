******
**:mega: IMPORTANT:**

`reclin` has been superseded by [`reclin2`](https://github.com/djvanderlaan/reclin2). In general `reclin2` has all the functionality `reclin` has with the added benefit of being much faster and memory efficient. The package is, however, not completely backwards compatible with `reclin` although the syntax is quite similar. 

There is one thing missing from `reclin2` and that is the functionality in `reclin` where data is stored partially on disk for very large data sets (this can be enables by passing `large = TRUE` to the pair generation functions). However, `reclin2` is much more memory efficient and working from disk has the disadvantage of making the computations terriby slow. For me, the maintainer, this functionality has the disadvantage of being difficult to maintain across the different platforms supported by CRAN.

**Expect `reclin` to be removed from CRAN somewhere in 2023**.

******



Introduction
------------

`reclin` implements methodology for linking records based on inexact keys. It allows for maximum flexibility by giving users full control over each step of the linking procedure. The package is built with performance and scalability in mind: the core algorithms have been implemented in `C++` and where necessary, intermediate results are stored or retrieved from disc using an efficient memory mapping scheme.

``` r
library(reclin)
library(dplyr)
```

We will work with a pair of data sets with artificial data. They are tiny, but that allows us to see what happens.

``` r
data("linkexample1", "linkexample2")
print(linkexample1)
#>   id lastname firstname    address sex postcode
#> 1  1    Smith      Anna 12 Mainstr   F  1234 AB
#> 2  2    Smith    George 12 Mainstr   M  1234 AB
#> 3  3  Johnson      Anna 61 Mainstr   F  1234 AB
#> 4  4  Johnson   Charles 61 Mainstr   M  1234 AB
#> 5  5  Johnson    Charly 61 Mainstr   M  1234 AB
#> 6  6 Schwartz       Ben  1 Eaststr   M  6789 XY
print(linkexample2)
#>   id lastname firstname       address  sex postcode
#> 1  2    Smith    Gearge 12 Mainstreet <NA>  1234 AB
#> 2  3   Jonson        A. 61 Mainstreet    F  1234 AB
#> 3  4  Johnson   Charles    61 Mainstr    F  1234 AB
#> 4  6 Schwartz       Ben        1 Main    M  6789 XY
#> 5  7 Schwartz      Anna     1 Eaststr    F  6789 XY
```

We have two data sets with personal information. The second data set contains a lot of errors, but we will try to link the second data set to the first.

Step 1: generate pairs
----------------------

In principle linkage consists of comparing each combination of records from the two data sets and determine which of those combinations (or pairs as we will call them below) belong to the same entity. In case of a perfect linkage key, it is of course, not necessary to compare all combinations of records, but when the linkage keys are imperfect and contain errors, it is in principle necessary to compare all pairs.

However, comparing all pairs can result in an intractable number of pairs: when linking two data sets with a million records there are 10<sup>12</sup> possible pairs. Therefore, some sort of reduction of the possible pairs is usually applied. In the example below, we apply *blocking*, which means that pairs are only generated when they agree on the blocking variable (in this case the postcode). This means that pairs of records that disagree on the blocking variable are not considered. Therefore, one will only use variables that can be considered without errors as blocking variable, or link multiple times with different blocking variables and combine both data sets.

The first step in (probabilistic) linkage is, therefore, generating all pairs:

``` r
p <- pair_blocking(linkexample1, linkexample2, "postcode", large = FALSE)
print(p)
#> Simple blocking
#>   Blocking variable(s): postcode
#>   First data set:  6 records
#>   Second data set: 5 records
#>   Total number of pairs: 17 pairs
#> 
#> Showing all pairs:
#>    x y
#> 1  1 1
#> 2  1 2
#> 3  1 3
#> 4  2 1
#> 5  2 2
#> 6  2 3
#> 7  3 1
#> 8  3 2
#> 9  3 3
#> 10 4 1
#> 11 4 2
#> 12 4 3
#> 13 5 1
#> 14 5 2
#> 15 5 3
#> 16 6 4
#> 17 6 5
```

As you can see, record 1 from `x` (the first data set) is compared to records 1, 2 and 3 from `y`.

Step 2: compare pairs
---------------------

We can now compare the records on their linkage keys:

``` r
p <- compare_pairs(p, by = c("lastname", "firstname", "address", "sex"))
print(p)
#> Compare
#>   By: lastname, firstname, address, sex
#> 
#> Simple blocking
#>   Blocking variable(s): postcode
#>   First data set:  6 records
#>   Second data set: 5 records
#>   Total number of pairs: 17 pairs
#> 
#> Showing all pairs:
#>    x y lastname firstname address   sex
#> 1  1 1     TRUE     FALSE   FALSE    NA
#> 2  1 2    FALSE     FALSE   FALSE  TRUE
#> 3  1 3    FALSE     FALSE   FALSE  TRUE
#> 4  2 1     TRUE     FALSE   FALSE    NA
#> 5  2 2    FALSE     FALSE   FALSE FALSE
#> 6  2 3    FALSE     FALSE   FALSE FALSE
#> 7  3 1    FALSE     FALSE   FALSE    NA
#> 8  3 2    FALSE     FALSE   FALSE  TRUE
#> 9  3 3     TRUE     FALSE    TRUE  TRUE
#> 10 4 1    FALSE     FALSE   FALSE    NA
#> 11 4 2    FALSE     FALSE   FALSE FALSE
#> 12 4 3     TRUE      TRUE    TRUE FALSE
#> 13 5 1    FALSE     FALSE   FALSE    NA
#> 14 5 2    FALSE     FALSE   FALSE FALSE
#> 15 5 3     TRUE     FALSE    TRUE FALSE
#> 16 6 4     TRUE      TRUE   FALSE  TRUE
#> 17 6 5     TRUE     FALSE    TRUE FALSE
```

As you can see, we don't need to pass the original data sets although the variables `lastname` etc. are from those original data sets. This is because a copy of the original data sets are stored with the pairs object `p` (and should you be worrying about memory: as long as the original data sets are not modified the data sets are not actually copied).

The default comparison function returns `TRUE` when the linkage keys agree and false when they don't. However, when looking at the original data sets, we can see that most of our linkage keys are string variables that contain typing errors. The quality of our linkage could be improved if we could use a similarity score to compare the two strings: a high score means that the two strings are very similar a value close to zero means that the strings are very different.

Below we use the `jaro_winkler` similarity score to compare all fields:

``` r
p <- compare_pairs(p, by = c("lastname", "firstname", "address", "sex"),
  default_comparator = jaro_winkler(0.9), overwrite = TRUE)
print(p)
#> Compare
#>   By: lastname, firstname, address, sex
#> 
#> Simple blocking
#>   Blocking variable(s): postcode
#>   First data set:  6 records
#>   Second data set: 5 records
#>   Total number of pairs: 17 pairs
#> 
#> Showing all pairs:
#>    x y lastname firstname   address sex
#> 1  1 1 1.000000 0.4722222 0.9230769  NA
#> 2  1 2 0.000000 0.5833333 0.8641026   1
#> 3  1 3 0.447619 0.4642857 0.9333333   1
#> 4  2 1 1.000000 0.8888889 0.9230769  NA
#> 5  2 2 0.000000 0.0000000 0.8641026   0
#> 6  2 3 0.447619 0.5396825 0.9333333   0
#> 7  3 1 0.447619 0.4722222 0.8641026  NA
#> 8  3 2 0.952381 0.5833333 0.9230769   1
#> 9  3 3 1.000000 0.4642857 1.0000000   1
#> 10 4 1 0.447619 0.6428571 0.8641026  NA
#> 11 4 2 0.952381 0.0000000 0.9230769   0
#> 12 4 3 1.000000 1.0000000 1.0000000   0
#> 13 5 1 0.447619 0.5555556 0.8641026  NA
#> 14 5 2 0.952381 0.0000000 0.9230769   0
#> 15 5 3 1.000000 0.8492063 1.0000000   0
#> 16 6 4 1.000000 1.0000000 0.6111111   1
#> 17 6 5 1.000000 0.5277778 1.0000000   0
```

Step 3: score pairs
-------------------

The next step in the process, is to determine which pairs of records belong to the same entity and which do not. There are numerous ways to do this. One possibility is to label some of the pairs as match or no match, and use some machine learning algorithm to predict the match status using the comparison vectors. Another, method, is to score the pairs based on the comparison vectors and select those with a score above some threshold. The simplest way to score the pairs, is to calculate the sum of the comparison vectors. That is what `score_simsum` does:

``` r
p <- score_simsum(p, var = "simsum")
print(p)
#> Compare
#>   By: lastname, firstname, address, sex
#> 
#> Simple blocking
#>   Blocking variable(s): postcode
#>   First data set:  6 records
#>   Second data set: 5 records
#>   Total number of pairs: 17 pairs
#> 
#> Showing all pairs:
#>    x y lastname firstname   address sex    simsum
#> 1  1 1 1.000000 0.4722222 0.9230769  NA 2.3952991
#> 2  1 2 0.000000 0.5833333 0.8641026   1 2.4474359
#> 3  1 3 0.447619 0.4642857 0.9333333   1 2.8452381
#> 4  2 1 1.000000 0.8888889 0.9230769  NA 2.8119658
#> 5  2 2 0.000000 0.0000000 0.8641026   0 0.8641026
#> 6  2 3 0.447619 0.5396825 0.9333333   0 1.9206349
#> 7  3 1 0.447619 0.4722222 0.8641026  NA 1.7839438
#> 8  3 2 0.952381 0.5833333 0.9230769   1 3.4587912
#> 9  3 3 1.000000 0.4642857 1.0000000   1 3.4642857
#> 10 4 1 0.447619 0.6428571 0.8641026  NA 1.9545788
#> 11 4 2 0.952381 0.0000000 0.9230769   0 1.8754579
#> 12 4 3 1.000000 1.0000000 1.0000000   0 3.0000000
#> 13 5 1 0.447619 0.5555556 0.8641026  NA 1.8672772
#> 14 5 2 0.952381 0.0000000 0.9230769   0 1.8754579
#> 15 5 3 1.000000 0.8492063 1.0000000   0 2.8492063
#> 16 6 4 1.000000 1.0000000 0.6111111   1 3.6111111
#> 17 6 5 1.000000 0.5277778 1.0000000   0 2.5277778
```

The disadvantage of `score_simsum` is that it doesn't take into account that the amount of information in agreement or disagreement on a variable depends on the variable. For example, agreement on sex doesn't tell us much: when our data sets contain 50% men an 50% women, there is a 50% chance that two random records agree on sex. On the other hand the probability that two random records agree on last name is much lower. Therefore, agreement on last name makes it much more likely that the two records belong to the same entity.

This is what the probabilistic linkage framework initially formalised by Fellegi and Sunter tries to do. The function `problink_em` uses an EM-algorithm to estimate the so called m- and u-probabilities for each of the linkage variables. The m-probability is the probability that two records concerning the same entity agree on the linkage variable; this means that the m-probability corresponds to the probability that there is an error in the linkage variables. The u-probability is the probability that two records belonging to different entities agree on a variable. For a variable with few categories (such as sex) this probability will be large, while for a variable with a large number of categories (such as last name) this probability will be small.

``` r
m <- problink_em(p)
print(m)
#> M- and u-probabilities estimated by the EM-algorithm:
#>   Variable M-probability U-probability
#>   lastname     0.9995993  1.148282e-03
#>  firstname     0.2000808  6.534287e-11
#>    address     0.8999198  2.861829e-01
#>        sex     0.3001260  2.855427e-01
#> 
#> Matching probability: 0.5882748.
```

These m- and u-probabilities can be used to score the pairs:

``` r
p <- score_problink(p, model = m, var = "weight")
print(p)
#> Compare
#>   By: lastname, firstname, address, sex
#> 
#> Simple blocking
#>   Blocking variable(s): postcode
#>   First data set:  6 records
#>   Second data set: 5 records
#>   Total number of pairs: 17 pairs
#> 
#> Showing all pairs:
#>    x y lastname firstname   address sex    simsum     weight
#> 1  1 1 1.000000 0.4722222 0.9230769  NA 2.3952991  7.7138545
#> 2  1 2 0.000000 0.5833333 0.8641026   1 2.4474359 -6.8623638
#> 3  1 3 0.447619 0.4642857 0.9333333   1 2.8452381  0.8024181
#> 4  2 1 1.000000 0.8888889 0.9230769  NA 2.8119658  8.6108449
#> 5  2 2 0.000000 0.0000000 0.8641026   0 0.8641026 -7.2330326
#> 6  2 3 0.447619 0.5396825 0.9333333   0 1.9206349  0.7929395
#> 7  3 1 0.447619 0.4722222 0.8641026  NA 1.7839438  0.6008053
#> 8  3 2 0.952381 0.5833333 0.9230769   1 3.4587912  4.0666230
#> 9  3 3 1.000000 0.4642857 1.0000000   1 3.4642857  7.9375333
#> 10 4 1 0.447619 0.6428571 0.8641026  NA 1.9545788  0.7705671
#> 11 4 2 0.952381 0.0000000 0.9230769   0 1.8754579  3.6959542
#> 12 4 3 1.000000 1.0000000 1.0000000   0 3.0000000 29.7364771
#> 13 5 1 0.447619 0.5555556 0.8641026  NA 1.8672772  0.6709008
#> 14 5 2 0.952381 0.0000000 0.9230769   0 1.8754579  3.6959542
#> 15 5 3 1.000000 0.8492063 1.0000000   0 2.8492063  8.5499432
#> 16 6 4 1.000000 1.0000000 0.6111111   1 3.6111111 28.9246883
#> 17 6 5 1.000000 0.5277778 1.0000000   0 2.5277778  7.9174058
```

The higher the weight the more likely the two pairs belong to the same entity/are a match.

Step 4: select pairs
--------------------

The final step is to select the pairs that are considered to belong to the same entities. The simplest method is to select all pairs above a certain threshold

``` r
p <- select_threshold(p, "weight", var = "threshold", threshold = 8)
print(p)
#> Compare
#>   By: lastname, firstname, address, sex
#> 
#> Simple blocking
#>   Blocking variable(s): postcode
#>   First data set:  6 records
#>   Second data set: 5 records
#>   Total number of pairs: 17 pairs
#> 
#> Showing all pairs:
#>    x y lastname firstname   address sex    simsum     weight threshold
#> 1  1 1 1.000000 0.4722222 0.9230769  NA 2.3952991  7.7138545     FALSE
#> 2  1 2 0.000000 0.5833333 0.8641026   1 2.4474359 -6.8623638     FALSE
#> 3  1 3 0.447619 0.4642857 0.9333333   1 2.8452381  0.8024181     FALSE
#> 4  2 1 1.000000 0.8888889 0.9230769  NA 2.8119658  8.6108449      TRUE
#> 5  2 2 0.000000 0.0000000 0.8641026   0 0.8641026 -7.2330326     FALSE
#> 6  2 3 0.447619 0.5396825 0.9333333   0 1.9206349  0.7929395     FALSE
#> 7  3 1 0.447619 0.4722222 0.8641026  NA 1.7839438  0.6008053     FALSE
#> 8  3 2 0.952381 0.5833333 0.9230769   1 3.4587912  4.0666230     FALSE
#> 9  3 3 1.000000 0.4642857 1.0000000   1 3.4642857  7.9375333     FALSE
#> 10 4 1 0.447619 0.6428571 0.8641026  NA 1.9545788  0.7705671     FALSE
#> 11 4 2 0.952381 0.0000000 0.9230769   0 1.8754579  3.6959542     FALSE
#> 12 4 3 1.000000 1.0000000 1.0000000   0 3.0000000 29.7364771      TRUE
#> 13 5 1 0.447619 0.5555556 0.8641026  NA 1.8672772  0.6709008     FALSE
#> 14 5 2 0.952381 0.0000000 0.9230769   0 1.8754579  3.6959542     FALSE
#> 15 5 3 1.000000 0.8492063 1.0000000   0 2.8492063  8.5499432      TRUE
#> 16 6 4 1.000000 1.0000000 0.6111111   1 3.6111111 28.9246883      TRUE
#> 17 6 5 1.000000 0.5277778 1.0000000   0 2.5277778  7.9174058     FALSE
```

The select functions add a (logical) variable to the data set indicating whether a pairs is selected or not.

In this case we know which records truly belong to each other. We can use that to evaluate the linkage:

``` r
p <- add_from_x(p, id_x = "id")
print(p)
#> Compare
#>   By: lastname, firstname, address, sex
#> 
#> Simple blocking
#>   Blocking variable(s): postcode
#>   First data set:  6 records
#>   Second data set: 5 records
#>   Total number of pairs: 17 pairs
#> 
#> Showing all pairs:
#>    x y lastname firstname   address sex    simsum     weight threshold
#> 1  1 1 1.000000 0.4722222 0.9230769  NA 2.3952991  7.7138545     FALSE
#> 2  1 2 0.000000 0.5833333 0.8641026   1 2.4474359 -6.8623638     FALSE
#> 3  1 3 0.447619 0.4642857 0.9333333   1 2.8452381  0.8024181     FALSE
#> 4  2 1 1.000000 0.8888889 0.9230769  NA 2.8119658  8.6108449      TRUE
#> 5  2 2 0.000000 0.0000000 0.8641026   0 0.8641026 -7.2330326     FALSE
#> 6  2 3 0.447619 0.5396825 0.9333333   0 1.9206349  0.7929395     FALSE
#> 7  3 1 0.447619 0.4722222 0.8641026  NA 1.7839438  0.6008053     FALSE
#> 8  3 2 0.952381 0.5833333 0.9230769   1 3.4587912  4.0666230     FALSE
#> 9  3 3 1.000000 0.4642857 1.0000000   1 3.4642857  7.9375333     FALSE
#> 10 4 1 0.447619 0.6428571 0.8641026  NA 1.9545788  0.7705671     FALSE
#> 11 4 2 0.952381 0.0000000 0.9230769   0 1.8754579  3.6959542     FALSE
#> 12 4 3 1.000000 1.0000000 1.0000000   0 3.0000000 29.7364771      TRUE
#> 13 5 1 0.447619 0.5555556 0.8641026  NA 1.8672772  0.6709008     FALSE
#> 14 5 2 0.952381 0.0000000 0.9230769   0 1.8754579  3.6959542     FALSE
#> 15 5 3 1.000000 0.8492063 1.0000000   0 2.8492063  8.5499432      TRUE
#> 16 6 4 1.000000 1.0000000 0.6111111   1 3.6111111 28.9246883      TRUE
#> 17 6 5 1.000000 0.5277778 1.0000000   0 2.5277778  7.9174058     FALSE
#>    id_x
#> 1     1
#> 2     1
#> 3     1
#> 4     2
#> 5     2
#> 6     2
#> 7     3
#> 8     3
#> 9     3
#> 10    4
#> 11    4
#> 12    4
#> 13    5
#> 14    5
#> 15    5
#> 16    6
#> 17    6
```

The `add_from_x` function adds variables from the original `x`. As was mentioned before the two data sets are stored in `p`.

``` r
p <- add_from_y(p, id_y = "id")
p$true <- p$id_x == p$id_y
table(as.data.frame(p[c("true", "threshold")]))
#>        threshold
#> true    FALSE TRUE
#>   FALSE    12    1
#>   TRUE      1    3
```

We see that three of the four matches that should have been found have indeed been found (the recall is 3/4) and we have one false link (sensitivity is 1/4).

Using a threshold, does not take into account the fact that often we know that one record from the first data set can be linked to at most one record from the second data set and vice versa. If we make the threshold low enough we have more links than records in either data set. `reclin` contains two functions that force one-to-one linkage: `select_greedy` and `select_n_to_m`. The first is fast (it selects pairs starting from the highest score; pairs are only selected when each of the records in a pair have not been selected previously); the second is slower, but can lead to better results (it tries to optimise to total score of the selected records under the restriction that each record can be selected only once):

``` r
p <- select_greedy(p, "weight", var = "greedy", threshold = 0)
table(as.data.frame(p[c("true", "greedy")]))
#>        greedy
#> true    FALSE TRUE
#>   FALSE    13    0
#>   TRUE      0    4
```

``` r
p <- select_n_to_m(p, "weight", var = "ntom", threshold = 0)
table(as.data.frame(p[c("true", "ntom")]))
#>        ntom
#> true    FALSE TRUE
#>   FALSE    13    0
#>   TRUE      0    4
```

Perfection!

The final, last step
--------------------

The real final step is to create the linked data set. We now know which pairs are to be linked, but we still have to actually link them. `link` does that (the optional arguments `all_x` and `all_y` control the type of linkage):

``` r
linked_data_set <- link(p)
print(linked_data_set)
#>   id.x lastname.x firstname.x  address.x sex.x postcode.x id.y lastname.y
#> 1    2      Smith      George 12 Mainstr     M    1234 AB    2      Smith
#> 2    3    Johnson        Anna 61 Mainstr     F    1234 AB    3     Jonson
#> 3    4    Johnson     Charles 61 Mainstr     M    1234 AB    4    Johnson
#> 4    6   Schwartz         Ben  1 Eaststr     M    6789 XY    6   Schwartz
#> 5    1      Smith        Anna 12 Mainstr     F    1234 AB   NA       <NA>
#> 6    5    Johnson      Charly 61 Mainstr     M    1234 AB   NA       <NA>
#> 7   NA       <NA>        <NA>       <NA>  <NA>       <NA>    7   Schwartz
#>   firstname.y     address.y sex.y postcode.y
#> 1      Gearge 12 Mainstreet  <NA>    1234 AB
#> 2          A. 61 Mainstreet     F    1234 AB
#> 3     Charles    61 Mainstr     F    1234 AB
#> 4         Ben        1 Main     M    6789 XY
#> 5        <NA>          <NA>  <NA>       <NA>
#> 6        <NA>          <NA>  <NA>       <NA>
#> 7        Anna     1 Eaststr     F    6789 XY
```

All together now
----------------

The functions have been designed to be usable with pipe operators, so the entire linkage process could be written as:

``` r
library(dplyr)

linked_data_set <- pair_blocking(linkexample1, linkexample2, "postcode") %>%
  compare_pairs(by = c("lastname", "firstname", "address", "sex"),
      default_comparator = jaro_winkler(0.9)) %>%
  score_problink(var = "weight") %>%
  select_n_to_m("weight", var = "ntom", threshold = 0) %>%
  link()
```
