---
title: R4DS - Wrangle section
tags: [r, course]
style: none
# primary / secondary / success / danger / warning / info / light / dark (choose one only)
description: Notes on all of the chapters in the Wrangle section of R4DS book
---

-   [Continuation of the R4DS book -
    Wrangle](#continuation-of-the-r4ds-book---wrangle)
    -   [Chapter 10 - Tibbles](#chapter-10---tibbles)
        -   [Exercises](#exercises)
    -   [Chapter 11 - Data import](#chapter-11---data-import)
        -   [Exercises](#exercises-1)
        -   [Chapter 12 - Tidy data](#chapter-12---tidy-data)
        -   [12.3 Pivoting](#pivoting)
        -   [12.4 Separating and Uniting](#separating-and-uniting)
        -   [12.5 Missing values](#missing-values)
        -   [Case Study](#case-study)
    -   [13 Relational Data](#relational-data)
        -   [Exercises](#exercises-6)
        -   [13.3 Keys](#keys)
        -   [13.4 Mutating Joins](#mutating-joins)
        -   [13.5 Filtering Joins](#filtering-joins)
    -   [14 Strings](#strings)
        -   [14.2.3 Subsetting strings](#subsetting-strings)
        -   [14.2.4 Locales](#locales)
-   [COME BACK TO THIS AFTER FUNCTION
    CHAPTER](#come-back-to-this-after-function-chapter)
    -   [14.3 Matching patterns with regular
        expressions](#matching-patterns-with-regular-expressions)
    -   [14.4 Tools](#tools)
    -   [14.5 Other Types of pattern](#other-types-of-pattern)
    -   [14.6 Other uses of regular
        expressions](#other-uses-of-regular-expressions)
    -   [14.7 Stringi](#stringi)
    -   [15 Factors](#factors)
        -   [Exercises](#exercises-22)
        -   [15.4 Modifying factor order](#modifying-factor-order)
        -   [15.5 Modifying factor levels](#modifying-factor-levels)
    -   [16 Dates and Times](#dates-and-times)
        -   [16.2.3 From other types](#from-other-types)
        -   [16.3 Date-time components](#date-time-components)
        -   [16.4 Time spans](#time-spans)
        -   [16.5 Time zones](#time-zones)

Continuation of the R4DS book - Wrangle
=======================================

Section can be found [here](https://r4ds.had.co.nz/wrangle-intro.html)

Chapter 10 - Tibbles
--------------------

We start with Tibbles, where we distinguish between data frames and a
*tibble*

    as_tibble(iris)

    # To create our own
    tibble(
      x = 1:5,
      y = 1,
      z = x ^ 2 + y
    )

One of the main differences between data frames and tibbles is that a
tibble only prints the first 10 rows - which makes sure it doesn’t
overwhelm the machine when working with big datasets. If we want to
expand that however, there is a way

    nycflights13::flights %>%
      print(n = 10, width = Inf)

*Subsetting..*

    df <- tibble(
      x = runif(5),
      y = rnorm(5)
    )

    df %>% .$x
    df %>% .[["x"]]

##### Exercises

-   How can you tell if object is a tibble?

<!-- -->

    # Tibble or Dataframe?
    str(mtcars)
    ## Tibble print-out is much shorter and it says it's a tibble. Can also use str to determine.

-   Compare and contrast following operations - what is different?

<!-- -->

    df <- data.frame(abc = 1, xyz = "a")
    df$x
    df[, "xyz"]
    df[, c("abc", "xyz")]#
    # The operations aren't informative at all. Just returns one value

-   If you have the name of a variable stored in an object, how can you
    extract the reference variable from a tibble?

<!-- -->

    var <- "mpg"

You can use the double bracket, like `df[[var]]`. You cannot use the
dollar sign, because `df$var` would look for a column named `var`.

Chapter 11 - Data import
------------------------

##### Exercises

**What function would you use to read a file where fields were separated
with ‘|’**

In this case we’d simply use `read_delim(delim="|")`

**Apart from `file`, `skip`, and `comment`, what other arguments do
`read_csv` and `read_tsv` have in common?**

They have the following in common:

    union(names(formals(read_csv)), names(formals(read_tsv)))

**What are the most important arguments to `read_fwf`?**

The most important is col\_positions which tells the function where data
columns are

-   Sometimes strings in a CSV file contain commas. To prevent them from
    causing problems they need to be surrounded by a quoting character,
    like " or ’. By default, `read_csv()` assumes that the quoting
    character will be “.” What argument to `read_csv()` do you need to
    specify to read the following text into a data frame?

`"x,y\n1,'a,b'"`

In this case we can use the `quote` argument, so
`read_csv(x, quote="`")\`

-   Identify what is wrong with each of the following inline csv files:

<!-- -->

    read_csv("a,b\n1,2,3\n4,5,6")       # there are only 2 column names but rows have 3 columns so last column is dropped
    read_csv("a,b,c\n1,2\n1,2,3,4")     # there are 3 column names but first row has 2 data points and third has 4
    read_csv("a,b\n\"1")                # didn't close brackets around first column name so second is dropped
    read_csv("a,b\n1,2\na,b")
    read_csv("a;b\n1;3")                # separated by ';' instead of a comma, should use csv2 instead

### Chapter 12 - Tidy data

There are three interrelated rules which make a dataset tidy:

1.  Each variable must have its own column

2.  Each observation must have its own row

3.  Each value must have its own cell

Some of the things we can do when the data is tidy:

    # table1
    # table2
    # table3
    # table4a
    # table4b

    table1 %>%
      mutate(cases_by_population = cases / population * 10000)

    table1 %>%
      count(year, wt = cases)

    ggplot(table1,
           aes(year, cases)) +
      geom_line(aes(group = country), colour = "grey50") +
      geom_point(aes(colour = country))

##### Exercises

**Using prose, describe how variables and observations are organised in
each of the sample tables**

In `table1` everything is independent - each variable has its own
column, each observation has its own row, and each value has its own
cell

In `table2` the ‘type’ column merges two different variables which makes
it harder to summarise

In `table3` they get rid of the ‘type’ column altogether and instead
show the calculation for the rate. This means we’d have to use a
function to split the two out on ‘/’ and then calculate the result

In `table4a` there’s no description of what data is being shown to us.
Just a country name, year number and a random number

In `table4b` it’s the same as a except using population instead of cases

**Compute the `rate` for `table2`, and `table4a` + `table4b`. Will need
to perform 4 operations:**

-   Extract number of TB cases per country per year

-   Extract matching population per country per year

-   Divide cases by population and multiple by 10000

-   Store back in appropriate place

Which representation is easiest to work with? Which is hardest?

**table 2**

    table2 %>%
      spread(type, count) %>%
      mutate(case_rate = cases / population * 10000)

**table 4**

    table4c <-
      tibble(
        country = table4a$country,
        '1991' = table4a[["1999"]] / table4b[["1999"]] * 10000,
        '2000' = table4a[["2000"]] / table4b[["2000"]] * 10000
      )


    table4a_edit <- table4a %>%
      pivot_longer(c('1999', '2000'), names_to = "year", values_to = "cases")

    table4b_edit <- table4b %>%
      pivot_longer(c('1999', '2000'), names_to = "year", values_to = "population")

    table4a_edit %>%
      inner_join(table4b_edit, by=c("country", "year")) %>%
      mutate(case_rate = cases / population * 10000)

Personally find table2 the easiest because of the spread. Plus, you have
all the normal variables still there so can perform other operations

1.  Recreate the plot showing change in cases over time using `table2`
    instead of `table1`. What do we need to do?

<!-- -->

    table2 %>%
      spread(type, count) %>%
      ggplot(aes(year, cases)) +
      geom_line(aes(group = country)) +
      geom_point(aes(colour = country))

### 12.3 Pivoting

When dealing with data the first step is always to figure out what the
variables and observations are. Second step is usually to resolve one of
two common problems:

-   One variable might be spread across multiple columns

-   One observation might be scattered across multiple rows

Using `longer` we can fix a dataset where column names are not names of
variables but values

    table4a %>%
      pivot_longer(c('1999', '2000'), names_to = "year", values_to = "cases")

The above takes `table4a`, where year values are separate columns, and
creates a single year column instead and assigns values accordingly

`wider` is the opposite - we use it when an observation is scattered
across multiple rows. E.g. for table2:

    table2 %>%
      pivot_wider(names_from = type, values_from = count)

Here we take `table2` where two sepate values are in one column and
instead converts them into a column of their own (cases and population)

##### Exercises

**Why are `pivot_longer` and `pivot_wider` not perfectly symmetrical?**

    stocks <- tibble(
      year   = c(2015, 2015, 2016, 2016),
      half  = c(   1,    2,     1,    2),
      return = c(1.88, 0.59, 0.92, 0.17)
    )
    stocks %>% 
      pivot_wider(names_from = year, values_from = return) %>% 
      pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return", names_ptype = list(year = double()))

They’re not symmetrical because column type information is lost in the
operation

**Why does the below code fail?**

    table4a %>% 
      pivot_longer(c(1999, 2000), names_to = "year", values_to = "cases")

Need to use quotation marks around 1999 and 2000 in this case

1.  Tidy the below tibble - does it need to made longer or wider?

<!-- -->

    preg <- tribble(
      ~pregnant, ~male, ~female,
      "yes",     NA,    10,
      "no",      20,    12
    )

    preg %>%
      pivot_longer(c(male, female), names_to = "gender")

### 12.4 Separating and Uniting

**Separate**

`separate` allows us to pull apart one column into multiple columns -
splitting whereever a separator appears

    table3 %>%
      separate(rate, into = c("cases", "population"))

Here, because in the column there’s a ‘/’ separator, the command easily
picks up which is which and puts the numbers into different columns. By
default `separate()` will split when it sees a non-alphanumeric
character but we can use the `sep='n'` parameter (which is a RegEx) to
specify any separator. It’s also important to remember that by default
it’ll classify the values it separates as character columns. If we want
it to try to convert it into the actual types we can use
`convert = TRUE`

    table3 %>%
      separate(rate, into = c("cases", "population"), sep = "/", convert = TRUE)

You can take it a step further and pass integers to `sep` which will
lead the function to interpret the integers as positions to split.

-   Positive values start at 1 on far-left of strings, negative value
    start at -1 on far-right

<!-- -->

    table3 %>%
      separate(year, into = c("century", "year"), sep = 2)

**Unite**

`unite` does the inverse of `separate` - it combines multiple columns
into a single column. If we want to unite the previous table where we
split the years into century and year, we would simply do the below. By
default it’ll place an underscore between the values it unites, if you
want to avoid that we use `sep`

    table5 %>%
      unite(new, century, year, sep = "")

**Exercises**

-   What do the `extra` and `fill` arguments do in `separate()`?
    Experiment with the various options for the following two toy
    datasets.

<!-- -->

    tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
      separate(x, c("one", "two", "three"), extra = "merge")

    tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
      separate(x, c("one", "two", "three"), fill = "left")

    ?separate

The `extra` argument controls what happens when there are too many
values and has 3 different parameters. It defaults to `warn` which just
emits a warning and drops extra values. You can change it to `drop`
which drops any extra values without a warning and lastly there is
`merge`, which merges data together if there isn’t enough space. `fill`
on the other hand, controls when there aren’t enough pieces. You can use
`warn` (Default), `right` which fills the right side with missing values
or `left` which does it the other way around.

-   Both `unite` and `separate` have a `remove` argument. What does it
    do? Why would you set it to `FALSE`?

The `remove` parameter keeps the column you’re running the initial
command on. So where we split out cases and population before, it would
remove the ‘rate’ column by default. If we put `remove` to `FALSE` it’ll
keep the rate column. This might be good in instances where you want to
keep the original column such as when you have a date column with months
and years - you split it out into years / months to run YoY and MoM
comparisons but you may also want to do a time line of data and will
need the joined together option.

-   Compare and contrast `separate` and `extract`. Why are there three
    variations of separation (by position, by separator, and with
    groups) but only one unite?

`separate` splits column into multiple columns by separator as long as
the `sep` argument is a character vector, or by character. `extract`
uses a regular expression to specify groups in character vector and
splits that into multiple columns. Using RegEx is more complicated but
also a lot more flexible. `extract` uses a similar structure to
`separate` - you tell it the data, specify `into` and then specify
`regex`.

    tibble(x = c("X_1", "X_2", "AA_1", "AA_2")) %>%
      extract(x, into = c("variable", "id"), regex = "([A-Z])_([0-9])")

    ## # A tibble: 4 x 2
    ##   variable id   
    ##   <chr>    <chr>
    ## 1 X        1    
    ## 2 X        2    
    ## 3 A        1    
    ## 4 A        2

    # example with position
    tibble(x = c("X1", "X2", "Y1", "Y2")) %>%
      extract(x, c("variable", "id"), regex = "([A-Z])([0-9])")

    ## # A tibble: 4 x 2
    ##   variable id   
    ##   <chr>    <chr>
    ## 1 X        1    
    ## 2 X        2    
    ## 3 Y        1    
    ## 4 Y        2

    # example that separate could not parse
    tibble(x = c("X1", "X20", "AA11", "AA2")) %>%
      extract(x, c("variable", "id"), regex = "([A-Z]+)([0-9]+)")

    ## # A tibble: 4 x 2
    ##   variable id   
    ##   <chr>    <chr>
    ## 1 X        1    
    ## 2 X        20   
    ## 3 AA       11   
    ## 4 AA       2

### 12.5 Missing values

A value can be missing in two ways:

-   **Explicitly**, when flagged with `NA`

-   **Implicitly**, simply not present in data

<!-- -->

    stocks <- tibble(
      year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
      qtr    = c(   1,    2,    3,    4,    2,    3,    4),
      return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
    )

    stocks

    ## # A tibble: 7 x 3
    ##    year   qtr return
    ##   <dbl> <dbl>  <dbl>
    ## 1  2015     1   1.88
    ## 2  2015     2   0.59
    ## 3  2015     3   0.35
    ## 4  2015     4  NA   
    ## 5  2016     2   0.92
    ## 6  2016     3   0.17
    ## 7  2016     4   2.66

In this case we get two missing values - one is **explicit** and shows
us `NA`, the other is simply missing (Q1, 2016). To decide what to do
with missing values we can make them visible

    stocks %>% 
      pivot_wider(names_from = year, values_from = return)

And we can also use `pivot_longer` to drop the missing values completely
with the `values_drop_na` parameter

    stocks %>% 
      pivot_wider(names_from = year, values_from = return) %>% 
      pivot_longer(
        cols = c(`2015`, `2016`), 
        names_to = "year", 
        values_to = "return", 
        values_drop_na = TRUE
      )

**complete**

A great command to remember for making missing values **explicit** is
`complete()`. It takes a set of columns and finds all unique
combinations. It then ensures dataset contains all those values and
fills in `NA`s where necessary

    stocks %>%
      complete(year, qtr)

**fill**

Sometimes with data entry people may not repeat the value in one column
if the rows preceding all belong to the same value. An example would be
when you’re taking lunch orders in a spreadsheet - in column A you might
have someone’s name and then in column B you have different courses on a
different rows. This means that you have one name to 4 rows. `fill` can
help us with this, it takes in the column name and populates missing
values with the value from preceding rows. You can also pass a
`direction` parameter for example if you need to fill horizontally.

    treatment %>%
      fill(person)

##### Exercises

-   Compare and contrast the `fill` arguments to `pivot_wider()` and
    `complete()`.

They fill different needs. `pivot_wider` is very flexible and allows us
to `NA`s with a specified value and also allows us to easily identify
`NA`s. `complete` is a tool to quickly do EDA and find missing values.
`fill` is used when you are certain missing values are a simple
data-entry mistake and you can take the values from preceding rows.

-   What does the direction argument to fill() do?

The direction argument allows us to fill in any way we want. For
example, if we need to fill values horizontally.

### Case Study

Using the `who` dataset included with tidyr, we can apply some of the
things we learnt above to clean the data and make it more readable.
There are things like:

-   Redundant columns

-   Odd variable codes

-   Many missing values

<!-- -->

    who

    ## # A tibble: 7,240 x 60
    ##    country iso2  iso3   year new_sp_m014 new_sp_m1524 new_sp_m2534 new_sp_m3544
    ##    <chr>   <chr> <chr> <int>       <int>        <int>        <int>        <int>
    ##  1 Afghan~ AF    AFG    1980          NA           NA           NA           NA
    ##  2 Afghan~ AF    AFG    1981          NA           NA           NA           NA
    ##  3 Afghan~ AF    AFG    1982          NA           NA           NA           NA
    ##  4 Afghan~ AF    AFG    1983          NA           NA           NA           NA
    ##  5 Afghan~ AF    AFG    1984          NA           NA           NA           NA
    ##  6 Afghan~ AF    AFG    1985          NA           NA           NA           NA
    ##  7 Afghan~ AF    AFG    1986          NA           NA           NA           NA
    ##  8 Afghan~ AF    AFG    1987          NA           NA           NA           NA
    ##  9 Afghan~ AF    AFG    1988          NA           NA           NA           NA
    ## 10 Afghan~ AF    AFG    1989          NA           NA           NA           NA
    ## # ... with 7,230 more rows, and 52 more variables: new_sp_m4554 <int>,
    ## #   new_sp_m5564 <int>, new_sp_m65 <int>, new_sp_f014 <int>,
    ## #   new_sp_f1524 <int>, new_sp_f2534 <int>, new_sp_f3544 <int>,
    ## #   new_sp_f4554 <int>, new_sp_f5564 <int>, new_sp_f65 <int>,
    ## #   new_sn_m014 <int>, new_sn_m1524 <int>, new_sn_m2534 <int>,
    ## #   new_sn_m3544 <int>, new_sn_m4554 <int>, new_sn_m5564 <int>,
    ## #   new_sn_m65 <int>, new_sn_f014 <int>, new_sn_f1524 <int>,
    ## #   new_sn_f2534 <int>, new_sn_f3544 <int>, new_sn_f4554 <int>,
    ## #   new_sn_f5564 <int>, new_sn_f65 <int>, new_ep_m014 <int>,
    ## #   new_ep_m1524 <int>, new_ep_m2534 <int>, new_ep_m3544 <int>,
    ## #   new_ep_m4554 <int>, new_ep_m5564 <int>, new_ep_m65 <int>,
    ## #   new_ep_f014 <int>, new_ep_f1524 <int>, new_ep_f2534 <int>,
    ## #   new_ep_f3544 <int>, new_ep_f4554 <int>, new_ep_f5564 <int>,
    ## #   new_ep_f65 <int>, newrel_m014 <int>, newrel_m1524 <int>,
    ## #   newrel_m2534 <int>, newrel_m3544 <int>, newrel_m4554 <int>,
    ## #   newrel_m5564 <int>, newrel_m65 <int>, newrel_f014 <int>,
    ## #   newrel_f1524 <int>, newrel_f2534 <int>, newrel_f3544 <int>,
    ## #   newrel_f4554 <int>, newrel_f5564 <int>, newrel_f65 <int>

Some of the things that immediately stand out are: `country`, `iso2` and
`iso3` are all doing the same thing (specifying country). The other
columns like `new_sp_randnumber` we don’t know yet what they are but
considering the structure, it looks like they’re values instead of
variables.

    who1 <- who %>%
      pivot_longer(
        cols = new_sp_m014:newrel_f65,
        names_to = "key",
        values_to = "cases",
        values_drop_na = TRUE
       )

And then we can count our new values quickly:

    who1 %>%
      count(key)

We can read the documentation for the data to figure out what the
acronyms stand. Because of how the original data was input, we need to
rename newrel to new\_rel so there’s consistency across the values (we
have new\_ep and new\_sn)

    who2 <-  who1 %>%
      mutate(names_from = str_replace(key, "newrel", "new_rel"))

    who2

Now that’s done we can easily separate the values in each code with two
passes of `separate`

    who3 <- who2 %>%
      separate(key, c("new", "type", "sexage"), sep = "_")
      select(-new, -iso2, -iso3) %>%
      separate(sexage, c("sex", "age"), sep = 1)
      
    who3

Instead of breaking it into chunks, doing it all at once:

    who %>%
      pivot_longer(
        cols = new_sp_m014:newrel_f65,
        names_to = "key",
        values_to = "cases",
        values_drop_na = TRUE
      ) %>%
      mutate(
        key = str_replace(key, "newrel", "new_rel")
      ) %>%
      separate(key, c("new", "var", "sexage")) %>%
      separate(sexage, c("sex", "age"), sep = 1)

##### Exercises

-   In this case study I set values\_drop\_na = TRUE just to make it
    easier to check that we had the correct values. Is this reasonable?
    Think about how missing values are represented in this dataset. Are
    there implicit missing values? What’s the difference between an NA
    and zero?

There are plenty of missing values in this data set because not every
single strain of disease would’ve been present every year. That said,
using `values_drop_na` would make the analysis a lot easier to do and
because we convert the values to be in long-form anyway not much of
value should be lost

-   What happens if you neglect the `mutate()` step?
    `(mutate(names_from = stringr::str_replace(key, "newrel", "new_rel")))`

Because some of the values are called ‘newrel’ instead of ‘new\_rel’ it
keeps them as separate, leading to 2 variables being in the ‘new’
column. ‘new’ and ‘newrel’

-   I claimed that iso2 and iso3 were redundant with country. Confirm
    this claim.

As it states in the `who` documentation, iso2 and iso3 are just the 2 &
3 letter ISO country codes. As we already have the country name, it’s
not important in this case.

-   For each country, year, and sex compute the total number of cases of
    TB. Make an informative visualisation of the data.

<!-- -->

    who5 <- who %>%
      pivot_longer(
        cols = new_sp_m014:newrel_f65,
        names_to = "key",
        values_to = "cases",
        values_drop_na = TRUE
      ) %>%
      mutate(
        key = str_replace(key, "newrel", "new_rel")
      ) %>%
      separate(key, c("new", "var", "sexage")) %>%
      separate(sexage, c("sex", "age"), sep = 1) %>%
      select(-iso2, -iso3, -new, -age)

    who5 %>%
      group_by(country, year, sex) %>% 
      filter(year > 1995) %>%
      summarise(cases = sum(cases)) %>%
      unite(country_sex, country, sex, remove = FALSE) %>%
      ggplot(aes(x = year, 
                 y = cases,
                 group = country_sex,
                 colour = sex)) +
      geom_line()

13 Relational Data
------------------

Relational data is what it’s called when you use more than a single
table of data, which is usually the case with more complicated analysis.
It refers to the relations between the tables. There are 3 families of
verbs to work with relational data:

-   **Mutating** joins, adds new variables into one data frame from
    matching observations in another

-   **Filtering** joins which filter observations from one data frame
    based on whether or not they match observation in the other table

-   **Set** operations which treat observations as if they were set
    elements

The `nycflights13` dataset is an example of the combination of many
different tables. The relationship between these tables can be seen
below:

![](https://d33wubrfki0l68.cloudfront.net/245292d1ea724f6c3fd8a92063dcd7bfb9758d02/5751b/diagrams/relational-nycflights.png)

##### Exercises

-   Imagine you wanted to draw (approximately) the route each plane
    flies from its origin to its destination. What variables would you
    need? What tables would you need to combine?

We would need the following tables: `flights`, `planes.` Only variables
needed are: origin, dest, tailnum, date info (if that’s only way to
distinguish between flights)

-   I forgot to draw the relationship between `weather` and `airports.`
    What is the relationship and how should it appear in the diagram?

`Weather` and `airports` are connected through the `flights` table
(origin in `weather` and `flights`)

-   `weather` only contains information for the origin (NYC) airports.
    If it contained weather records for all airports in the USA, what
    additional relation would it define with flights?

If it contained all of the airport we could also relate it to
destination

-   We know that some days of the year are “special”, and fewer people
    than usual fly on them. How might you represent that data as a data
    frame? What would be the primary keys of that table? How would it
    connect to the existing tables?

We could create a table called `special_dates` like the below and use
the date keys to join it onto other tables:

    special_days <- tribble(
      ~year, ~month, ~day, ~holiday,
      2013, 01, 01, "New Years Day",
      2013, 07, 04, "Independence Day",
      2013, 11, 29, "Thanksgiving Day"
    )

### 13.3 Keys

The variables we use to connect tables are called **keys**. There are
two types:

-   **Primary** which uniquely identifies an observation in its own
    table. An example would be tailnum in `planes` which uniquely
    identifies each plane

-   **Foreign** which uniquely identifies an observation in another
    table

It’s good practice to verify that the primary keys you’ve identified do,
in fact, match unique observations. We can use `count` for that

    planes %>%
      count(tailnum) %>%
      filter(n > 1)

Unfortunately not all tables have a primary key. If it lacks one, it’s
sometimes helpful to add one using `mutate` and `row_numer` which makes
it easier to match observations when we’ve done filtering. This is
called a **surrogate key**.

##### Exercises

-   Add a surrogate key to flights.

<!-- -->

    flights %>%
      mutate(unique_id = row_number()) 

-   Identify the keys in the following datasets

1.  Lahman::Batting,
2.  babynames::babynames
3.  nasaweather::atmos
4.  fueleconomy::vehicles
5.  ggplot2::diamonds

(You might need to install some packages and read some documentation.)

    diamonds %>%
      distinct() %>%
      nrow()

The primary key for `Batting` is (`playerID`, `yearID`, `stint`)

For `babynames` it is (`year`, `sex`, `name`)

For `atmos` we can use (`lat`, `long`, `year`, `month`)

`vehicles` is nice and easy, we just use `id`

There is no primary key in `diamonds` because there was duplicate
values, can calculate this using `distinct` and pipe into `nrow()`

### 13.4 Mutating Joins

Just like with mutate, join functions add variables to the right. We can
use a `left_join` to join two sets together:

    flights2 <- flights %>% 
      select(year:day, hour, origin, dest, tailnum, carrier)

    flights2 %>%
      select(-origin, -dest) %>%
      left_join(airlines, by = "carrier")

Joins are similar to how they’re used in SQL. Some interesting things to
remember though:

-   You can join using a named character vector: `by = c("a" = "b")`
    will match var `a` in table `x` to var `b` in table `y`

##### Exercises

-   Compute the average delay by destination, then join on the airports
    data frame so you can show the spatial distribution of delays.
    Here’s an easy way to draw a map of the United States:

<!-- -->

    airports %>%
      semi_join(flights, c("faa" = "dest")) %>%
      ggplot(aes(lon, lat)) +
        borders("state") +
        geom_point() +
        coord_quickmap()

    flights %>%
      group_by(dest) %>%
      summarise(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
      inner_join(airports, by = c(dest = "faa")) %>%
      ggplot(aes(lon, lat, colour = avg_arr_delay)) +
      borders("state") +
      geom_point() + 
      coord_quickmap()

-   Add the location of the origin and destination (i.e. the lat and
    lon) to flights.

<!-- -->

    airport_locations <- airports %>%
      select(faa, lat, lon)

    flights %>%
      select(year:day, tailnum, origin, dest) %>%
      left_join(
        airport_locations, 
        by = c("origin" = "faa")
        ) %>%
      left_join(
        airport_locations,
        by = c("dest" = "faa")
      )

-   Is there a relationship between the age of a plane and its delays?

<!-- -->

    plane_info <- planes %>%
      select(tailnum, year, model) %>%
      rename(manu_year = year)


    plane_cohorts <- flights %>%
      select(year:day, dep_delay, arr_delay, carrier, tailnum, origin, dest) %>%
      inner_join(
        plane_info,
        by = "tailnum"
      ) %>%
      mutate(age_of_plane = year - manu_year) %>%
      filter(!is.na(age_of_plane)) %>%
      mutate(age_of_plane = if_else(age_of_plane > 25, 25L, age_of_plane)) %>% 
      group_by(age_of_plane) %>%
      summarise(
        dep_delay_mean = mean(dep_delay, na.rm = TRUE),
        dep_delay_sd = sd(dep_delay, na.rm = TRUE),
        arr_delay_mean = mean(arr_delay, na.rm = TRUE),
        arr_delay_sd = sd(arr_delay, na.rm = TRUE),
        n_arr_delay = sum(!is.na(arr_delay)),
        n_dep_delay = sum(!is.na(dep_delay))
      )

    ggplot(plane_cohorts,
           aes(x = age_of_plane,
               y = dep_delay_mean)) +
      geom_point() +
      scale_x_continuous("Age of plane (years)", breaks = seq(0, 30, by = 10)) +
      scale_y_continuous("Mean Departure Delay (minutes)")

It seems that up until the age of 15 departure delay increases yearly
but then it levels off. Surprisingly, the older the plane, the less
departure delay there is (after 15 years)

-   What weather conditions make it more likely to see a delay?

<!-- -->

    flight_weather <-
      flights %>%
      inner_join(weather, by = c(
        "origin" = "origin",
        "year" = "year",
        "month" = "month",
        "day" = "day",
        "hour" = "hour"
      ))

    flight_weather %>%
      group_by(precip) %>%
      summarise(delay = mean(dep_delay, na.rm = TRUE)) %>%
      ggplot(aes(x = precip, y = delay)) +
      geom_line() + geom_point()

Main cause for delays is any increase in precip

-   What happened on June 13 2013? Display the spatial pattern of
    delays, and then use Google to cross-reference with the weather.

<!-- -->

    flights %>%
      filter(month == 6,
             day == 13) %>% 
      select(year:day, hour, dep_delay, arr_delay, flight, tailnum, origin, dest) %>%
      left_join(
        weather,
        by = c("year" = "year",
               "month" = "month",
               "day" = "day",
               "hour" = "hour",
               "origin" = "origin")
      ) %>% View()

On 13/6/2013 the USA saw 2 derechos - long-lived, straight-line wind
storms, that caused massive disruption to flights. Can read more about
it
[here](https://en.wikipedia.org/wiki/June_12%E2%80%9313,_2013_derecho_series)

#### 13.4.7 Other implementations

We can also use `merge` instead of any of the joins but joins are
preferred as they more clearly convey intent of code. A `merge` example
for a `left_join` would be `merge(x, y, all.x = TRUE)`

### 13.5 Filtering Joins

Filtering joins match observations in the same way as mutating joins but
affect the observations instead of the variables. We can use two
variations:

-   `semi_join(x, y)` keeps all observations in `x` that have a match in
    `y`

-   `anti_join(x, y)` drops all observations in `x` that have a match in
    `y`

semi\_joins are useful for matching filtered summary tables back to
original rows

    top_flights <- flights %>%
      count(dest, sort = TRUE) %>%
      head(10) 

    flights %>%
      semi_join(top_flights)

Anti-joins on the other hand are helpful for diagnosing join mismatches,
e.g. when connecting flights and planes we can see there are many
flights that don’t have a match in planes

    flights %>%
      anti_join(planes, by = "tailnum") %>%
      count(tailnum)

###### Exercises

-   What does it mean for a flight to have a missing tailnum? What do
    the tail numbers that don’t have a matching record in planes have in
    common? (Hint: one variable explains ~90% of the problems.)

Flights that have a missing `tailnum` all have a missing `arr_time` too,
meaning the flights were cancelled.

-   Filter flights to only show flights with planes that have flown at
    least 100 flights.

<!-- -->

    flights_fte_100 <- flights %>%
      filter(!is.na(tailnum)) %>%
      group_by(tailnum) %>%
      count() %>%
      filter(n >= 100) 
      
    flights %>%
      semi_join(flights_fte_100,
                by = "tailnum")

-   Combine fueleconomy::vehicles and fueleconomy::common to find only
    the records for the most common models.

<!-- -->

    fueleconomy::vehicles %>%
      semi_join(fueleconomy::common, by = c("make", "model"))

-   Find the 48 hours (over the course of the whole year) that have the
    worst delays. Cross-reference it with the weather data. Can you see
    any patterns?

<!-- -->

    worst_hours <- flights %>%
      mutate(hour = sched_dep_time %/% 100) %>% 
      group_by(origin, year, month, day, hour) %>%
      summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(desc(dep_delay)) %>%
      slice(1:48)

    weather_most_delayed <- semi_join(weather, worst_hours,
              by = c("origin", "year",
                     "month", "day", "hour")
              )

    select(weather_most_delayed, temp, wind_speed, precip) %>%
      print(n = 48)

    ggplot(weather_most_delayed, 
           aes(x = precip,
               y = wind_speed,
               color = temp)) +
      geom_point()

-   What does `anti_join(flights, airports, by = c("dest" = "faa"))`
    tell you? What does
    `anti_join(airports, flights, by = c("faa" = "dest"))` tell you?

The expression `anti_join(flights, airports, by = c("dest" = "faa"))`
returns the flights that went to an airport that is not in the FAA list
of destinations. Since the FAA list only contains domestic airports,
these are likely foreign flights.

The expression `anti_join(airports, flights, by = c("faa" = "dest"))`
returns the US airports that were not the destination of any flight in
the data. Since the data contains all flights from New York City
airports, this is also the list of US airports that did not have a
nonstop flight from New York City in 2013.

-   You might expect that there’s an implicit relationship between plane
    and airline, because each plane is flown by a single airline.
    Confirm or reject this hypothesis using the tools you’ve learned
    above.

Planes can be flown by multiple airlines. For example, some airlines go
bust and then go on to sell their planes. As a result we can’t trust
there to be an implicit relationship between the two as for many, that
is simply not the case.

#### 13.6 Join Problems

When working with own data it’s very unlikely it’s already been tidied
up. It’s important to keep a few things in mind to make it as easy as
possible to perform joins:

-   Identify variables that form the primary key in each table. Should
    do this based on understanding of the data, not just by looking at
    combinations of variables that might give an idea

-   Check that none of the variables in the primary key are missing. If
    value is missing, it can’t identify an observation

-   Check that foreign keys match primary keys in another table. Best
    way to do this is to use an `anti_join`. Common for keys not to
    match because of data entry problems.

#### 13.7 Set Operations

These aren’t used as often but are helpful when breaking up a complex
filter into simpler pieces. All these operations work with a complete
row, comparing the value of every variable. We have:

-   `intercept(x, y)` returns only observations in both `x` and `y`

-   `union(x, y)` returns unique observations in `x` and `y`

-   `setdiff(x, y)` returns observations in `x`, but not in `y`

<!-- -->

    df1 <- tribble(
      ~x, ~y,
       1,  1,
       2,  1
    )
    df2 <- tribble(
      ~x, ~y,
       1,  1,
       1,  2
    )

    intersect(df1, df2)
    union(df1, df2)
    setdiff(df2, df1)

14 Strings
----------

This section is mainly focused on Regular Expressions when it comes to
analysing strings and manipulating filters.

There are several commands that will be useful when working with
strings. Some important ones are:

-   `str_length()` which takes an input (can be a list) and outputs the
    length

-   `str_c()` which allows you to combine strings together. You can use
    the `sep` argument to control how they are to be separated. Can also
    use `collapse` argument if we want to collapse a vector of strings.

-   `str_replace_na()` allows you to print errors as NA

<!-- -->

    str_c("What's going", "awn", sep = ",")

    x <- c("abc", NA)
    x
    str_c("|-", x, "-|") # here NA is ignored and it causes an error. If we want NA to be included as a string

    str_c("|-", str_replace_na(x), "-|")

    paste(x)

Objects of length 0 are silently dropped. We can use this to our
advantage with an IF statement

    name <- "Duncan"
    time_of_day <- "morning"
    birthday <- TRUE

    str_c("Good ", time_of_day, " ", name,
          if (birthday) " and HAPPY BIRTHDAY",
          ".")

    ## [1] "Good morning Duncan and HAPPY BIRTHDAY."

#### 14.2.3 Subsetting strings

We can also take parts of a string by subsetting it with a parameter.
For example, if we just want the first 4 digits of everything in a list.
The command for this is `str_sub(x, start, end)` and can be great
sometimes to use with `str_to_upper` or lower

    x <- c("Apple", "Banana", "Pear")

    str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))

#### 14.2.4 Locales

Because different languages have different rules for capitalisation, R
offers locales that you can change easily by specifying the `locale`
parameter in `str_to_upper` / `str_to_lower`

###### Exercises

-   In code that doesn’t use stringr, you’ll often see paste() and
    paste0(). What’s the difference between the two functions? What
    stringr function are they equivalent to? How do the functions differ
    in their handling of NA?

You use paste0 when you want to have to sep parameter pre-defined (as
there’s no way to alter it). `paste0` is equivalent to using
`paste(x, x, sep="")`. `paste` coerces the NA character to “NA” which
can be undesirable. In that way, it’s similar to using `str_c` in
conjunction with `str_replace_na`

-   In your own words, describe the difference between the sep and
    collapse arguments to str\_c().

The `sep` argument still divides the given input into separate lists or
vectors. The collapse argument on the other hand puts it all in a single
variable

    x <- c("a", "b", "c", "d")
    y <- c("w", "x", "y", "z")

    str_c(x, collapse = " || ")

-   Use str\_length() and str\_sub() to extract the middle character
    from a string. What will you do if the string has an even number of
    characters?

<!-- -->

    c <- "Middlevaluee"

    str_sub(c, round(str_length(c) / 2, 0), round(str_length(c) / 2, 0))

    x <- c("a", "abc", "abcd", "abcde", "abcdef")
    L <- str_length(x)
    m <- ceiling(L / 2)
    str_sub(x, m, m)

-   What does str\_wrap() do? When might you want to use it?

`str_wrap` automatically wraps paragraphs in a way that makes them more
readable. Useful sometimes when you have very long sentences.

-   What does str\_trim() do? What’s the opposite of str\_trim()?

`str_trim` allows us to remove whitespace from start and end of string.
You can use different parameters using `side` for example,
`side = c("both")`. You could also use `str_squish` which reduces
repeated whitespace inside a string. To do the opposite of trim we use
`str_pad`

    str_trim("  String with trailing and leading white space\t")
    str_trim("\n\nString with trailing and leading white space\n\n")

    str_squish("  String with trailing,  middle, and leading white space\t")

    str_pad(c("a", "abc", "abcdef"), 10)

-   Write a function that turns (e.g.) a vector c(“a”, “b”, “c”) into
    the string a, b, and c. Think carefully about what it should do if
    given a vector of length 0, 1, or 2.

COME BACK TO THIS AFTER FUNCTION CHAPTER
========================================

### 14.3 Matching patterns with regular expressions

For regular expressions we’ll use `str_view` and `str_view_all`, both of
which take a character vector and a regex. We can also use `str_subset`
which keeps string that match a pattern

    x <- c("Apple", "Banana", "Pear")

    str_view(x, ".a.")
    str_view(c("abc", "a.c", "bef"), "a\\.c")

**RegEx Rules** contain:

-   `.` matches any character (except new line)

-   `^` to match start of string

-   `$` to match end of string

-   `\d` matches any digit

-   `\s` matches any whitespace

-   `\w` matches any word

-   `[abc]` matches a, b, or c

-   \`\[^abc\] matches anything but a, b, or c

-   `|` is the alternation or the OR character

-   `?` 0 or 1 matches

-   `+` 1 or more matches

-   `*` 0 or more matches

-   `{n}` number of matches - you can do `{2}`, `{2,}` (2 or more),
    `{, 2}` (at most 2), `{2, 4}` between 2 and 4

-   `\\1` `\\2` etc match the first or second expression you’ve put it
    (see backreferences)

It’s very important to note that if you want to search for a character
like a full stop and circumvent the regex rule, you need to use two
backslashes and escape instead of just one. So `"\\."`. The most
confusing part of this is if you want to search for a backslash, you
need to do four backslashes to escape the one.

##### Exercises

-   Explain why each of these strings don’t match a : “",”\\“,”\\".

One backslash is looking for an escaped character, two is looking for a
regular expression, three is looking for a regular expression but needs
another backslash to be escaped.

-   How would you match the sequence "’?

We would use `str_view(h, "\"\'\\\\")`

-   What patterns will the regular expression ...... match? How would
    you represent it as a string?

This would be `.`\[x\]`.`\[x\]`.`\[x\]

#### 14.3.2 Anchors

Anchors are defined as the `^` and `$` characters. We can also use the
boundary between words with `\b` for example, `\bsum\b` will only match
sum, not summarise or cumsum

##### Exercises

-   How would you match the literal string “$^$”?

We would have to use `str_view("$^$", "\\$\\^\\$")`

-   Given the corpus of common words in stringr::words, create regular
    expressions that find all words that:

1.  Start with “y”.

2.  End with “x”

3.  Are exactly three letters long. (Don’t cheat by using
    str\_length()!)

4.  Have seven letters or more.

Since this list is long, you might want to use the match argument to
str\_view() to show only the matching or non-matching words.

    str_view(words, "^y", match = TRUE)
    str_view(words, "x$", match = TRUE)
    str_view(words, "^...$", match = TRUE)
    str_view(words, "^.......", match = TRUE)

#### 14.3.3 Character classes and alternatives

These are special patterns that match more than one character, `.` is
one of them and things like `\d` are examples of others (see list
above). A character class containing a single character can be a nice
alternative to using backslashes as it’s more readable:

    str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")

This works for most regex metacharacters but not all of them, you can’t
use them for `]` `\` `^` `-`

    str_view(c("grey", "gray"), "gr(e|a)y")

##### Exercises

-   Create regular expressions to find all words that:

1.  Start with a vowel.

2.  That only contain consonants. (Hint: thinking about matching
    “not”-vowels.)

3.  End with ed, but not with eed.

4.  End with ing or ise.

<!-- -->

    str_view_all(words, "^(a|e|i|o|u)", match = TRUE)
    str_view_all(words, "^[^aeiou]+$", match = TRUE)
    str_view_all(words, "[^e]ed$", match = TRUE)
    str_view_all(words, "(ing|ise)$", match = TRUE)

-   Empirically verify the rule “i before e except after c”.

<!-- -->

    length(str_subset(words, "(cei|[^c]ie)"))
    length(str_subset(words, "(cie|[^c]ei)"))

-   Is “q” always followed by a “u”?

It is in this dataset, yes.

    str_subset(words, "q[^u]")

-   Write a regular expression that matches a word if it’s probably
    written in British English, not American English.

Requires overgeneralising unless we’re using a dictionary to check. That
said, we’d use “ou” instead of “o”, “ae” and “oe” instead of “a” and
“o”, ends in “ise” instead of “ize” and ends in “yse”

    str_subset(words, "ou|ae|oe|ise$|yse$")

#### 14.3.4 Repetition

We can also control the amount of times the character appears with `?`,
`+` and `*`

    x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"

    str_view(x, "CC+")
    str_view(x, "C[LX]+")

This is especially handy when we’re doing things like matching both
American and British spelling

    str_view(c("color", "colour"), "colou?r")

To control amount of times it appears more directly, use the square
brackets

    str_view(x, "C{2,}")

By default these matches are “greedy” meaning they’ll match the longest
string possible. If instead we want them to match the shortest string
possible, we can put a question mark after the statement.

    str_view(x, "C{2,3}?")
    str_view(x, "C[LX]+?")

##### Exercises

-   Describe the equivalents of ?, +, \* in {m,n} form.

The equivalent of `?` would be `{1}`, equivalent of `+` is `{1,}`,
equivalent of `*` is `{0,}`

-   Describe in words what these regular expressions match: (read
    carefully to see if I’m using a regular expression or a string that
    defines a regular expression.)

1.  `^.*$`

2.  `"\\{.+\\}"`

3.  `\d{4}-\d{2}-\d{2}`

The *first* one will match any string as the combination of `.` and `*`
means any character for any amount of length. The *second* one will
match anything with curly bracers around at least one character. The
*third* will match 4 digits with a hypen, 2 digits with a hyphen, 2
digits. In other words will match data format.

-   Create regular expressions to find all words that:

1.  Start with three consonants.

2.  Have three or more vowels in a row.

3.  Have two or more vowel-consonant pairs in a row.

<!-- -->

    str_view(words, "^[^aeuio]{3}", match = TRUE)
    str_view(words, "[aeuio]{3,}", match = TRUE)
    str_view(words, "([aeuio][^aeuio]){2,}", match = TRUE)

-   Solve the beginner regexp crosswords at
    <a href="https://regexcrossword.com/challenges/beginner" class="uri">https://regexcrossword.com/challenges/beginner</a>.

#### 14.3.5 Grouping and back references

Parantheses aren’t just a way to make complex expressions easier to
read, we can also use them to create a numbered capturing group

    str_view(fruit, "(..)\\1", match = TRUE)

In this example it starts with a regex declaration of 2 characters and
then the `\\1` denotes that if it repeats, show the words so things like
b**anan**a and **cucu**mber

##### Exercises

-   Describe, in words, what these expressions will match:

1.  (.)\\1\\1

2.  “(.)(.)\\2\\1”

3.  (..)\\1

4.  “(.).\\1.\\1”

5.  "(.)(.)(.).\*\\3\\2\\1"

Number one will match if there’s one character that repeats three times
in a row. Number two will match if a pair of characters is followed by
same pair but in reverse order. Number three will match if two
characters are repeated. Four will match a character followed by any
other character, original character, any other character, original
character again. Number five will match three characters followed by
zero or more characters of any kind followed by the same three
characters but in reverse order.

-   Construct regular expressions to match words that:

1.  Start and end with the same character.

2.  Contain a repeated pair of letters (e.g. “church” contains “ch”
    repeated twice.)

3.  Contain one letter repeated in at least three places (e.g. “eleven”
    contains three “e”s.)

<!-- -->

    str_subset(words, "^(.)((.*\\1$)|(\\1?$))")
    str_subset("church", "(..)(.*\\1)") # OR
    str_subset(words, "([A-Za-z][A-Za-z]).*\\1")
    str_subset(words, "(.).*\\1.*\\1")

### 14.4 Tools

Now that we’ve learnt some basic regular expressions.. this chapter we
will learn some stringr functions that will let us determine which
strings match a pattern, find positions of matches, extract content of
matches, replace matches with new values, split string based on match

First up is `str_detect` which will return a logical vector the same
length as input (so if we have a list of 3, it will return a list of 3
TRUE/FALSE depending whether the regex matches). This is great
inconjunction with `sum` so we can see how many things match our regex
in a dataset

    sum(str_detect(words, "^t"))

    # how many common words end up with a vowel?
    mean(str_detect(words, "[aeuio]$"))

When we’re dealing with complex logical conditions it’s sometimes easier
to combine multiple `str_detect` rather than trying to fit it all into
one. Another important function to remember is `str_subset` which
returns only the elements that match a pattern however, most of the time
we’ll be using dataframes so we’ll work with `filter` and `str_detect`
instead. There’s also a variation of `str_detect` which is `str_count`,
this simply tells us how many matches there are of a regex in a string.

    df <- tibble(
      word = words, 
      i = seq_along(word)
    )

    df %>%
      filter(str_detect(word, "x$"))

`str_count` is commonly used in conjunction with `mutate`:

    df %>%
      mutate(
        vowels = str_count(word, "aeiou"),
        consonants = str_count(word, "[^aeiou]")
      )

##### Exercises

-   For each of the following challenges, try solving it by using both a
    single regular expression, and a combination of multiple
    str\_detect() calls.

1.  Find all words that start or end with x.

2.  Find all words that start with a vowel and end with a consonant.

3.  Are there any words that contain at least one of each different
    vowel?

<!-- -->

    words[str_detect(words, "($x|x$)")]
    words[str_detect(words, "^[aeuio].*[^aeuio]$")]
    words[str_detect(words, "a") &
            str_detect(words, "e") &
            str_detect(words, "u") &
            str_detect(words, "i") &
            str_detect(words, "o")]

-   What word has the highest number of vowels? What word has the
    highest proportion of vowels? (Hint: what is the denominator?)

Highest proportion of vowels is shared by a view by area and idea are at
the top. In terms of highest number, appropriate is the winner!

    df %>%
      mutate(vowels = str_count(word, "[aeiou]"),
             word_length = str_length(word),
             vowel_prop = vowels / word_length * 100) %>%
      arrange(desc(vowels))

##### Extract matches

If we want to extract the actual text of a match, we can use
`str_extract`. It’s important to note that this function only extracts
the first match. If we want to get all matches we need to use
\`str\_extract\_all

    colours <- c("red", "orange", "yellow", "green", "blue")
    colour_match <- str_c(colours, collapse = "|")

    str_extract(str_subset(sentences, colour_match), colour_match)

    str_view_all(sentences[str_count(sentences, colour_match) > 1], str_c("\\b", colour_match, "\\b"))

##### Exercises

-   In the previous example, you might have noticed that the regular
    expression matched “flickered”, which is not a colour. Modify the
    regex to fix the problem.

We can use the following to avoid it including flickered,
`str_view_all(sentences[str_count(sentences, colour_match) > 1], str_c("\\b", colour_match, "\\b"))`

-   From the Harvard sentences data, extract:

1.  The first word from each sentence.

2.  All words ending in ing.

3.  All plurals.

<!-- -->

    str_extract(sentences, "[A-Za-z]+")
    str_extract(sentences, "\\b[A-Za-z]+ing\\b")
    str_extract(sentences, "\\b[A-Za-z]{3,}s\\b")

#### 14.4.3 Grouped matches

Where `str_extract` gives us the complete match, `str_match` gives us
each individual component

    str_extract(sentences, "(a|the) ([^ ]+)")
    str_match(sentences, "(a|the) ([^ ]+)")

When working with a tibble however, it might be using to simply use
extract which forces you to name the new columns. The syntax is like
this:

    tibble(sentence = sentences) %>%
      extract(
        sentence, c("article", "noun"), regex = "(a|the) ([^ ]+)",
        remove = FALSE)


    ?extract

##### Exercises

-   Find all words that come after a “number” like “one”, “two”, “three”
    etc. Pull out both the number and the word.

<!-- -->

    numword <- "\\b(one|two|three|four|five|six|seven|eight|nine|ten) \\w+"

    sentences[str_detect(sentences, numword)] %>%
      str_extract(numword)

    ##  [1] "seven books"   "two met"       "two factors"   "three lists"  
    ##  [5] "seven is"      "two when"      "ten inches"    "one war"      
    ##  [9] "one button"    "six minutes"   "ten years"     "two shares"   
    ## [13] "two distinct"  "five cents"    "two pins"      "five robins"  
    ## [17] "four kinds"    "three story"   "three inches"  "six comes"    
    ## [21] "three batches" "two leaves"

-   Find all contractions. Separate out the pieces before and after the
    apostrophe.

<!-- -->

    sentences[str_detect(sentences, "([A-Za-z]+)'([A-Za-z]+)")] %>%
      str_extract("([A-Za-z]+)'([A-Za-z]+)") %>%
      str_split("'")

#### 14.4.4 Replacing matches

To replace matches with new strings we can use `str_replace` and
`str_replace_all`. This can be helpful with phone numbers or if you need
to add in punctuation. There are a few ways of doing it, you can replace
a pattern with a fixed string or you can use `str_replace_all` to
perform multiple replacements using a named vector. You can also flip
words this way

    x <- c("1 house", "2 cars", "3 people")
    str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three")) # quick word replace

    sentences %>%
      str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>%
      head(5)

##### Exercises

-   Replace all forward slashes in a string with backslashes.

<!-- -->

    backslash_string <- "Hello there / all ya / cool cats / and kittens /"
    str_replace_all(backslash_string, "\\/", "\\\\")

-   Implement a simple version of str\_to\_lower() using replace\_all().

<!-- -->

    replacements <- c(
      "A" = "a", "B" = "b", "C" = "c", "D" = "d", "E" = "e",
      "F" = "f", "G" = "g", "H" = "h", "I" = "i", "J" = "j",
      "K" = "k", "L" = "l", "M" = "m", "N" = "n", "O" = "o",
      "P" = "p", "Q" = "q", "R" = "r", "S" = "s", "T" = "t",
      "U" = "u", "V" = "v", "W" = "w", "X" = "x", "Y" = "y",
      "Z" = "z"
    )
    str_replace_all(words, pattern = replacements)

-   Switch the first and last letters in words. Which of those strings
    are still words?

<!-- -->

    words %>%
      str_replace_all("^([A-Za-z])(.*)([a-z])$", "\\3\\2\\1")

#### 14.4.5 Splitting

Lastly we can use splitting with `str_split`. Again, we can use the
`simplify = TRUE` parameter to return a matrix. There’s also an `n`
parameter which allows you to request a maximum number of pieces. One
good tip for using it is it makes it easy to split up a sentence by
using the `boundary` function

    sentences %>%
      str_split(boundary("word")) %>%
      head(5)

-   Split up a string like “apples, pears, and bananas” into individual
    components.

<!-- -->

    str_split(c("apples, pears, and bananas"), ", +(and +)?")

-   Why is it better to split up by boundary(“word”) than " "?

It’s a more sophisticated method to split into words - it recognises
non-space punctuation and also removes it

-   What does splitting with an empty string ("") do? Experiment, and
    then read the documentation.

If we leave it empty it’s equivalent to the \`boundary(“character)” so
it splits by letter

    sentences %>%
      str_split("") %>%
      head(3)

#### 14.4.6 Find matches

When we want to find the starting and ending positions of each match we
use `str_locate` or `str_locate_all`. These are useful when none of the
other functions do what we want so we can use `str_locate` to find the
matching pattern and then `str_sub` to extract / modify them

### 14.5 Other Types of pattern

When you use a pattern that’s a string, it’s automatically wrapped into
a regex. So `str_view(fruit, "nana")` is the same as saying
`str_view(fruit, regex("nana"))`. If we want to control details of the
match we’ll have to type regex out. We can use:

-   `ignore_case` so it becomes
    `str_view(fruit, regex("nana", ignore_case = TRUE))`

-   `multiline = TRUE` allows `^` and `$` to match start and end of each
    line rather than start and end of each string

-   `comments = TRUE` allows us to make comments and white space to make
    regex more understandable. An example:

<!-- -->

    phone <- regex("
      \\(?     # optional opening parens
      (\\d{3}) # area code
      [) -]?   # optional closing parens, space, or dash
      (\\d{3}) # another three numbers
      [ -]?    # optional space or dash
      (\\d{3}) # three more numbers
      ", comments = TRUE)

-   `dotall = TRUE` allows `.` to match everything including `\n`

Instead of `regex` we can use three other functions:

-   `fixed` matches exactly specified number of bytes

-   `coll` compares strings using standard collation rules. Useful for
    doing case insensitive matching, it takes a `locale` parameter

-   `boundary` as demonstrated above

##### Exercises

-   How would you find all strings containing `\` with `regex()`
    vs. with `fixed()`?

With `regex` we’d have to use `\\\\` with `fixed` we’d simply do `"\"`

    str_extract_all(backslash_string, fixed("/"))

-   What are the five most common words in sentences?

<!-- -->

    tibble(word = unlist(str_extract_all(sentences, boundary("word")))) %>%
      mutate(word = str_to_lower(word)) %>% 
      count(word, sort = TRUE)

### 14.6 Other uses of regular expressions

There are two useful functions in Base R that are built on Regex as
well, `apropos` and `dir`. `apropos` searches all objects available in
the global environment which is useful when you can’t remember the name
of a function. `dir` lists all the files in a directory. It takes a
`pattern` parameter like `head(dir(pattern = "\\.Rmd$"))`

### 14.7 Stringi

Stringr is built on the **stringi** package, Stringr is great to begin
with because it has a minimal set of functions that have been selected
because they handle the most common user problems. Stringi, on the other
hand, is very comprehensive and contains every function you could ever
need.

##### Exercises

-   Find the stringi functions that:

1.  Count the number of words.

2.  Find duplicated strings.

3.  Generate random text.

To count the number of words we can use `stri_length`, `stri_unique`
will allow us to find duplicated strings, and `stri_rand_strings` will
generate random text

    stri_rand_strings

15 Factors
----------

We use factors to work with categorical variables - historically,
factors were easier to work with than characters and so many functions
in base R still convert characters to factors. To create factors there
are two steps, first we create a list and then we convert it to a
factor. Any values not in the set you’re converting will be silently
converted to NA - if we want a warning, we can use `parse_vector`.

    # Creating a list and ordering it by a factor list

    x1 <- c("Dec", "Apr", "Jan", "Mar")

    month_levels <- c(
      "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
    )

    y1 <- factor(x1, levels = month_levels)

    sort(y1)

If we want the order of the levels to match the order of first
appearance in the data we can do that when creating the factor by
setting levels with `unique` or after the fact with `fct_inorder`. We
can also access the set of valid levels directly with `levels`.

    f1 <- factor(x1, levels = unique(x1))

    x1 %>%
      factor() %>%
      fct_inorder() %>%
      levels()

##### Exercises

-   Explore the distribution of `rincome` (reported income). What makes
    the default bar chart hard to understand? How could you improve the
    plot?

<!-- -->

    ggplot(gss_cat, 
           aes(x = rincome)) +
      geom_bar() +
      coord_flip()

    gss_cat %>%
      filter(!rincome %in% c("Not applicable")) %>%
      mutate(rincome = fct_recode(rincome,
                                  "Less than 1000" = "Lt $1000")) %>%
      mutate(rincome_na = rincome %in% c("Refused", "Don't know", "No answer")) %>%
      ggplot(aes(x = fct_rev(fct_infreq(rincome)),
                 fill = rincome_na)) +
      geom_bar() +
      coord_flip() +
      scale_y_continuous("Number of respondents", labels = scales::comma) +
      scale_x_discrete("Respondent's income") +
      scale_fill_manual(values = c("FALSE" = "black", "TRUE" = "gray")) +
      theme(legend.position = "None")

First of all, the default bar chart is illegible until we use
`coord_flip()` because there are too many different options. Secondly,
because it’s not ordered correctly it is hard to get any insights from
it

-   What is the most common `relig` in this survey? What’s the most
    common `partyid`?

By far the most common religion in the survey is protestant with 50% of
the share. Second place is Catholic with 24% of the share. In terms of
partyid, most of the survey respondents are Independent (19%) with
second place being Not str democrat (17%) and Strong democrat not far
behind (16%)

-   Which relig does denom (denomination) apply to? How can you find out
    with a table? How can you find out with a visualisation?

In terms of a table, if we get rid of all the “non-answers” like not
applicable, no answer, don’t know, it becomes very clear it only matters
to Protestants.

    gss_cat %>%
      filter(!denom %in% c("Not applicable", "No answer", "No denomination", "Don't know")) %>%
      count(relig, denom) %>%
      arrange(desc(n)) 

We can also use a scatterplot to make it clear if we want to do it
visually

    gss_cat %>%
      count(relig, denom) %>%
      ggplot(aes(x = relig,
                 y = denom,
                 size = n)) +
      geom_point() +
      theme(axis.text.x = element_text(angle = 90, size = 6))

### 15.4 Modifying factor order

One of the main things we use factors for is to reorder levels in a
visualisation. For example, the bar plot above can be confusing as it’s
not ordered in any obvious way so people can’t quickly look at it and
come to a conclusion. To do this we can use `fct_reorder` - this
function takes three arguments: `f` the factor you want to modify, `x`
numeric vector we want to use to reorder, `fun` which is optional can be
used when there are multiple values of `x` for each value of `f`

If we do this for age in the GSS for example:

    gss_cat %>%
      group_by(rincome) %>%
      summarise(
        age = mean(age, na.rm = TRUE),
        n = n()
      ) %>%
      mutate(rincome = fct_reorder(rincome, age)) %>%
      ggplot(aes(x = age,
                 y = rincome)) +
      geom_point()

In the above it doesn’t quite make sense to order by number of
respondents for each number as it was already ordered by income. Now the
y-axis has become confusing. If we just want to pull one element to the
front (such as “Not applicable”) we can use `fct_relevel` which takes a
factor and then any number of levels you want to move to front of the
line

    gss_cat %>%
      group_by(rincome) %>%
      summarise(
        age = mean(age, na.rm = TRUE),
        n = n()
      ) %>%
      mutate(rincome = fct_relevel(rincome, "Not applicable")) %>%
      ggplot(aes(x = age,
                 y = rincome)) +
      geom_point()

Another type of reordering that is especially useful when we’re
colouring lines on a plot is `fct_reorder2` which reorders a factor by
the y values associated with the highest x values. Makes a plot easier
to read because line colours line up with legend

    by_age <- gss_cat %>%
      filter(!is.na(age)) %>%
      count(age, marital) %>%
      group_by(age) %>%
      mutate(prop = n / sum(n))

    ggplot(by_age, 
           aes(x = age,
               y = prop,
               colour = marital)) +
      geom_line()

    ggplot(by_age, 
           aes(x = age,
               y = prop,
               colour = fct_reorder2(marital, age, prop))) +
      geom_line() +
      labs(colour = "marital")

Lastly there is `fct_infreq` which is useful for bar charts as it orders
levels in increasing frequency. Doesn’t require any additional variables
and is frequently used with `fct_rev` which reverses the order of factor
levels

##### Exercises

-   There are some suspiciously high numbers in tvhours. Is the mean a
    good summary?

Judging by the distribution I wouldn’t use mean. As you’d expect most
people only watch a few hours of tv (peak at 3 hours) and only very few
watch more than 4/5 hours. Would use median instead or even mode as 3/4
would be the best answer here.

    gss_cat %>%
      filter(!is.na (tvhours)) %>%
      ggplot(aes(x = tvhours)) +
      geom_histogram(binwidth = 1)

-   For each factor in gss\_cat identify whether the order of the levels
    is arbitrary or principled.

There are a total of six factors in gss\_cat. Of these, I would only say
rincome is principled. The rest are things like religion or marital
status which we can order arbitrarily.

-   Why did moving “Not applicable” to the front of the levels move it
    to the bottom of the plot?

Because being at the top gives “Not applicable” an integer level of 1,
meaning ggplot plots it first

### 15.5 Modifying factor levels

Even though changing the order is great especially when plotting, one of
the most powerful things we can do is changing the values. This allows
us to quickly change labels for publication and collapse levels for
high-level displays. The most general function for this is `fct_recode`

    gss_cat %>%
      mutate(partyid = fct_recode(partyid,
        "Republican, strong"    = "Strong republican",
        "Republican, weak"      = "Not str republican",
        "Independent, near rep" = "Ind,near rep",
        "Independent, near dem" = "Ind,near dem",
        "Democrat, weak"        = "Not str democrat",
        "Democrat, strong"      = "Strong democrat"
      )) %>%
      count(partyid)

`fct_recode` will leave levels that aren’t explicitly mentioned as they
are and will warn you if you mention a level that doesn’t exist. To
combine groups we can assign multiple old levels to the same new level

    gss_cat %>%
      mutate(partyid = fct_recode(partyid,
        "Republican, strong"    = "Strong republican",
        "Republican, weak"      = "Not str republican",
        "Independent, near rep" = "Ind,near rep",
        "Independent, near dem" = "Ind,near dem",
        "Democrat, weak"        = "Not str democrat",
        "Democrat, strong"      = "Strong democrat",
        "Other"                 = "No answer",
        "Other"                 = "Don't know",
        "Other"                 = "Other party"
      )) %>%
      count(partyid)

Whilst `fct_recode` is a great way of renaming things, if we want to
collapse stuff into a common group we can instead use `fct_collapse`

    gss_cat %>%
      mutate(partyid = fct_collapse(partyid,
        other = c("No answer", "Don't know", "Other party"),
        rep = c("Strong republican", "Not str republican"),
        ind = c("Ind,near rep", "Independent", "Ind,near dem"),
        dem = c("Not str democrat", "Strong democrat")
      )) %>%
      count(partyid)

If we want to leave it to a function to group together smaller groups we
can use `fct_lump` which just puts all the smaller groups in a data set
together. The default behaviour for `fct_lump` is to progressively lump
together smaller groups so that the aggregate is still the smaller
group. In this case we may have over collapsed so instead we can use the
`n` parameter to explicitly specify amount of groups we want to keep

    gss_cat %>%
      mutate(relig = fct_lump(relig, n = 10)) %>%
      count(relig)

##### Exercises

-   How have the proportions of people identifying as Democrat,
    Republican, and Independent changed over time?

<!-- -->

    gss_cat %>%
      mutate(partyid = fct_collapse(partyid,
        other = c("No answer", "Don't know", "Other party"),
        rep = c("Strong republican", "Not str republican"),
        ind = c("Ind,near rep", "Independent", "Ind,near dem"),
        dem = c("Not str democrat", "Strong democrat")
      )) %>%
      count(year, partyid) %>%
      ggplot(aes(x = year,
                 y = n,
                 colour = fct_reorder2(partyid, year, n))) +
      geom_line() +
      labs(colour = "partyid")

In terms of Independent the gap between it and the other 2 parties
seemed to close around 2007 and then is shooting up again most recently.
They all had a peak around 2006. Everything else has seemed steady but I
think it’s more to do with amount of voters in later years.

-   How could you collapse rincome into a small set of categories?

<!-- -->

    gss_cat %>%
      mutate(
        rincome =
          fct_collapse(
            rincome,
            `Unknown` = c("No answer", "Don't know", "Refused", "Not applicable"),
            `Lt $5000` = c("Lt $1000", str_c(
              "$", c("1000", "3000", "4000"),
              " to ", c("2999", "3999", "4999")
            )),
            `$5000 to 10000` = str_c(
              "$", c("5000", "6000", "7000", "8000"),
              " to ", c("5999", "6999", "7999", "9999")
            )
          )
      ) %>%
      ggplot(aes(x = rincome)) +
      geom_bar() +
      coord_flip()

16 Dates and Times
------------------

Working with dates is usually done with the package `lubridate` which
isn’t part of the tidyverse. Should try and avoid date-time where
possible because it’s infinitely more complicated due to timelines etc.
To get current date or date time you can use `today()` or `now()`. Other
ways we create dates:

-   `mdy()`

-   `ymd()`

-   `dmy()`

If we want to add time as well:

-   `ymd_hms`

-   `mdy_hm`

All of these also take a `tz` parameter which stands for timezone.
Instead of individual date creations, most often than not we’ll be
working with a dataset where we want to convert certain columns to date
and time. For nycflights13 for example, we can use `make_datetime`

    flights %>%
      select(year, month, day, hour, minute) %>%
      mutate(departure = make_datetime(year, month, day, hour, minute))

We can do the same with the other columns in `nycflights13`:

    make_datetime_100 <- function(year, month, day, time) {
      make_datetime(year, month, day, time %/% 100, time %% 100)
    }

    flights_dt <- flights %>%
      filter(!is.na(dep_time), !is.na(arr_time)) %>%
      mutate(
        dep_time = make_datetime_100(year, month, day, dep_time),
        arr_time = make_datetime_100(year, month, day, arr_time),
        sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
        sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
      ) %>%
      select(origin, dest, ends_with("delay"), ends_with("time"))

    flights_dt %>%
      ggplot(aes(x = dep_time)) +
      geom_freqpoly(binwidth = 86400) # 86400 s = 1 day

We can also do it in a single day

    flights_dt %>%
      filter(dep_time < ymd(20130102)) %>%
      ggplot(aes(x = dep_time)) +
      geom_freqpoly(binwidth = 600) # 600 s = 10 mins

#### 16.2.3 From other types

If we want to switch between a date-time and date, we can use
`as_datetime()` or `as_date()`

##### Exercises

-   What happens if you parse a string that contains invalid dates?

It returns it as an error (NA) and gives a warning message saying one of
the items failed to parse, but still processes the one date that was
input correctly.

-   What does the tzone argument to today() do? Why is it important?

This sets the timezone for today. As default it uses the system’s
timezone.

-   Use the appropriate lubridate function to parse each of the
    following dates:

<!-- -->

    d1 <- "January 1, 2010"
    d2 <- "2015-Mar-07"
    d3 <- "06-Jun-2017"
    d4 <- c("August 19 (2015)", "July 1 (2015)")
    d5 <- "12/30/14" # Dec 30, 2014

    mdy(d1)
    ymd(d2)
    dmy(d3)
    mdy(d4)
    mdy(d5)

### 16.3 Date-time components

We can also pull out individual parts of the date with different
accessor functions like `year`, `month`, `mday` (day of month), `yday`,
`wday`, `hour`, `minute`, `second`.

    datetime <- ymd_hms("2020-04-15 08:37:02")

    year(datetime)
    month(datetime)
    yday(datetime)
    mday(datetime)
    wday(datetime)

For `month` and `wday` we can use the parameter `label=TRUE` to return
abbreviated name of the month or day of the week. If you add
`abbr = FALSE` it returns the full name.

    month(datetime, label = TRUE)
    wday(datetime, label = TRUE, abbr = FALSE)

We can use weekday to see if flights mostly go during weekdays or during
weekends

    flights_dt %>%
      mutate(wday = wday(dep_time, label = TRUE),
             weekend = wday %in% c("Sat", "Sun")) %>%
      ggplot(aes(x = wday,
                 fill = weekend)) +
      geom_bar() +
      scale_fill_manual(values = c("FALSE" = "black", "TRUE" = "gray")) +
      theme(legend.position = "None")

There’s also an interesting pattern if we look at average departure
delay by minute within the hour, flights leaving in minutes 20-30 and
50-60 have much lower delays than rest of the hour

    flights_dt %>%
      mutate(minute = minute(dep_time)) %>%
      group_by(minute) %>%
      summarise(
        avg_delay = mean(arr_delay, na.rm = TRUE),
        n = n()
      ) %>%
      ggplot(aes(minute, avg_delay)) +
      geom_line()

If we explore a bit further it’s because it’s data collected by humans -
we prefer timnes that look nice and easy and so certain things get
rounded up / down.

#### 16.3.2 Rounding

An alternative to plotting individual components is to round dates to
nearby unit of times. To do this we can use `floor_date`, `round_date`,
`ceiling_date`. Each of these functions takes a vector of dates and then
what unit we want to scale

    flights_dt %>%
      count(week = floor_date(dep_time, "week")) %>% 
      ggplot(aes(week, n)) +
      geom_line()

#### 16.3.3 Setting components

We can also use each accessor function listed above to set the values.
If we want to edit multiple values at once we can use `update`. If
values we set are too big they will roll over!

    (datetime <- ymd_hms("2016-07-06 12:34:56"))

    datetime
    year(datetime) <- 2020
    hour(datetime) <- hour(datetime) + 1

    update(datetime, year = 2020, month = 2, mday = 2, hour = 2)

    ymd("2015-02-01") %>%
      update(mday = 30)

    ymd("2015-02-01") %>%
      update(hour = 400)

Using update we can easily change the dates for every flight through the
year to a single day which then allows us to look at distribution of
flights across the course of the day. Instead of looking at different
days, the focus is on the time

    flights_dt %>%
      mutate(dep_hour = update(dep_time, yday = 1)) %>%
      ggplot(aes(dep_hour)) +
      geom_freqpoly(binwidth = 300)

We can quickly see that most airports seem to be shut between midnight
and 6, with very few flights going out just before 6. Also, first period
of the morning seems to be busiest and then there’s a peak of outgoing
flights at around 3 after which it trails down to stop.

##### Exercises

-   How does the distribution of flight times within a day change over
    the course of the year?

<!-- -->

    flights_dt %>%
      select(dep_time) %>%
      filter(!is.na(dep_time)) %>%
      mutate(dep_hour = update(dep_time, yday = 1)) %>%
      mutate(month = factor(month(dep_time))) %>% 
      ggplot(aes(x = dep_hour,
                 colour = month)) +
      geom_freqpoly(binwidth = 60 * 60)

There doesn’t seem to be much difference over the year

-   Compare dep\_time, sched\_dep\_time and dep\_delay. Are they
    consistent? Explain your findings.

In theory dep\_delay should be dep\_time - sched\_dep\_time. When
running it though we can see there are still over 1200 discrepancies.
Looking through this most of these seem to be errors when we processed
the data befre as we didn’t take into consideration flights that are
departing on the next day compared to when they were scheduled.

    flights_dt %>%
      select(dep_time, sched_dep_time, dep_delay) %>%
      mutate(dep_delay_check = sched_dep_time + dep_delay * 60) %>%
      filter(dep_delay_check != dep_time)

-   Compare air\_time with the duration between the departure and
    arrival. Explain your findings. (Hint: consider the location of the
    airport.)

<!-- -->

    flights_dt %>%
      mutate(
        flight_duration = as.numeric(arr_time - dep_time),
        air_time_mins = air_time,
        diff = flight_duration - air_time_mins
      ) %>%
      select(origin, dest, flight_duration, air_time_mins, diff)

-   How does the average delay time change over the course of a day?
    Should you use dep\_time or sched\_dep\_time? Why?

<!-- -->

    flights_dt %>%
      mutate(hour = hour(sched_dep_time)) %>%
      group_by(hour) %>%
      summarise(avg_delay = mean(dep_delay, na.rm = TRUE)) %>%
      ggplot(aes(x = hour,
                 y = avg_delay)) +
      geom_point() +
      geom_smooth()

Average delay time goes up by a lot as the day gets later. Part of this
is because there aren’t many flights before 6 and a built up of delays
will lead to other flights being delayed as well. If we use dep\_time
instead the picture changes because there are more flights in the night.
This is because there are very few flights and the ones in the data seem
to have been delayed.

-   On what day of the week should you leave if you want to minimise the
    chance of a delay?

<!-- -->

    flights_dt %>%
      mutate(wday = wday(sched_dep_time, label = TRUE)) %>%
      group_by(wday) %>%
      summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
      ggplot(aes(x = wday,
                 y = dep_delay)) +
      geom_col()

Saturday seems to be the best day where there are no delays followed by
Sunday. If you need to go during a weekday it’s best to pick Tuesday.

-   What makes the distribution of
    diamonds*c**a**r**a**t**a**n**d**f**l**i**g**h**t**s*sched\_dep\_time
    similar?

Both of these variables are influenced by the way humans want to
perceive data. For the diamonds dataset we can see there’s a clear skew
to numbers like 1, 1/3, 1/2. For flights\_dt we can see there’s a clear
rounding of minutes with 00 and 30 being favoured in particular.

    diamonds %>%
      ggplot(aes(x = carat %% 1 * 100)) +
      geom_histogram(binwidth = 1)

    flights_dt %>%
      ggplot(aes(x = minute(sched_dep_time))) +
      geom_histogram(binwidth = 1)

-   Confirm my hypothesis that the early departures of flights in
    minutes 20-30 and 50-60 are caused by scheduled flights that leave
    early. Hint: create a binary variable that tells you whether or not
    a flight was delayed.

If we create a variable that shows us whether a flight is early or not
we can quickly summarise by minute when those flights left. As shown in
the graph the hypothesis can’t be rejected.

    flights_dt %>%
      mutate(
        minute = minute(dep_time),
        early = arr_delay < 0
      ) %>%
      group_by(minute) %>%
      summarise(
        early = mean(early, na.rm = TRUE),
        n = n()
      ) %>%
      ggplot(aes(minute, early)) +
      geom_line()

### 16.4 Time spans

There are three classes that fall under time spans:

-   durations, which represent exact number of seconds

-   periods, which represent human units like weeks and months

-   intervals, which represent a starting and ending point

In R, when we subtract two dates, we get a difftime object. Because
difftime object records a time span of seconds, minutes, hours, days, or
weeks there’s a bit of ambiguity that can be difficult to work with. To
combat that we can use a lubridate function that always uses seconds,
`as.duration`

    h_age <- today() - ymd(19910607)
    h_age
    as.duration(h_age)

Durations come with a few different constructors. Because it always uses
a single unit of measurement (seconds) we can easily add and multiply

    dhours(15)
    dminutes(10)
    dhours(c(12,24))
    ddays(0:5)
    dweeks(3)
    dyears(1)

    2 * dyears(1)
    dyears(1) + dweeks(12) + dhours(15)

We can also add and subtract durations to and from days

    today() + ddays(1) # tomorrow
    wday(today() - dyears(1), label = TRUE) # last year

#### 16.4.2 Periods

As durations represent an exact number of seconds we can sometimes run
into unexpected results, mainly because of timezones. To solve this
problem lubridate provides **periods**. Periods are time spans but don’t
have a fixed length in seconds - instead, they work with “human” times
which allows them to work in a more intuitive way.

    one_pm <- ymd_hms("2016-03-12 13:00:00")

    one_pm + days(1)

Just likew tih seconds, periods can be created with a number of friendly
constructor functions. Most of these have the exact same format as the
seconds functions just with the **d** removed so `seconds`, `minutes`
and so forth. We can still add and multiply periods along with adding
them to dates:

    10 * (months(6) + days(1))
    days(50) + hours(25) + minutes(2)

    ymd("2016-01-01") + dyears(1) # leap year so works incorrectly (as calculates year in seconds)
    ymd("2016-01-01") + years(1)

Now that we know these tricks we can use them to fix oddities in our
flight dates dataset where some planes seem to arrive to their
destination before they’ve departed

    flights_dt %>%
      select(-origin:-arr_delay) %>%
      filter(arr_time < dep_time)

As we can see these seem to be overnight flights. We used the same date
information for departure and arrival times but these flights actually
arrived the following day. Can fix this by adding `days(1)`

    flights_dt <- flights_dt %>%
      mutate(
        overnight = arr_time < dep_time,
        arr_time = arr_time + days(overnight * 1), # USES 1* TRUE (1) where it is overnight flight
        sched_arr_time = sched_arr_time + days(overnight * 1)
      )

    flights_dt %>% 
      filter(overnight, arr_time < dep_time)

#### 16.4.3 Intervals

When it comes to things like leap years there’s not always enough info
for lubridate to give a clear answer. If we do want to have an accurate
measurement, we’ll have to use an **interval**. An interval is defined
as a duration with a starting point - that makes it precise so we can
determine exactly how long it is.

    next_year <- today() + years(1)
    (today() %--% current_year) / ddays(1)

    # to find out how many periods fall into an interval, have to use integer division
    (today() %--% next_year) %/% days(1)

#### 16.4.4 Summary

How do we pick between durations, periods, and intervals? Should pick
the simplest that solves our problem. If we care about physical time -
use duration. If we need to add human times, use period. If we need to
figure out how long a span is in human units, use interval

![table of date
manipulations](https://d33wubrfki0l68.cloudfront.net/0020136325ea844476bc61eb7e95d2ac5aeebf00/893e9/diagrams/datetimes-arithmetic.png)

##### Exercises

-   Why is there months() but no dmonths()?

Because there is no unambiguous answer to measuring a month in seconds.
Some months are 31 days, some are 30 - some are 28!

-   Explain days(overnight \* 1) to someone who has just started
    learning R. How does it work?

It works because in the mutation we’re assigning a `TRUE` or `FALSE` to
the column overnight. In R, TRUE can also be seen as a 1 and FALSE can
also be seen as a 0. So days(overnight \* 1) will be going through all
the values in the overnight column and if it’s FALSE do 0 \* 1 (still 0)
or 1 \* 1 (1) and we then add that to arr\_time

-   Create a vector of dates giving the first day of every month
    in 2015. Create a vector of dates giving the first day of every
    month in the current year.

<!-- -->

    ymd("2015-01-01") + months(0:11)

    ymd("2020-01-01") + months(0:11)

-   Write a function that given your birthday (as a date), returns how
    old you are in years.

<!-- -->

    birthday_years <- function(bday) {
      (ymd(bday) %--% today()) %/% years(1)
    }

    birthday_years(19910607)

-   Why can’t (today() %–% (today() + years(1))) / months(1) work?

<!-- -->

    (today() %--% (today() + years(1))) / months(1)

Because of the answer to question 1 - a month isn’t clearly defined. Are
we dividing by 31, 30 or 28? To find months within an interval we’ll
have to use `%/%`instead of `/`

### 16.5 Time zones

Time zones are very complicated because of their interaction with
geopolitical entities. They’re not all that important for data analysis
but some challenges we’ll need o tackle. The first challenge is that
everyday names tend to be ambiguous. To get an entire list of timezones
we can use `OlsonNames()` (very long list). As a default, lubridate will
always use UTC.

To change timezones we can use `with_tz(x, tzone = "Australia/Lord_Howe`
or `force_tz`
