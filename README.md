# Kroger - Market-Basket-Analysis
## Objective
The objective of this analysis is to study the interaction between two
pairs of products - **Pasta** with **Pasta Sauce** and **Syrup** with
**Pancake Mix**.
We intend to ascertain the **popularity index** for each commodity-brand
combination and further analyze the **magnitude of association** between
pairs of brands each belonging to one of the associated pairs of
commodities *(E.g. Brand X of Pasta Sauce with Brand Y of Pasta)*

## Approach

 Firstly, we intend to **rank** each brand within each commodity category
according to its overall popularity by examining total units sold and
then total dollar value *(We prefer to rank based on units sold over
dollar value, as we are assuming that lower prices is a factor in
popularity)*.

* Then, we intend to compute certain statistics which will help in our
**Market Basket Analysis**. These are the **support** for each commodity-brand
entity overall *(not within each category)* which is the frequency of
the itemset in the dataset, the **confidence** of item pairs which is the
how often we find that **association rule** to be true, and the **lift** which
is the main **statistic of interest** and is the degree to which those two
occurrences *(the purchase of the two items)* are dependent on one
another.
