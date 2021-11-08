# c58_16

## Timeout
- spider/musical/0008
- spider/musical/0015
- spider/store_1/0038
- spider/store_1/0040
- spider/store_1/0042
- spider/chinook_1/0008

## GT Mismatch
- spider/product_catalog/0007

## Base Incorrect

## Fuzzy incorrect

### Using mutate+distinct where summarise should be used
- spider/allergy_1/0033
- spider/election_representative/0013

### max() of str not supported (fixed?)
- spider/product_catalog/0012
- spider/product_catalog/0013

### Bools consts not supported (fixed?)
- spider/apartment_rentals/0040

### ??? (but fixed?)
- spider/product_catalog/0003
- spider/apartment_rentals/0031

### Correct???
- spider/college_2/0062
- spider/election_representative/0019

### Missing distinct
- spider/product_catalog/0001

### Very wrong (underspecified)
- spider/customers_card_transactions/0022

### group_by column where it should summarise the whole table
- spider/insurance_fnol/0009
- spider/game_injury/0013

### Anti join where there should be a normal join (with an inverted filter)
- spider/allergy_1/0037
- spider/insurance_fnol/0015

### Normal join where there should be an intersect (with weird filters)
- spider/game_injury/0014

### Group has too many cols
- spider/customers_card_transactions/0040

### Wrong order for join and summarise
- spider/match_season/0021

## Exec. Error Solution
