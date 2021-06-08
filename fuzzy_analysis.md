# c55_16

## Timeout
- spider/musical/0008
- spider/musical/0015
- spider/store_1/0038
- spider/store_1/0040
- spider/store_1/0042
- spider/chinook_1/0008
- spider/chinook_1/0016
- spider/chinook_1/0026

## Base Incorrect

### ??
- spider/college_2/0047

### Is correct??? (type problem?)
- spider/customers_card_transactions/0002
- spider/apartment_rentals/0036

## Fuzzy incorrect

### Incorrect R -> SQL semantics ??
- spider/allergy_1/0033
- spider/election_representative/0013

### Solution not in program space when it should be??
- spider/product_catalog/0012 (Max(STRING) not supported - change?)
- spider/product_catalog/0013
- spider/apartment_rentals/0040

### Solution has more lines than expected (and the extra lines make it wrong)
- spider/twitter_1/0011
- spider/product_catalog/0003
- spider/race_track/0012
- spider/apartment_rentals/0031
- spider/college_2/0062

### Solution has less lines than needed (underspecified)
- spider/election_representative/0019

### Missing distinct
- spider/product_catalog/0001

### Semi join where there should be a normal join
- spider/product_catalog/0007
- spider/allergy_1/0047
- spider/customers_card_transactions/0022

### Anti join where there should be a normal join (with an inverted filter)
- spider/allergy_1/0037
- spider/insurance_fnol/0009
- spider/insurance_fnol/0015
- spider/game_injury/0013

### Normal join where there should be an anti join (with an inverted filter)
- spider/coffee_shop/0010

### Normal join where there should be an intersect (with weird filters)
- spider/game_injury/0014

### Group has too many cols
- spider/customers_card_transactions/0040

### Wrong order for join and summarise
- spider/match_season/0021

### Wrong comparison operator

#### >= instead of >
- spider/flight_1/0012
- spider/climbing/0007

#### != instead of ==
- spider/match_season/0006

## Exec. Error Solution

### DISTINCT is not supported for window functions
- spider/university_basketball/0019