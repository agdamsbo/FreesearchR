# Test is date/datetime/time

Test is date/datetime/time

## Usage

``` r
is_datetime(data)
```

## Arguments

- data:

  data

## Value

factor

## Examples

``` r
vapply(REDCapCAST::redcapcast_data, is_datetime, logical(1))
#>                    record_id            redcap_event_name 
#>                        FALSE                        FALSE 
#>     redcap_repeat_instrument       redcap_repeat_instance 
#>                        FALSE                        FALSE 
#>                          cpr                    inclusion 
#>                        FALSE                         TRUE 
#>               inclusion_time                          dob 
#>                         TRUE                         TRUE 
#>                          age                  age_integer 
#>                        FALSE                        FALSE 
#>                          sex                 cohabitation 
#>                        FALSE                        FALSE 
#>                 hypertension                     diabetes 
#>                        FALSE                        FALSE 
#>                       region baseline_data_start_complete 
#>                        FALSE                        FALSE 
#>                 mrs_assessed                     mrs_date 
#>                        FALSE                         TRUE 
#>                    mrs_score                 mrs_complete 
#>                        FALSE                        FALSE 
#>                      con_mrs                     con_calc 
#>                        FALSE                        FALSE 
#>           consensus_complete               event_datetime 
#>                        FALSE                         TRUE 
#>                    event_age                   event_type 
#>                        FALSE                        FALSE 
#>           new_event_complete 
#>                        FALSE 
```
