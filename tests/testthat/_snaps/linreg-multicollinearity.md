# variables with multicollinearity are returned for `lm` model

    Code
      result_lm_tbl
    Output
      # A tibble: 3 x 3
        variable   vif result               
        <chr>    <dbl> <chr>                
      1 disp      7.32 highly correlated    
      2 wt        4.84 moderately correlated
      3 hp        2.74 moderately correlated

# variables with multicollinearity are returned for `_lm` model

    Code
      result_linreg_tbl
    Output
      # A tibble: 3 x 3
        variable   vif result               
        <chr>    <dbl> <chr>                
      1 disp      7.32 highly correlated    
      2 wt        4.84 moderately correlated
      3 hp        2.74 moderately correlated

