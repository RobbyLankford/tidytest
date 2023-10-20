# a `tibble` of residuals are extracted from `lm` model

    Code
      utils::head(resids_tbl, n = 10)
    Output
      # A tibble: 10 x 2
         .pred .resid
         <dbl>  <dbl>
       1  23.6 -2.57 
       2  22.6 -1.60 
       3  25.3 -2.49 
       4  21.2  0.183
       5  18.2  0.459
       6  20.5 -2.37 
       7  15.6 -1.27 
       8  22.9  1.49 
       9  22.0  0.759
      10  20.0 -0.841

# a `tibble` of residuals is calculated using an `lm` model

    Code
      utils::head(resids_tbl, n = 10)
    Output
      # A tibble: 10 x 2
         .pred .resid
         <dbl>  <dbl>
       1  23.6 -2.57 
       2  22.6 -1.60 
       3  25.3 -2.49 
       4  21.2  0.183
       5  18.2  0.459
       6  20.5 -2.37 
       7  15.6 -1.27 
       8  22.9  1.49 
       9  22.0  0.759
      10  20.0 -0.841

