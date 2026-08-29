# the console interpretation table is stable

    Code
      print(mca_interpret(fx_mca(), axes = 1:2, type = "console"), n = Inf, width = Inf)
    Output
      # A tibble: 12 x 9
         Axe     pct Question   contrib Positive_levels  `  ` Negative_levels `   `
         <chr> <dbl> <chr>        <dbl> <chr>           <dbl> <chr>           <dbl>
       1 1      23.4 dinner        31.3 dinner           29.2 <NA>             NA  
       2 1      23.4 tea.time      25.7 Not.tea time     14.5 <NA>             NA  
       3 1      23.4 tea.time      25.7 <NA>             NA   tea time         11.2
       4 1      23.4 lunch         22.2 <NA>             NA   lunch            19.0
       5 1      NA   All levels    73.8 <NA>             43.6 <NA>             30.2
       6 2      19.6 always        32.2 always           21.1 <NA>             NA  
       7 2      19.6 always        32.2 <NA>             NA   Not.always       11.0
       8 2      19.6 evening       31.6 evening          20.8 <NA>             NA  
       9 2      19.6 evening       31.6 <NA>             NA   Not.evening      10.9
      10 2      19.6 breakfast     27.7 <NA>             NA   breakfast        14.4
      11 2      19.6 breakfast     27.7 Not.breakfast    13.3 <NA>             NA  
      12 2      NA   All levels    91.5 <NA>             55.2 <NA>             36.3
         spread
          <dbl>
       1   NA  
       2  100  
       3  100  
       4   NA  
       5   55.3
       6  100  
       7  100  
       8  100  
       9  100  
      10  100  
      11  100  
      12   88.6

# the PCA interpretation table is stable

    Code
      print(pca_interpret(fx_pca(), axes = 1:2), n = Inf, width = Inf)
    Output
      # A tibble: 8 x 7
        variable  Dim.1  ctr.1 cos2.1  Dim.2  ctr.2 cos2.2
        <fct>    <mean> <col%> <row%> <mean> <col%> <row%>
      1 mpg       -0.93    17%    87%  -0.09     1%     1%
      2 cyl        0.96    18%    92%  -0.08     1%     1%
      3 disp       0.95    18%    91%   0.09     1%     1%
      4 hp         0.87    15%    76%  -0.36    11%    13%
      5 drat      -0.75    11%    56%  -0.48    20%    23%
      6 weight     0.88    15%    78%   0.35    10%    12%
      7 qsec      -0.54     6%    29%   0.81    56%    65%
      8 Total             100%                 100%       

