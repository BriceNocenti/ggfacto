# the MCA interpretation table is stable

    Code
      print(mca_interpret(fx_mca(), axes = 1:2), n = Inf, width = Inf)
    Output
      | Axe     |eigenvalue  |% variance  |cumul.  | |Benzecri's modified rate  |cumul. mod.  |
      |:--------|-----------:|-----------:|-------:|-|-------------------------:|------------:|
      |         | *Variance* |            |        | | *Benzecri*               |             |
      |         |   *<var>*  |  *<col%>*  |        | |                *<col%>*  |             |
      | Axe 1   |     0.234  |     23.4%  | 23.4%  | |                   82.6%  |      82.6%  |
      | Axe 2   |     0.196  |     19.6%  | 43.0%  | |                   15.4%  |      98.0%  |
      | Axe 3   |     0.177  |     17.7%  | 60.7%  | |                    2.0%  |       100%  |
      | Axe 4   |     0.146  |     14.6%  | 75.4%  | |                          |             |
      | Axe 5   |     0.131  |     13.1%  | 88.4%  | |                          |             |
      |**Total**|   **0.884**|    **100%**|        | |                  **100%**|             |
      
      # A tabxplor tab: 8 x 7
        Axe                                 Question       contrib Positive_levels
                                                            <col%>                
      1 Axe 1: 23.4% of variance (mod. 83%) dinner           31.3% "dinner"       
      2 Axe 1: 23.4% of variance (mod. 83%) tea.time         25.7% "Not.tea time" 
      3 Axe 1: 23.4% of variance (mod. 83%) lunch            22.2% ""             
      4 Axe 1: 23.4% of variance (mod. 83%) Above mean ctr   73.8% ""             
      5 Axe 2: 19.6% of variance (mod. 15%) always           32.2% "always"       
      6 Axe 2: 19.6% of variance (mod. 15%) evening          31.6% "evening"      
      7 Axe 2: 19.6% of variance (mod. 15%) breakfast        27.7% "Not.breakfast"
      8 Axe 2: 19.6% of variance (mod. 15%) Above mean ctr   91.5% ""             
          `  ` Negative_levels  `   `
        <col%> <chr>           <col%>
      1  29.2% ""                    
      2  14.5% "tea time"       11.2%
      3        "lunch"          19.0%
      4  43.6% ""               30.2%
      5  21.1% "Not.always"     11.0%
      6  20.8% "Not.evening"    10.9%
      7  13.3% "breakfast"      14.4%
      8  55.2% ""               36.3%
      # contribution to the variance of the axis (vs the mean contribution): ×10 ×5 ×2 ×1 ×1 ×2 ×5 ×10
      # contrib: the whole question's contribution to the axis

---

    Code
      print(mca_interpret(fx_mca(), axes = 1:2, complete = TRUE), n = Inf, width = Inf)
    Output
      | Axe     |eigenvalue  |% variance  |cumul.  | |Benzecri's modified rate  |cumul. mod.  |
      |:--------|-----------:|-----------:|-------:|-|-------------------------:|------------:|
      |         | *Variance* |            |        | | *Benzecri*               |             |
      |         |   *<var>*  |  *<col%>*  |        | |                *<col%>*  |             |
      | Axe 1   |     0.234  |     23.4%  | 23.4%  | |                   82.6%  |      82.6%  |
      | Axe 2   |     0.196  |     19.6%  | 43.0%  | |                   15.4%  |      98.0%  |
      | Axe 3   |     0.177  |     17.7%  | 60.7%  | |                    2.0%  |       100%  |
      | Axe 4   |     0.146  |     14.6%  | 75.4%  | |                          |             |
      | Axe 5   |     0.131  |     13.1%  | 88.4%  | |                          |             |
      |**Total**|   **0.884**|    **100%**|        | |                  **100%**|             |
      
      # A tabxplor tab: 8 x 12
        Axe                                 Question       contrib Positive_levels
                                                            <col%>                
      1 Axe 1: 23.4% of variance (mod. 83%) dinner           31.3% "dinner"       
      2 Axe 1: 23.4% of variance (mod. 83%) tea.time         25.7% "Not.tea time" 
      3 Axe 1: 23.4% of variance (mod. 83%) lunch            22.2% ""             
      4 Axe 1: 23.4% of variance (mod. 83%) Above mean ctr   73.8% ""             
      5 Axe 2: 19.6% of variance (mod. 15%) always           32.2% "always"       
      6 Axe 2: 19.6% of variance (mod. 15%) evening          31.6% "evening"      
      7 Axe 2: 19.6% of variance (mod. 15%) breakfast        27.7% "Not.breakfast"
      8 Axe 2: 19.6% of variance (mod. 15%) Above mean ctr   91.5% ""             
           ctr  coord   cos2 Negative_levels `ctr ` `coord ` `cos2 ` spread
        <col%> <mean> <row%> <chr>           <col%>   <mean>  <row%> <col%>
      1  29.2%   2.42    44% ""                                            
      2  14.5%   0.68    36% "tea time"       11.2%    -0.53     36%   100%
      3                      "lunch"          19.0%    -1.35     31%       
      4  43.6%               ""               30.2%                   55.3%
      5  21.1%   0.85    38% "Not.always"     11.0%    -0.44     38%   100%
      6  20.8%   0.84    37% "Not.evening"    10.9%    -0.44     37%   100%
      7  13.3%   0.55    33% "breakfast"      14.4%    -0.59     33%   100%
      8  55.2%               ""               36.3%                   88.6%
      # contribution to the variance of the axis (vs the mean contribution): ×10 ×5 ×2 ×1 ×1 ×2 ×5 ×10
      # contrib: the whole question's contribution to the axis
      # coord: coordinate on the axis
      # cos2: quality of representation
      # spread: share of the group's contribution the gap between its two sides accounts for

# the CA interpretation table is stable

    Code
      print(ca_interpret(fx_ca(), complete = TRUE), n = Inf, width = Inf)
    Output
      | Axe     |eigenvalue  |% variance  |cumul.  |
      |:--------|-----------:|-----------:|-------:|
      |         | *Variance* |            |        |
      |         |   *<var>*  |  *<col%>*  |        |
      | Axe 1   |     0.041  |     88.8%  | 88.8%  |
      | Axe 2   |     0.005  |     11.2%  |  100%  |
      |**Total**|   **0.046**|    **100%**|        |
      
      # A tabxplor tab: 9 x 11
        Axe                      Variable         Positive_levels    ctr  coord   cos2
                                                                  <col%> <mean> <row%>
      1 Axe 1: 88.8% of variance "Rows"           "Black"          72.6%   0.45    98%
      2 Axe 1: 88.8% of variance "Above mean ctr" ""               72.6%              
      3 Axe 1: 88.8% of variance "Columns"        "Never married"  54.2%   0.30   100%
      4 Axe 1: 88.8% of variance "Above mean ctr" ""               54.2%              
      5 Axe 2: 11.2% of variance "Rows"           ""                                  
      6 Axe 2: 11.2% of variance "Above mean ctr" ""                                  
      7 Axe 2: 11.2% of variance "Columns"        "Widowed"        46.2%   0.17    84%
      8 Axe 2: 11.2% of variance ""               "Divorced"       26.0%   0.09    86%
      9 Axe 2: 11.2% of variance "Above mean ctr" ""               72.3%              
        Negative_levels `ctr ` `coord ` `cos2 ` spread
        <chr>           <col%>   <mean>  <row%> <col%>
      1 ""                                            
      2 ""                                            
      3 "Married"        30.5%    -0.16     92%  84.7%
      4 ""               30.5%                   84.7%
      5 "Other"          84.5%    -0.22     63%       
      6 ""               84.5%                        
      7 "Married"        21.1%    -0.05      8%  86.2%
      8 ""                                            
      9 ""               21.1%                   86.2%
      # contribution to the variance of the axis (vs the mean contribution): ×10 ×5 ×2 ×1 ×1 ×2 ×5 ×10
      # coord: coordinate on the axis
      # cos2: quality of representation
      # spread: share of the group's contribution the gap between its two sides accounts for

# the PCA interpretation table is stable

    Code
      print(pca_interpret(fx_pca(), axes = 1:2), n = Inf, width = Inf)
    Output
      | Axe     |eigenvalue  |% variance  |cumul.  |
      |:--------|-----------:|-----------:|-------:|
      |         | *Variance* |            |        |
      |         |   *<var>*  |  *<col%>*  |        |
      | Axe 1   |     5.086  |     72.7%  | 72.7%  |
      | Axe 2   |     1.157  |     16.5%  | 89.2%  |
      | Axe 3   |     0.345  |      4.9%  | 94.1%  |
      | Axe 4   |     0.158  |      2.3%  | 96.4%  |
      | Axe 5   |     0.129  |      1.8%  | 98.2%  |
      |**Total**|   **6.875**|    **100%**|        |
      
      # A tabxplor tab: 8 x 10
        variable mean_Variables sd_Variables `sd/mean_Variables` `coord_Axe 1`
                         <mean>         <sd>                <cv>        <mean>
      1 mpg               20.09         5.93                 30%         -0.93
      2 cyl                6.19         1.76                 28%          0.96
      3 disp             230.72       121.99                 53%          0.95
      4 hp               146.69        67.48                 46%          0.87
      5 drat               3.60         0.53                 15%         -0.75
      6 weight             3.22         0.96                 30%          0.88
      7 qsec              17.85         1.76                 10%         -0.54
      8 Total                                                                 
        `contrib_Axe 1` `cos2_Axe 1` `coord_Axe 2` `contrib_Axe 2` `cos2_Axe 2`
                 <col%>       <row%>        <mean>          <col%>       <row%>
      1             17%          87%         -0.09              1%           1%
      2             18%          92%         -0.08              1%           1%
      3             18%          91%          0.09              1%           1%
      4             15%          76%         -0.36             11%          13%
      5             11%          56%         -0.48             20%          23%
      6             15%          78%          0.35             10%          12%
      7              6%          29%          0.81             56%          65%
      8            100%                                       100%             
      # coord: coordinate on the axis (Total): -0.8 -0.4 -0.2 -0.1 +0.1 +0.2 +0.4 +0.8
      # cos2: quality of representation (Total): -30 -20 -10 -5 +5 +10 +20 +30
      # contrib: its contribution to the variance of the axis; an axis sums to 100 %
      # sd/mean: coefficient of variation -- the standard deviation as a percentage of the mean, comparable between variables measured in different units

