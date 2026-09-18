# the rendered tooltip text is stable

    Code
      for (lv in c("breakfast breakfast", "breakfast Not.breakfast")) cat(lv, "\n",
        txt[[lv]], "\n\n", sep = "")
    Output
      breakfast breakfast
      <b>breakfast</b>
      breakfast
      Frequency (n=144):  48%
      Contrib axe 1 :  7%
      Contrib axe 2 : 14%
      
      <b>Active variables:</b>
      breakfast_lv: (+52%) <font color="#300DFD"><b>100%</b></font>
      Not.tea time: (    -5%) <font color="#DCA331"><b>38%</b></font>
      evening_lv: (    -4%)  30%
      lunch_lv: (  +3%)  17%
      dinner_lv: (    -4%)    3%
      always_lv: (    -4%)  31%
      
      breakfast Not.breakfast
      <b>Not.breakfast</b>
      breakfast
      Frequency (n=156):  52%
      Contrib axe 1 :  7%
      Contrib axe 2 : 13%
      
      <b>Active variables:</b>
      breakfast_lv: (  -48%) <font color="#D60103"><b>  0%</b></font>
      Not.tea time: (  +5%) <font color="#02A5B3"><b>49%</b></font>
      evening_lv: (  +4%)  38%
      lunch_lv: (    -2%)  12%
      dinner_lv: (  +4%)  11%
      always_lv: (  +3%)  38%
      

