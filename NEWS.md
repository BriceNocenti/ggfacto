
# ggfacto 0.3.0

## Added : 
* New fonction : `HCPC_tab()`           
* New fonction : `ggmca_initial_dims()` 
* New fonction : `ggmca_with_base_ref()`
* New fonction : `ggmca_3d()`           
* New fonction : `pca_interpret()`      
* New fonction : `ggpca_cor_circle()`   
* New fonction : `ggpca_3d()`           

## Bug correction
* `ggmca()` can now properly color profiles of answer with `cah` (HCPC clusters) levels.


# ggfacto 0.2.3

## Added : 
* New argument `reverse_axes` in `ggmca()` to invert left and right or up and down. 

## Bug correction
* `mca_interpret` was not working with `MCA(excl = )`
* in `ggmca`, profiles of answers were plain wrong (because of the unanticipated results of a change in code)


# ggfacto 0.2.2

* First public version of ggfacto. 
