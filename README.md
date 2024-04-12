This is a R package.

Startup-download: 

`devtools::install_github("XiaPoppy/SEM")`


`library(dvinesem)`


| function                                                             | file                                                                             | description                                                                                                         |
|:--------------------------------------------------------------------:|:--------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------:|
| `createModel()`                                                      | [create_model.R](https://github.com/XiaPoppy/SEM/blob/main/R/create_model.R)     | Return an object which stores the dependence fomula, e.g. 'y ~ x1 + x2' of each node with two different data types. |
| `dvinelist()`                                                        | [dvinelist.R](https://github.com/XiaPoppy/SEM/blob/main/R/dvinelist.R)           | Get List of Dvine Objects and Quantile plots                                                                        |
| `get_adj_matrix()`<br/>`get_variable_names()`<br/>`get_igraph_obj()` | [get_adjM_name.R](https://github.com/XiaPoppy/SEM/blob/main/R/get_adjM_name.R)   | Get the adjacency matrix and an igraph object from a given edge list and variable names from an igraph object.      |
| `getModelData()`                                                     | [get_model_data.R](https://github.com/XiaPoppy/SEM/blob/main/R/get_model_data.R) | Return sub-dataset of the original data that contains only the columns used in the dependence formula.              |
| `qplot_sem()`                                                        | [qplot_sem.R](https://github.com/XiaPoppy/SEM/blob/main/R/qplot_sem.R)           | Get Quantile Plots from a Gaussia SEM                                                                               |
