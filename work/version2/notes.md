

Generate pairs
--------------------------

### `pair(x, y, cluster = NULL, add_xy = TRUE)`

Methods:

- `pair.data.frame`: converts `x` and `y` to `data.table` and calls `pair.data.table`
- `pair.data.table`

Notes:
- Is it necessary to have separate methods? Is it likely that we will have methods for other object
  types in the future? 
- `add_xy` is ignores when using the cluster variant.
- `pair` with `!is.null(cluster)` calls `pair_cluster`.


### `pair_blocking(x, y, on, clustr = NULL, add_xy = TRUE)`

`pair_blocking.data.frame`
`pair_blocking.data.table`
For these methods it is necessary, when working with a cluster, to split the data using the blocking
variables. 

