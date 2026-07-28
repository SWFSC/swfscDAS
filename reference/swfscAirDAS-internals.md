# Internal functions for swfscAirDAS

These functions are exported only to be used internally by swfscAirDAS.
They implement functionality that is used when processing both DAS and
AirDAS data

## Usage

``` r
.chop_condition_eff(i, call.x, call.conditions, call.seg.min.km, call.func1)

.chop_equallength_eff(
  i,
  call.x,
  call.conditions,
  call.seg.km,
  call.r.pos,
  call.func1
)

.process_num(init.val, das.df, col.name, event.curr, event.na)

.process_chr(init.val, das.df, col.name, event.curr, event.na)

.segdata_proc(
  das.df,
  conditions,
  segdata.method,
  seg.lengths,
  section.id,
  df.out1
)

.segdata_aggr(data.list, curr.df, idx, dist.perc)

.dist_from_prev(
  z,
  z.distance.method = c("greatcircle", "lawofcosines", "haversine", "vincenty")
)
```

## Arguments

- i:

  ignore

- call.x:

  ignore

- call.conditions:

  ignore

- call.seg.min.km:

  ignore

- call.func1:

  ignore

- call.seg.km:

  ignore

- call.r.pos:

  ignore

- init.val:

  ignore

- das.df:

  ignore

- col.name:

  ignore

- event.curr:

  ignore

- event.na:

  ignore

- conditions:

  ignore

- segdata.method:

  ignore

- seg.lengths:

  ignore

- section.id:

  ignore

- df.out1:

  ignore

- data.list:

  ignore

- curr.df:

  ignore

- idx:

  ignore

- dist.perc:

  ignore

- z:

  ignore

- z.distance.method:

  ignore
