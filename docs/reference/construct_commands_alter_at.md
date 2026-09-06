# make_par_values

helper to generate filter commands specifying rows of parameters_df that
should be altered given an alter_at statement

## Usage

``` r
construct_commands_alter_at(alter_at)
```

## Arguments

- alter_at:

  string specifying filtering operations to be applied to parameters_df,
  yielding a logical vector indicating parameters for which values
  should be altered.

## Value

string specifying a filter command
