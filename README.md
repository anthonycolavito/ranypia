# ranypia

The Social Security benefit formula as R functions: AIME, PIA, the family
maximum, COLAs, claiming-age reductions and credits, insured status, the
special minimum, and spouse and survivor benefits. Every function is
vectorized, so the call that answers "what would this worker get?" also
runs a microsimulation, and the results come back as tibbles.

The arithmetic is SSA's own. ranypia is an R port of the Python package
[pyanypia](https://github.com/anthonycolavito/pyanypia), and its tests
require every result to match pyanypia's to the cent. pyanypia in turn
matches SSA's Detailed Calculator (AnyPIA, 2026 Trustees Report).

## Install

```r
# install.packages("remotes")
remotes::install_github("anthonycolavito/ranypia")
```

R 4.0 or newer, though current CRAN releases of tidyr need R 4.1, so a fresh
install needs R 4.1 (an R 4.0 setup with older tidyr works). ranypia depends on
tibble, tidyr and rlang.

## Quickstart

```r
library(ranypia)

# A worker born 15 June 1964 who earned the national average wage every
# year from 22 to 66. Ages are in months: claiming at 62 and 1 month, at
# the normal retirement age (67), and at 70.
earnings <- setNames(current_law()$awi[(1986:2030) - 1936], 1986:2030)
retired_worker(earnings, 1964, 6, c(62 * 12 + 1, 67 * 12, 70 * 12))
#> # A tibble: 3 × 10
#>   elig_year  aime pia_elig   pia   mfb   nra factor benefit method       insured
#>       <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl>  <dbl>   <dbl> <chr>        <lgl>
#> 1      2026  5825    2610. 2610. 4765.   804  0.704    1837 wage_indexed TRUE
#> 2      2026  5968    2656. 2998. 5449.   804  1        2998 wage_indexed TRUE
#> 3      2026  5968    2656. 3219. 5851.   804  1.24     3992 wage_indexed TRUE

# The same worker at 67 with the middle bracket cut from 32% to 30%
reform <- policy_replace(current_law(), pia_pct = c(0.90, 0.30, 0.15))
retired_worker(earnings, 1964, 6, 67 * 12, policy = reform)$pia
#> [1] 2892.8

# A spouse born March 1966 who claims at 64, in the month the worker claims at 67
b <- retired_worker(earnings, 1964, 6, 67 * 12)
family_benefits(b$pia, b$mfb, list(auxiliary("spouse", 1966, 3, claim_age = 64 * 12 + 3)),
                benefit_year = 2031, benefit_month = 6)
#> # A tibble: 1 × 7
#>   family member kind    full after_max reduction_factor benefit
#>    <int>  <int> <chr>  <dbl>     <dbl>            <dbl>   <dbl>
#> 1      1      1 spouse 1499.     1499.            0.771    1155
```

## Microsimulation with panel data

Earnings can be a long data frame (`id`, `year`, `earnings`), and the
per-person inputs can come from columns of a `people` data frame. Rows are
matched by `id`, and results keep it, so they join straight back:

```r
people   # id, birth_year, birth_month, claim_age, ...
panel    # id, year, earnings: one row per person-year

benefits <- retired_worker(panel, people = people)
people |> dplyr::left_join(benefits, by = "id")
```

With 100,000 workers and 66 years of earnings each, `retired_worker()`
takes about 4 seconds on a laptop. The arithmetic is matrix operations
throughout, looping only over years, never over people. A wide matrix (one
row per worker, columns named by year) works too, and so does a named
vector for one worker.

## Benefits across retirement

`benefit_stream()` gives the benefit from the claim month to a final age
(100 by default), one row a year or, with `step = 1`, every month. Each row
is what `retired_worker()` gives in that month, so COLAs, rounding and
recomputation for work after claiming are all included. `to_age` can vary
by worker, for instance a simulated age at death:

```r
s <- benefit_stream(earnings, 1964, 6, claim_age = 67 * 12)
#> one row per year: id or worker, claim_age, benefit_age, benefit_year,
#> benefit_month, aime, pia, mfb, factor, benefit, method, insured

# exact calendar-year totals
m <- benefit_stream(earnings, 1964, 6, claim_age = 67 * 12, step = 1)
tapply(m$benefit, m$benefit_year, sum)
```

`disabled_stream()` does the same for a disabled worker, from entitlement
(after the five-month waiting period) to a final age, with a `type` column
that switches from `"disabled"` to `"retired"` at NRA.

With panel earnings, `people` may list the same id more than once (one row
per claim age to compare, say); each row gets its own stream.

## Lifetime benefits on a panel

`lifetime_benefits()` runs the whole chain for a panel: each person's
benefits every month (or one month a year) from first entitlement until a
given age at death, including spouse, survivor and child benefits on their
family members' records.

```r
people <- data.frame(id = 1:2, birth_year = c(1964, 1966), birth_month = 6,
                     death_age = c(78, 92) * 12, claim_age = c(67, 62) * 12 + c(0, 1),
                     spouse_id = c(2, 1))
lb <- lifetime_benefits(panel, people, months = 6)
#> id, role, benefit_year, benefit_month, age, own_type, own_benefit,
#> aux_type, aux_record, aux_benefit, total
```

Claiming ages are inputs, and so is `death_age`, so mortality,
discounting and claiming strategies stay with the caller. With one row a
year, 100,000 people take about 90 seconds on a laptop.

## The building blocks

The one-call helpers at the bottom of this table chain the others. Each of
those is usable alone.

| Function | What it gives |
|---|---|
| `eligibility_year()`, `computation_years()`, `elapsed_years()` | the computation period |
| `capped_earnings()`, `indexed_earnings()`, `aime()` | average indexed monthly earnings |
| `bend_points()`, `pia()` | the PIA formula at eligibility |
| `family_max()`, `di_family_max()` | the maximum family benefit at eligibility |
| `apply_colas()` | carries a PIA or maximum to a benefit month |
| `normal_retirement_age()`, `earliest_claim_age()` | in months |
| `benefit_factor()`, `early_reduction_factor()`, `delayed_credit_factor()` | claiming early or late |
| `monthly_benefit()` | factor × PIA, rounded as SSA rounds it |
| `quarters_of_coverage()`, `fully_insured()`, `currently_insured()`, `disability_insured()` | insured status |
| `years_of_coverage()`, `special_minimum_pia()` | the special minimum |
| `childcare_aime()` | the AIME with child-care dropout years |
| `auxiliary()`, `family_benefits()` | spouse, child and survivor benefits under the family maximum |
| `widow_guarantee_pia()` | the re-indexed widow(er)'s guarantee |
| `wep_pia()`, `gpo_offset()` | the repealed WEP and GPO, off by default |
| `retired_worker()`, `disabled_worker()`, `deceased_worker()` | the whole chain in one call |
| `benefit_stream()`, `disabled_stream()` | a retired or disabled worker's benefits to a final age |
| `lifetime_benefits()` | a panel's benefits of every type until death |

## Conventions

- **Ages are whole months**, counted from the month of the day before
  birth, which is how SSA counts. Someone born 15 March 1964 is 62 (744
  months) in March 2026. Someone born on the 1st attains each age in the
  month before their birthday month, which `birth_day = 1` handles.
- **Benefits are for a month.** The one-call helpers compute the benefit
  for the claim month unless you pass a later `benefit_age` (or
  `benefit = list(year, month)`), and they ignore earnings from the benefit
  year on.
- **Errors name the problem.** Bad input stops with the field, how many
  rows are wrong and the first one, e.g.
  `earnings: negative in 2 row(s); first at row 4`.

## Policy and reforms

Every function takes `policy`, defaulting to `current_law()`: present law
under the 2026 Trustees Report intermediate assumptions. `current_law(1)`
and `current_law(3)` give the low- and high-cost alternatives.
`policy_replace()` changes primitive inputs, and everything derived from
them follows. `policy_with_series()` pins individual derived values:

```r
faster_wages <- policy_replace(current_law(), awi_growth = current_law()$awi_growth + 0.5)
four_brackets <- policy_replace(current_law(), pia_bend_base = c(180, 1085, 2000),
                                pia_pct = c(0.90, 0.32, 0.15, 0.05))
higher_max <- policy_with_series(current_law(), "taxmax", c("2030" = 250000))
```

## What is exact, and what is not covered

The same as pyanypia, with one deliberate exception: after a disabled
worker's benefit converts to a retirement benefit at NRA, `disabled_worker()`
uses the regular family maximum, as section 203(a)(6) requires, where AnyPIA
keeps the disability maximum. Results are exact for wage-indexed computations with
eligibility in 1979 or later: retirement; disability, with the child-care
dropout years and the non-freeze computation; and survivors, with the
re-indexed widow(er)'s guarantee. The special minimum and the WEP are also
exact.

`family_benefits()` also applies rules AnyPIA does not model, written from
the statute and SSA's POMS and tested against POMS's worked examples rather
than against pyanypia: the widow(er)'s limit when the worker claimed early,
the worker's delayed credits in a survivor's benefit, dual entitlement for
a member with their own benefit (including its effect on the family
maximum), and stop ages such as a child's 18th birthday. Each is off unless
its `auxiliary()` argument is given.

Not covered:

- the pre-1979 computation methods
- totalization, and the disability guarantee after a prior disability
- divorced spouses, and the earnings test
- **pre-1978 quarters of coverage without an earnings record.** The Act
  credited a quarter for each calendar quarter with $50 of wages, which
  annual earnings can't show. Give the counts from an earnings record in
  `qc_history` (accepted by every function that tests insured status) and
  they are used exactly; without one, an annual rule approximates them and
  can overstate them
- **the GPO**, which follows the statute, because AnyPIA has none

The WEP and GPO were repealed for benefits after December 2023, so
`current_law()` applies neither.

## How it is tested

`tools/make_fixtures.py` runs pyanypia over thousands of random workers:
retired, disabled (with and without child care), survivors, families, and
WEP cases. It writes the inputs and pyanypia's outputs to
`tests/testthat/fixtures/`. The testthat suite requires ranypia to
reproduce every value identically. Running the tests needs only R.

## License

MIT. ranypia is not an official Social Security Administration product.
