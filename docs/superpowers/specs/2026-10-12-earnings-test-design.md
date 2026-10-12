# ranypia: the retirement earnings test (design)

Date: 2026-10-12
Status: approved by Anthony 2026-10-12; built on branch earnings-test.

## The law, as it applies from 2000 on

Sources: Social Security Act §203(b), (f) and (h), and §202(q)(7), read from
ssa.gov; POMS RS 00615.482 for the adjustment of the reduction factor. The
Senior Citizens' Freedom to Work Act of 2000 ended the test for months from
normal retirement age (NRA) on.

| Rule | Where | What it does |
|---|---|---|
| Who | §203(b)(1), (f)(1)(B) | Any beneficiary under NRA, worker or auxiliary. A worker's excess earnings reduce their own benefit and the benefits of everyone paid on their record, except a divorced spouse divorced 2+ years (§203(b)(2)). An auxiliary's own excess reduces only their own benefit. |
| Excess earnings | §203(f)(3) | 1/2 of annual earnings above the lower exempt amount × 12. In the year of reaching NRA: 1/3 above the higher exempt amount × 12, counting only earnings before the NRA month. In the year of death, still 12 months. Rounded down to the dollar. |
| Exempt amounts | §203(f)(8) | Lower: $670 a month for 1994 × AWI(t−2) / AWI(1992). Higher: $2,500 a month for 2002 × AWI(t−2) / AWI(2000). Rounded to the nearest $10 ($5 rounds up), never below the year before, and changed only in years with a COLA (frozen in 2010, 2011 and 2016). Checked: this reproduces $22,320 / $59,520 (2024), $23,400 / $62,160 (2025) and $24,480 / $65,160 (2026). |
| Earnings | §203(f)(5) | All wages, not capped at the taxable maximum and including noncovered work, plus net self-employment earnings. Pensions and similar retirement payments are excluded. |
| Charging | §203(f)(1), (f)(7) | Whole months' benefits are withheld, from January (or the first month of entitlement) forward, until the excess is used up. The worker's excess is charged before the auxiliaries'. What is left over in the last month is paid out in proportion to benefits. |
| Monthly test (grace year) | §203(f)(1)(E) | In the first year with a month of entitlement and no work, no deduction for a month with wages at or below 1/12 of the annual exempt amount and no substantial self-employment. |
| Adjustment of the reduction factor | §202(q)(7)(A) | From NRA, the reduction is recomputed leaving out every month with a full or partial work deduction (POMS: proration does not matter). This applies to retired workers, spouses and widow(er)s. Spouses also get credit for months with a child in care, which ranypia doesn't model yet. |
| Delayed credits | §202(w) | Since 2000 the test stops at NRA, so withholding never earns credits. |

## Where it fits in ranypia

ranypia has single-month functions (`retired_worker()`, `family_benefits()`)
and month-by-month drivers (`benefit_stream()`, `lifetime_benefits()`). The
test is annual, applies across a whole family and has a memory (the reduction
adjustment at NRA), so it belongs in the drivers. The single-month functions
only need one new input.

1. **Policy** (`R/policy.R`): primitives `ret_lower_1994 = 670`,
   `ret_higher_2002 = 2500`, `ret_rate_lower = 1/2`, `ret_rate_higher = 1/3`,
   and a switch `earnings_test = TRUE`; derived series `ret_lower` and
   `ret_higher` by year (historical published amounts as data, projected
   after 2026). Reforms (eliminate the test, change the amounts or rates)
   work through `policy_replace()`.
2. **A pure function** `earnings_test()` (new `R/earnings_test.R`, exported):
   vectorized over person-years. Given the year's earnings, age, NRA, the
   months of entitlement and the monthly benefits on the record, it returns
   the excess earnings and the months withheld. Examples can be checked
   against SSA's published worked examples.
3. **Reduction adjustment** (`R/claiming.R`): an optional `withheld_months`
   argument to `adjustment_months()` and `benefit_factor()`, and the same for
   spouses and widow(er)s in `aux_reduction()`. For benefit months from NRA
   on, the reduction months fall by that many. Left unset, results don't
   change.
4. **Drivers:** `benefit_stream()` and `lifetime_benefits()` get
   `earnings_test = FALSE`. When it is TRUE, they run year by year: the
   benefits payable, then the withholding, then the months withheld are
   carried into the factor from NRA. The output gains `withheld` and `paid`
   columns, and `benefit` stays the scheduled amount. `lifetime_benefits()`
   charges a worker's excess against the whole family on their record.

The default is off, so every existing result, and parity with pyanypia and
SSA's calculator (which has no earnings test), stays exact. The new code is
tested against hand-worked cases from the Act and SSA's examples.

## Decisions for Anthony

1. **Earnings within the year: annual approximation (Anthony,
   2026-10-12).** No monthly inputs. Each year:
   - Earnings are taken as spread evenly over the year. In the year of
     reaching NRA, the earnings counted are earnings × (months before
     NRA) / 12, tested against the higher amount at 1/3.
   - Each year's excess is withheld month by month from January (or the
     first month entitled), whole months until it is used up and the last
     month in part, across the family on a retired worker's record and
     shared in proportion in that last month. Given the annual excess, this
     is the law's charging exactly.
   - Every month with a deduction (full or partial, POMS) counts for the
     reduction recomputed at NRA.
   - First year of entitlement (the grace year): earnings × (months
     entitled) / 12. The law would charge nothing to someone who stops
     work at claiming (the monthly test) and full-year earnings to
     someone who keeps working (the annual test). Proration sits between
     the two, so the errors go in opposite directions and roughly offset
     across a population. An option `grace_year = "prorate"` (default),
     `"none"` (no withholding in that year) or `"annual"` (full-year
     earnings) lets a user pick the other limits.
   Left out: the monthly test's all-or-nothing months, and the whole-month
   timing of withholding within a year.
2. **History: include 1979–1999 (Anthony, 2026-10-12).** The test ran to
   age 72 through 1982 and to 70 from 1983 to 1999. The rate was 1/2 at
   every age before 1990, and 1/3 from NRA to 69 from 1990. There were two
   exempt-amount tiers (the upper set by statute in 1996–2002), stored as
   historical data from SSA's published table. Months withheld after NRA
   earned delayed credits, so `withheld_months` also feeds
   `adjustment_months()`'s credit count for those years.
3. **Default.** Recommended: off (`earnings_test = FALSE`), to keep parity.
   The alternative is on, which is closer to what people are paid but
   changes every existing result for claims before NRA.
4. **Proration (left out for now).** SSA's annual report and estimate
   timing, overpayment recovery and the §203(h) penalties are administrative.
   Recommended: model the deductions the law sets for the year, not SSA's
   timing of collecting them.
