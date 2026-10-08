# ranypia vs. the Social Security Act: audit

October 8, 2026 · Anthony Colavito. Working copy:
https://claude.ai/code/artifact/d72dea60-6a95-4f51-b97b-55c34577769a

## Summary

ranypia's core benefit formula matches the Social Security Act: the PIA, AIME, family maximums, reductions, credits, rounding and the survivor rules all line up with Title II's text or SSA's implementing rules. The audit found four differences that change results inside ranypia's scope, two of which matter in practice, plus simplifications and practice-over-text choices worth tracking. The audit itself changed no code.

The two to think about first:

1. **A disabled worker's family maximum after conversion.** ranypia keeps the disability maximum after the worker converts to a retirement benefit at full retirement age. §203(a)(6) applies only while the worker is entitled to disability benefits, and SSA converts to the regular maximum at that point. Example: the regular maximum is $6,459.90 a month instead of $5,305.60 at age 70. This is inherited from pyanypia and AnyPIA, and it flows into `lifetime_benefits()`.
2. **The COLA stabilizer.** When the OASDI trust fund ratio is below 20%, §215(i)(1)(C)(ii) makes the COLA the lower of the CPI and wage increases. ranypia always uses the CPI increase. Under the Trustees' intermediate assumptions wages outgrow prices, so this has no effect; it matters for scenarios where wage growth falls below inflation.

The audit covered 25 provisions, compared against the code on the `phase3-4-lifetime` branch (ranypia 0.2.0).

## Decisions

From Anthony's review on October 8, 2026. Item numbers refer to the tables below.

**Fix now:**

- [ ] **#1** Use the regular family maximum for a disabled worker's family once the benefit converts to retirement at full retirement age.
- [ ] **#5** Allow child's and mother's/father's benefits when the deceased worker was currently insured.
- [ ] **#14** Replace the pre-1978 quarters-of-coverage approximation with actual historical data, and update the documentation to match.

**Will incorporate later:** the earnings test; divorced and surviving divorced spouses, remarriage and marriage-length rules; pre-1979 computation methods; the lump-sum death payment; payable (not only scheduled) benefits.

**Noted, no change planned:** #2 the COLA stabilizer; #7 a disabled child in care; #9 the combined family maximum; #10 to #13, where ranypia follows SSA practice.

**Ignored:** #3 the $1 recomputation threshold; #4 WEP and GPO dating; #6 pre-2016 deemed filing; totalization.

**Explained below:** #8 survivors of a disabled worker; disabled widow(er)s, disability recovery and prior disability.

## Differences that change results

These apply to calculations ranypia does today, under its default settings or a plausible reform scenario.

| # | Provision | The Act says | ranypia does | Who it affects | Resolution |
| --- | --- | --- | --- | --- | --- |
| 1 | §203(a)(6); POMS RS 00615.742 | The disability family maximum applies while the worker is entitled to disability benefits; it converts to the regular maximum when the benefit becomes a retirement benefit or the worker dies | `disabled_worker()` returns the disability maximum for benefit months after full retirement age too | Spouses and children of a disabled worker past full retirement age: their benefits are understated when the maximum binds. Survivors are unaffected, since `deceased_worker()` uses the regular maximum. | Fix |
| 2 | §215(i)(1)(C)(ii), §215(i)(5) | With an OASDI fund ratio under 20%, the COLA is the lower of the CPI and wage increases, with a later catch-up once the ratio exceeds 32% | Projected COLAs always follow the CPI (`cola_proj`) | Reform or stress scenarios where wage growth falls below inflation, e.g. `policy_replace(awi_growth = ...)` below the CPI path, or the high-cost alternative in years when wages lag. No effect under intermediate assumptions. | Ignore, make note |
| 3 | §215(f)(4) | An automatic recomputation is made only if it raises the PIA by at least $1 | Every benefit month uses all earnings through the prior year, so any increase counts, however small | Workers with small earnings after entitlement; at most a dollar a month. | Ignore |
| 4 | Pub. L. 118-273 (Jan. 5, 2025) | The WEP and GPO were repealed for benefits payable after December 2023 only | `wep_enabled` and `gpo_enabled` are switches for every month; `current_law()` turns both off for all months | Historical benefit months before 2024 for workers with noncovered pensions, if those months are ever modeled. | Ignore |

## Simplifications in lifetime_benefits()

The panel runner adds eligibility rules on top of the formula functions. Each of these departs from the Act in a way a panel run could notice; all are documented in the function's help page.

| # | Provision | The Act says | The runner does | Effect | Resolution |
| --- | --- | --- | --- | --- | --- |
| 5 | §202(d), §202(g) | Child's and mother's/father's benefits are payable if the worker died fully **or currently** insured | Requires the worker to be entitled or fully insured at death | Young survivor families of workers with short careers get nothing when they should get benefits | Fix |
| 6 | §202(r), as amended by the Bipartisan Budget Act of 2015 | Deemed filing at any age applies to people who turn 62 in 2016 or later; earlier cohorts were deemed to file only before full retirement age | Assumes deemed filing for everyone | Spouses born before 1954 who filed a restricted application are not modeled | Ignore |
| 7 | §202(s)(1) | A child in care can be under 16 **or disabled** | Counts only children under 16 | Parents caring for a disabled adult child lose spouse-with-child or mother's/father's benefits too early | Ignore, make note |
| 8 | §215(b)(2)(B) | Years wholly in a disability period are excluded from the survivor computation | For a disabled worker who dies, uses the higher of the death PIA (computed without the freeze) and the frozen disability PIA | Close to the statute, but not the statute's computation; can understate survivors in edge cases | Explain more (see below) |
| 9 | §203(a)(3) | A child entitled on two records has a combined family maximum | Each child is on one record | Families with two insured parents: children's benefits may be understated | Ignore, make note |

The runner also uses the disability family maximum after conversion; that is difference 1 above, inherited from `disabled_worker()`.

## Where ranypia follows SSA practice rather than the Act's text

These are not errors: the Act is silent or general, and ranypia follows the rule SSA actually applies. They are worth knowing when a result is cited as "what the law says."

| # | Rule | Source ranypia follows | The Act's text | Resolution |
| --- | --- | --- | --- | --- |
| 10 | A dually entitled member counts against the family maximum only for what is payable, and others share the rest (benefits from October 1999) | POMS RS 00615.768, an SSA policy that cites no statute (it has a separate rule for First Circuit states) | §203(a)(4) says only that benefits are reduced proportionately; it is silent on dual entitlement | Make note |
| 11 | Ages are attained the day before the birthday, so someone born on the 1st attains each age the month before | SSA regulations and AnyPIA | Not defined in Title II | Make note |
| 12 | Indexed earnings are rounded to the cent before summing | AnyPIA | §215(b)(3) specifies the index ratio, not intermediate rounding | Make note |
| 13 | Credits for the year of filing take effect the next January; a survivor gets all credits earned before death | 20 CFR 404.313(c) and (e)(1) | §202(w) and §202(e)(2)(C) are consistent with these, with the timing detail left to regulation | Make note |
| 14 | Pre-1978 quarters of coverage are approximated from annual earnings | The README's stated approximation | §213(a)(2) counts quarters with at least $50 of wages paid in the quarter | Fix, use actual historical data and amend documentation to reflect |

## Known out-of-scope provisions

Already listed in the README or the lifetime plan; the Act provisions are noted so they can be picked up later.

- **Earnings test**, §203(b) and (f), and the adjustment of the reduction factor at full retirement age, §202(q)(7). This also leaves out the widow(er)'s limit for a reduced disability benefit after a reduced retirement benefit. *Will incorporate eventually, make note.*
- **Divorced spouses and surviving divorced spouses**, §202(b)–(f), paid outside the family maximum under §203(a)(3)(C); **remarriage**; and the **marriage-length requirements** of §216(b)–(g). *Will incorporate eventually, make note.*
- **Disabled widow(er)s** in `lifetime_benefits()`, §202(e)(1)(B)(ii) (the 71.5% factor is in `family_benefits()`); **disability recovery**; and **prior disability** rules in §215(a)(2)(A) and §215(b)(2)(B). *Need more explanation (see below).*
- **Pre-1979 computation methods**, **totalization** (§233) and the **lump-sum death payment** (§202(i)). *We will want to incorporate pre-1979 computation methods and lump-sum death payments. We will ignore totalization.*
- **Scheduled versus payable benefits.** Benefits can be paid only from the trust funds, so after depletion only incoming revenue could be paid. ranypia computes scheduled benefits; the Act contains no automatic reduction rule to model. *We will want to incorporate payable benefits.*

## More explanation

### #8: survivors of a disabled worker

When a disabled worker dies, the Act computes the survivors' PIA with the disability "freeze": years wholly within the period of disability are dropped from the computation base years, and years wholly or partly in it from the elapsed years (§215(b)(2)(B)). If the worker was getting disability benefits within 12 months of death, the survivor computation also keeps the disability eligibility year, so it indexes wages to the same year as the disability PIA (§215(a)(2)(A)).

`deceased_worker()` knows nothing about disability, so it counts every year of disability as a zero year and indexes to the year of death. That understates the PIA. `lifetime_benefits()` works around it by taking the higher of that death PIA and the worker's disability PIA carried forward by COLAs, with the regular family maximum on it.

For most people this gives the statutory answer, because a worker who dies while on disability has a survivor PIA equal to the disability PIA. It can differ when:

- the worker had covered earnings during the disability period (a trial work period, say), which a recomputation at death could add; or
- disability benefits ended more than 12 months before death, so the statute recomputes with the freeze but a later eligibility year, and neither of ranypia's two PIAs is that computation.

A full fix would give `deceased_worker()` a disability period, as `disabled_worker()` already has.

### Disabled widow(er)s, disability recovery and prior disability

These are three separate gaps, all about disability.

**Disabled widow(er)s** (§202(e)(1)(B)(ii)). A widow(er) aged 50 to 59 who is disabled can get a survivor benefit before 60, at 71.5% of the PIA. The disability must begin within seven years of the worker's death (or of the end of a mother's/father's benefit), and a five-month waiting period applies. `family_benefits()` already has the `disabled_widow` kind; `lifetime_benefits()` never assigns it, so a disabled widow(er) under 60 gets nothing on the spouse's record until 60.

**Disability recovery.** Disability benefits stop when the person is no longer disabled (after a trial work period and a grace period). `lifetime_benefits()` assumes disability lasts until full retirement age.

**Prior disability** (§215(a)(2)(A), §215(b)(2)(B)). Someone who recovers and later retires keeps the freeze: the disability years drop out of their retirement computation, and if they got disability benefits within 12 months before turning 62, the earlier eligibility year is kept. `retired_worker()` assumes no prior disability, so it counts those years as zeros and understates the retirement benefit. This matters only once recovery is modeled, since without recovery a disabled worker converts at full retirement age instead.

## Provisions that match the Act

| Area | Provision | What was checked |
| --- | --- | --- |
| PIA formula | §215(a)(1)(A)–(B) | 90/32/15 percent factors; $180 and $1,085 bend points indexed by the AWI from two years before eligibility, rounded to the nearest dollar; PIA rounded down to the dime |
| AIME | §215(b)(1)–(3), (e) | Indexing to the second year before eligibility, later years nominal, earnings capped at the taxable maximum, AIME rounded down to the dollar |
| Computation years | §215(b)(2) | Elapsed years from 1951 or age 22 to the year before 62 or death; 5 dropout years; disabled workers one-fifth of elapsed years, at most 5; child-care years for disabled workers up to a combined 3; never fewer than 2 |
| Special minimum | §215(a)(1)(C) | $11.50 per year of coverage over 10, capped at 30 years; pre-1951 wages at $900 a year, up to 14 years |
| COLAs | §215(i)(2) | Increased PIA rounded down to the dime; effective with December |
| Recomputation timing | §215(f)(2)(D) | Earnings count from January of the following year; for a deceased worker, from the month of death |
| Family maximums | §203(a)(1)–(2), (a)(4), (a)(6) | 150/272/134/175 percent bands with $230/$332/$433 bend points; proportional reduction rounded down to the dime; disability maximum the smaller of 85% of AIME (or the PIA, if larger) and 150% of the PIA |
| Reductions | §202(q)(1), (q)(9) | Workers 5/9 of 1% for 36 months, then 5/12; spouses 25/36 of 1%, then 5/12; widow(er)s 28.5% scaled from 60 to full retirement age; disabled widow(er)s 71.5% |
| Delayed credits | §202(w); 20 CFR 404.313 | 2/3 of 1% a month for births after 1942; none after 70; timing per 404.313(c) |
| Survivors | §202(e)(2)(B)–(D) | Re-indexing year for a worker who died before 62; the deceased's credits, including the year of death; the limit at the larger of the reduced benefit and 82.5% of the PIA |
| Dual entitlement | §202(k)(3)(A), (q)(3) | Own benefit subtracted after the other benefit's reductions, the widow(er)'s limit and the family maximum; the spouse result equals "method C" |
| Children and parents | §202(d), (g) | 50% of the PIA while the worker lives, 75% after death; end at 18, 19 for students, continued for disability before 22; mother's/father's benefit 75% |
| Full retirement age | §216(l) | 65 rising by 2 months a year to 66, then to 67 for people who turn 62 in 2022 or later; for widow(er)s the schedule shifted by the year they turn 60 |
| Final rounding | §215(g) | Monthly benefits rounded down to the dollar after reductions |
| WEP and GPO repeal | Pub. L. 118-273 | Both off in `current_law()` |

## Method and sources

Each provision was read in the Act or the U.S. Code and compared with the R code that implements it; difference 1 was confirmed by running `disabled_worker()` at age 70 against `family_max()` on the same PIA. SSA's online compilation of the Act was last reviewed in 2010 and predates the 2015 and 2025 amendments, so current law (the WEP repeal, §215(i)) was checked against the U.S. Code. Where the compiled text of §202 cut off, the implementing regulations were used. Difference 6 relies on the 2015 amendment to §202(r) as I understand it; SSA's online regulation page predates it, so that one is worth confirming against the current text.

Separately, ranypia was confirmed to still match pyanypia 0.3.1: every fixture regenerates byte-identical, and the full suite passed against five fresh sets of random workers (about 2,950 expectations each, no failures).

Sources:

- [Social Security Act §215](https://www.ssa.gov/OP_Home/ssact/title02/0215.htm), [§202](https://www.ssa.gov/OP_Home/ssact/title02/0202.htm), [§203](https://www.ssa.gov/OP_Home/ssact/title02/0203.htm) (SSA compilation)
- [42 U.S.C. 415](https://uscode.house.gov/view.xhtml?req=granuleid:USC-prelim-title42-section415&num=0&edition=prelim) and [42 U.S.C. 402](https://www.law.cornell.edu/uscode/text/42/402) (current text)
- [20 CFR 404.313](https://www.ssa.gov/OP_Home/cfr20/404/404-0313.htm), [20 CFR 404.352](https://www.ssa.gov/OP_Home/cfr20/404/404-0352.htm), [20 CFR 404.623](https://www.ssa.gov/OP_Home/cfr20/404/404-0623.htm)
- POMS [RS 00615.742](https://secure.ssa.gov/poms.nsf/lnx/0300615742), [RS 00615.768](https://secure.ssa.gov/poms.nsf/lnx/0300615768), [RS 00615.320](https://secure.ssa.gov/poms.nsf/lnx/0300615320), [RS 00615.020](https://secure.ssa.gov/poms.nsf/lnx/0300615020)
