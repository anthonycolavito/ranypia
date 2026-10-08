# ranypia 0.2.0

* `disabled_worker()`: from the month the benefit converts to a retirement
  benefit at NRA, `mfb` is the regular family maximum on the same PIA
  instead of the disability maximum (section 203(a)(6); POMS RS 00615.742).
  This is a deliberate departure from AnyPIA and pyanypia, which keep the
  disability maximum; results before NRA are unchanged.
* New `currently_insured()` (section 214(b)). `lifetime_benefits()` now pays
  children and a parent caring for a child when the deceased worker was
  only currently insured, as section 202(d) and (g) allow.
* The earnings needed for a quarter of coverage are now stored as data
  (`qc_hist` in `current_law()`): the Act's $50 before 1978 and SSA's
  published amounts for 1978-2026, with only later years projected. Results
  are unchanged (the stored values equal the formula's), but a reform to
  `qc_base` no longer rewrites history.
* New `benefit_stream()`: a retired worker's benefit every year (or every
  month) from the claim month to a final age, each row exactly what
  `retired_worker()` gives with that `benefit_age`. End ages can differ by
  worker, and long runs are computed in chunks.
* `people =` may now hold several rows per id (one worker under several
  claim ages, say). Each row gets its own output row, on that id's earnings;
  before, only the first row for each id was used, silently. Output stays in
  earnings order, then people order within an id. Applies to
  `retired_worker()`, `disabled_worker()` (including a `childcare` matrix)
  and `deceased_worker()`.
* New `disabled_stream()`: a disabled worker's benefit from entitlement to a
  final age, each row exactly what `disabled_worker()` gives that month,
  with a `type` column marking the conversion to a retirement benefit at
  NRA. A new fixture checks `disabled_worker()` against pyanypia for
  benefit months out to age 100 (the earlier fixtures stopped before 62).
* New `lifetime_benefits()`: from a panel of people (with spouses and
  children), each person's monthly benefits from first entitlement until
  death, by type: their own retired or disabled benefit, and a spouse,
  spouse-with-child, widow(er), parent-with-child or child benefit on
  another record, sharing the family maximum and applying the rules below.
  Claiming ages are inputs. 100,000 people with one row a year take about
  90 seconds on a laptop.
* `family_benefits()` gains four statutory rules, each off unless its
  `auxiliary()` argument is given, so existing results are unchanged:
  * `worker_factor` for a widow(er): the widow(er)'s limit when the worker
    claimed early (the larger of the worker's reduced benefit and 82.5% of
    the PIA, POMS RS 00615.320), and the worker's delayed credits in the
    widow(er)'s base (RS 00615.706).
  * `own_pia` and `own_factor`: dual entitlement (RS 00615.020). Only the
    excess over the member's own benefit is paid, and from October 1999 the
    other members share the room that frees under the family maximum
    (RS 00615.768).
  * `end_age`: a member stops being paid from that age, such as a child at
    18 (20 CFR 404.352).

# ranypia 0.1.1

* Two fixes carried over from pyanypia 0.3.1, found by comparing with SSA's
  own C++ calculator: a disability non-freeze winner whose AIME is not the
  highest takes the highest-AIME method's family maximum, and the special
  minimum uses the corrected 1999 COLA from July 2001.

# ranypia 0.1.0

* First release: an R port of pyanypia 0.3.0's benefit-formula functions,
  reproducing its results to the cent (and through it SSA's AnyPIA).
* Tibble outputs, long panel earnings via `earnings_matrix()`, and a
  `people =` data frame for the one-call helpers.
