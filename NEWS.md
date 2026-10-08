# ranypia 0.2.0

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
