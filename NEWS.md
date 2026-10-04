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
