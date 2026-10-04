"""Writes tests/testthat/fixtures/*.json: inputs and pyanypia's outputs.

pyanypia is penny-exact against anypia-engine (and through it SSA's C++),
so ranypia matching these fixtures exactly chains back to the official
calculator. Run from the repo root with a pyanypia checkout:

    PYANYPIA=/path/to/pyanypia $PYANYPIA/.venv/bin/python tools/make_fixtures.py

The random workers come from pyanypia's own test generators (tests/cases.py).
"""

from __future__ import annotations

import json
import math
import os
import pathlib
import sys

import numpy as np

PYANYPIA = pathlib.Path(os.environ.get("PYANYPIA", "/Users/anthony/pyanypia"))
sys.path.insert(0, str(PYANYPIA))

import pyanypia as pa  # noqa: E402
from pyanypia import claiming, earnings, formula, rounding  # noqa: E402
from pyanypia.wep import _windfall_first_pct  # noqa: E402
from tests.cases import (  # noqa: E402
    adjusted_birth_index,
    as_matrix,
    column,
    disabled_cases,
    life_family_cases,
    retired_cases,
    survivor_cases,
)

OUT = pathlib.Path(__file__).resolve().parent.parent / "tests" / "testthat" / "fixtures"
OUT.mkdir(parents=True, exist_ok=True)


def clean(x):
    """JSON-safe: arrays to lists, NaN to None, numpy scalars to Python."""
    if isinstance(x, dict):
        return {k: clean(v) for k, v in x.items()}
    if isinstance(x, (list, tuple)):
        return [clean(v) for v in x]
    if isinstance(x, np.ndarray):
        return clean(x.tolist())
    if isinstance(x, (np.floating, float)):
        v = float(x)
        return None if math.isnan(v) else v
    if isinstance(x, (np.integer,)):
        return int(x)
    if isinstance(x, np.bool_):
        return bool(x)
    if isinstance(x, np.str_):
        return str(x)
    return x


def write(name: str, obj) -> None:
    path = OUT / f"{name}.json"
    path.write_text(json.dumps(clean(obj)))
    print(f"{path.name}: {path.stat().st_size // 1024} KB")


def rng(seed: int) -> np.random.Generator:
    return np.random.default_rng(seed)


# ---- rounding ----
r = rng(1)
amounts = r.uniform(0, 5000, 3000)
amounts[:300] = np.round(amounts[:300], 1) + 0.0005
years = [1970, 1975, 1981, 1982, 2000, 2030]
write("rounding", {
    "amounts": amounts, "years": years,
    "round_benefit": [rounding.round_benefit(amounts, y) for y in years],
    "cola_pct": [2.5, 11.2, 0.0], "cola_year": [2026, 1980, 2010],
    "apply_cola": [rounding.apply_cola(amounts, p, y)
                   for p, y in [(2.5, 2026), (11.2, 1980), (0.0, 2010)]],
})

# ---- policy ----
pol = {}
for alt in (1, 2, 3):
    p = pa.Policy.current_law(alt)
    pia_t, mfb_t, pia01, mfb01 = p.spec_min_tables
    pol[f"alt{alt}"] = {
        "awi": p.awi, "cola": p.cola, "taxmax": p.taxmax, "base77": p.base77,
        "qc_amount": p.qc_amount, "yoc_specmin": p.yoc_specmin, "yoc_wep": p.yoc_wep,
        "nra": p.nra, "drc_rate_by_elig": p.drc_rate_by_elig,
        "spec_min_pia": pia_t, "spec_min_mfb": mfb_t,
        "spec_min_pia_aug2001": pia01, "spec_min_mfb_aug2001": mfb01,
    }
higher = pa.CURRENT_LAW.replace(
    awi_growth={y: g + 1.0 for y, g in pa.CURRENT_LAW.awi_growth.items()})
pol["awi_plus_1"] = {"awi": higher.awi, "taxmax": higher.taxmax, "qc_amount": higher.qc_amount}
write("policy", pol)

# ---- claiming ----
months = np.arange(0, 61)
grid = []
for (by, bm, bd) in [(1955, 3, 15), (1960, 1, 1), (1964, 12, 2), (1943, 7, 28)]:
    for claim in range(745, 861, 3):
        for extra in (0, 5, 14):
            ar, drc = claiming.adjustment_months(by, bm, claim, birth_day=bd,
                                                 benefit_age=claim + extra)
            f = claiming.benefit_factor(by, bm, claim, birth_day=bd, benefit_age=claim + extra)
            grid.append([by, bm, bd, claim, claim + extra, int(ar), int(drc), float(f)])
write("claiming", {
    "nra_birth_years": list(range(1930, 2001)),
    "nra": [pa.normal_retirement_age(y, 6) for y in range(1930, 2001)],
    "months": months,
    "early": claiming.early_reduction_factor(months),
    "spouse": claiming.spouse_reduction_factor(months),
    "drc_elig": [1980, 1990, 2003, 2030],
    "drc": [claiming.delayed_credit_factor(months, e) for e in (1980, 1990, 2003, 2030)],
    "grid": grid,
})

# ---- earnings ----
r = rng(2)
n, first, width = 300, 1960, 75
m = r.uniform(0, 1.0, (n, width)) * pa.CURRENT_LAW.awi[first - 1937:first - 1937 + width] * 2.2
m = np.floor(m * 100 + 0.5) / 100
m[r.random((n, width)) < 0.25] = 0.0
by = r.integers(1938, 1995, n)
bm = r.integers(1, 13, n)
bd = r.choice([1, 2, 15], n)
elig = by + 62 - (bd == 1) * (bm == 1)
last = np.minimum(by + 62 + r.integers(-6, 6, n), first + width - 1)
comp = np.asarray(earnings.computation_years(by, bm, elig, birth_day=bd))
onset_y = np.minimum(by + r.integers(25, 61, n), first + width - 3)
onset_m = r.integers(1, 13, n)
through_month = r.integers(1, 13, n)
write("earnings", {
    "earnings": m, "first_year": first, "birth_year": by, "birth_month": bm, "birth_day": bd,
    "elig_year": elig, "last_year": last, "comp_years": comp,
    "comp_disabled": earnings.computation_years(by, bm, elig, birth_day=bd, disabled=True),
    "comp_death": earnings.computation_years(by, bm, elig, birth_day=bd, death_year=by + 40),
    "aime": earnings.aime(m, elig, comp, first_year=first, last_year=last),
    "indexed": earnings.indexed_earnings(m[:20], elig[:20], first_year=first),
    "qcs": earnings.quarters_of_coverage(m[:20], first_year=first),
    "yoc_specmin": earnings.years_of_coverage(m, first_year=first, last_year=last),
    "yoc_wep": earnings.years_of_coverage(m, first_year=first, last_year=last, kind="wep"),
    "through_month": through_month,
    "fully_insured": earnings.fully_insured(m, by, bm, through_year=last,
                                            through_month=through_month,
                                            first_year=first, birth_day=bd),
    "onset_year": onset_y, "onset_month": onset_m,
    "disability_insured": earnings.disability_insured(m, by, bm, onset_y, onset_m,
                                                      first_year=first, birth_day=bd),
})
# ---- formula ----
r = rng(4)
aimes = np.floor(r.uniform(0, 15000, 2000))
eligs = r.integers(1979, 2090, 2000)
pias = np.round(r.uniform(0, 5000, 2000), 1)
c_elig = r.integers(1979, 2060, 1500)
c_by = c_elig + r.integers(0, 31, 1500)
c_bm = r.integers(1, 13, 1500)
c_amt = np.round(r.uniform(100, 4000, 1500), 1)
write("formula", {
    "aime": aimes, "elig": eligs, "pia": formula.pia(aimes, eligs),
    "pia_in": pias, "family_max": formula.family_max(pias, eligs),
    "bp_years": list(range(1979, 2106)),
    "bend_points": formula.bend_points(np.arange(1979, 2106)),
    "mfb_bend_points": formula.family_max_bend_points(np.arange(1979, 2106)),
    "cola_amount": c_amt, "cola_elig": c_elig, "cola_by": c_by, "cola_bm": c_bm,
    "cola_out": formula.apply_colas(c_amt, c_elig, c_by, c_bm),
    "four_bracket_pia": formula.pia(aimes[:200], eligs[:200], policy=pa.CURRENT_LAW.replace(
        pia_bend_base=(180.0, 1085.0, 2000.0), pia_pct=(0.9, 0.32, 0.15, 0.05))),
    "di_family_max": pa.di_family_max(pias, aimes, eligs),
})

# ---- special minimum ----
yoc = np.arange(0, 36)
sm = {}
for ym in [(1985, 6), (1999, 12), (2001, 7), (2001, 8), (2001, 12), (2026, 11), (2070, 3)]:
    p_, f_ = pa.special_minimum_pia(yoc, *ym)
    sm[f"{ym[0]}-{ym[1]}"] = {"pia": p_, "mfb": f_}
write("minimum", {"yoc": yoc, "tables": sm})


def benefit_dict(b):
    return {k: getattr(b, k) for k in ("elig_year", "aime", "pia_elig", "pia", "mfb", "nra",
                                       "factor", "benefit", "method", "insured")}


# ---- retired workers ----
for alt in (1, 2, 3):
    cases = retired_cases(rng(100 + alt), 300)
    mat, first = as_matrix(cases)
    ins = {"earnings": mat, "first_year": first,
           "birth_year": column(cases, lambda c: c.birth[0]),
           "birth_month": column(cases, lambda c: c.birth[1]),
           "birth_day": column(cases, lambda c: c.birth[2]),
           "claim_age": column(cases, lambda c: c.claim_age),
           "benefit_age": column(cases, lambda c: c.benefit_age)}
    b = pa.retired_worker(mat, ins["birth_year"], ins["birth_month"], ins["claim_age"],
                          first_year=first, birth_day=ins["birth_day"],
                          benefit_age=ins["benefit_age"], policy=pa.Policy.current_law(alt))
    write(f"retired_alt{alt}", {"inputs": ins, "outputs": benefit_dict(b)})

# ---- disabled workers ----
for name, cc_flag, seed, pick in (
    ("disabled", False, 202, None),
    ("disabled_childcare", True, 9, None),
    # non-freeze winners whose AIME is not the highest, which take the family
    # maximum of the highest-AIME method; found by comparing with SSA's C++
    ("disabled_nonfreeze", True, 12, [169, 762, 949]),
):
    cases = disabled_cases(rng(seed), 300 if pick is None else 2500, childcare=cc_flag)
    if pick is not None:
        cases = [cases[i] for i in pick]
    mat, first = as_matrix(cases)
    cc = np.zeros_like(mat, dtype=bool)
    for i, c in enumerate(cases):
        for y in c.extra.get("childcare", ()):
            cc[i, y - first] = True
    ent = column(cases, lambda c: c.extra["ent"])
    ben = column(cases, lambda c: c.extra["ben"])
    ins = {"earnings": mat, "first_year": first, "childcare": cc,
           "birth_year": column(cases, lambda c: c.birth[0]),
           "birth_month": column(cases, lambda c: c.birth[1]),
           "onset_year": column(cases, lambda c: c.extra["onset"][0]),
           "onset_month": column(cases, lambda c: c.extra["onset"][1]),
           "onset_day": column(cases, lambda c: c.extra["onset"][2]),
           "ent_year": ent // 12, "ent_month": ent % 12 + 1,
           "ben_year": ben // 12, "ben_month": ben % 12 + 1}
    b = pa.disabled_worker(mat, ins["birth_year"], ins["birth_month"], ins["onset_year"],
                           ins["onset_month"], first_year=first, onset_day=ins["onset_day"],
                           entitlement=(ins["ent_year"], ins["ent_month"]),
                           benefit=(ins["ben_year"], ins["ben_month"]),
                           childcare=cc if cc_flag else None)
    write(name, {"inputs": ins, "outputs": benefit_dict(b)})

# ---- families ----
KIND = {"B": "spouse", "B2": "spouse_with_child", "C1": "child", "C2": "child",
        "D": "widow", "W": "disabled_widow", "E": "parent_with_child"}


def members(case, guarantee=None):
    out = []
    for bic, birth, ent in case.extra["family"]:
        out.append({"kind": KIND[bic.strip()], "birth_year": birth[0], "birth_month": birth[1],
                    "birth_day": birth[2], "claim_age": ent - adjusted_birth_index(birth),
                    "ent": ent})
    return out


life = []
for c in life_family_cases(rng(1), 200):
    w = pa.retired_worker(c.earnings, c.birth[0], c.birth[1], c.claim_age,
                          benefit_age=c.benefit_age)
    ben = adjusted_birth_index(c.birth) + c.benefit_age
    mem = members(c)
    aux = [pa.Auxiliary(x["kind"], x["birth_year"], x["birth_month"],
                        birth_day=x["birth_day"], claim_age=x["claim_age"]) for x in mem]
    fam = pa.family_benefits(w.pia, w.mfb, aux, benefit_year=ben // 12,
                             benefit_month=ben % 12 + 1)
    life.append({"pia": w.pia, "mfb": w.mfb, "ben_year": ben // 12,
                 "ben_month": ben % 12 + 1, "members": mem,
                 "full": fam.full, "after_max": fam.after_max,
                 "reduction_factor": fam.reduction_factor, "benefit": fam.benefit})
write("family_life", life)

surv = []
for c in survivor_cases(rng(4), 250):
    ben = c.extra["ben"]
    dy, dm, dd = c.extra["death"]
    d = pa.deceased_worker(c.earnings, c.birth[0], c.birth[1], dy, birth_day=c.birth[2],
                           benefit=(ben // 12, ben % 12 + 1))
    mem = members(c)
    aux = []
    for x in mem:
        g = None
        if x["kind"] in ("widow", "disabled_widow"):
            disabled = x["kind"] == "disabled_widow"
            g = pa.widow_guarantee_pia(
                c.earnings, c.birth[0], c.birth[1], dy, dm, death_day=dd,
                birth_day=c.birth[2], widow_birth_year=x["birth_year"],
                widow_birth_month=x["birth_month"], widow_birth_day=x["birth_day"],
                disabled_onset_year=(x["ent"] - 12) // 12 if disabled else None,
                entitlement_year=x["ent"] // 12 if disabled else None,
                benefit=(ben // 12, ben % 12 + 1))
        x["guarantee_pia"] = g
        aux.append(pa.Auxiliary(x["kind"], x["birth_year"], x["birth_month"],
                                birth_day=x["birth_day"], claim_age=x["claim_age"],
                                guarantee_pia=g))
    fam = pa.family_benefits(d.pia, d.mfb, aux, benefit_year=ben // 12,
                             benefit_month=ben % 12 + 1, survivor=True)
    years_ = sorted(c.earnings)
    surv.append({"earnings": [c.earnings[y] for y in years_], "first_year": years_[0],
                 "birth": c.birth, "death": c.extra["death"], "ben_year": ben // 12,
                 "ben_month": ben % 12 + 1, "members": mem,
                 "worker": benefit_dict(d), "full": fam.full, "after_max": fam.after_max,
                 "reduction_factor": fam.reduction_factor, "benefit": fam.benefit})
write("family_survivor", surv)

# ---- WEP / GPO ----
wep_on = pa.CURRENT_LAW.replace(wep_enabled=True)
cases = [c for c in retired_cases(rng(42), 1500)
         if (adjusted_birth_index(c.birth) + c.benefit_age) // 12 < 2024
         and c.birth[0] + 62 > 1986][:300]
mat, first = as_matrix(cases)
pension = np.round(rng(43).uniform(200, 3000, len(cases)), 2)
ins = {"earnings": mat, "first_year": first,
       "birth_year": column(cases, lambda c: c.birth[0]),
       "birth_month": column(cases, lambda c: c.birth[1]),
       "birth_day": column(cases, lambda c: c.birth[2]),
       "claim_age": column(cases, lambda c: c.claim_age),
       "benefit_age": column(cases, lambda c: c.benefit_age), "pension": pension}
b = pa.retired_worker(mat, ins["birth_year"], ins["birth_month"], ins["claim_age"],
                      first_year=first, birth_day=ins["birth_day"],
                      benefit_age=ins["benefit_age"], noncovered_pension=pension, policy=wep_on)
eg = np.arange(1979, 2030)
pct_grid = [[_windfall_first_pct(np.full(35, e), np.full(35, e + k), np.arange(35), 0.9)
             for k in (0, 3)] for e in eg]
write("wep", {"inputs": ins, "outputs": benefit_dict(b), "pct_elig": eg, "pct": pct_grid,
              "gpo_benefit": [1000.0, 500.0, 1000.0, 1000.5],
              "gpo_pension": [900.0, 1500.0, 0.0, 0.0],
              "gpo": pa.gpo_offset(np.array([1000.0, 500.0, 1000.0, 1000.5]),
                                   np.array([900.0, 1500.0, 0.0, 0.0]))})
