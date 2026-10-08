# ranypia Lifetime Benefits Plan

> **For agentic workers:** REQUIRED SUB-SKILL: superpowers:executing-plans. Steps use `- [ ]` checkboxes. Work one phase per branch and PR; stop and ask the user at every **Ask** item.

**Goal:** Run ranypia on a panel of workers (and their spouses) and get every person's monthly Social Security benefit, by type, from first entitlement until death: retired workers, disabled workers, spouses, children and survivors.

**Starting point:** ranypia 0.2.0 on branch `people-dupes-and-streams` (commits `fix: people may hold several rows per id` and `feat: benefit_stream()`). If that branch is not on GitHub, apply `ranypia-0.2.0.patch` with `git am` first. `benefit_stream()` in `R/stream.R` is the pattern every later stream follows.

**Architecture:** Thin layers over the existing formula functions. No formula code is rewritten. Each new layer expands people into person-months, calls an existing one-call function with a vector of benefit months, and returns a long tibble. Missing statutory rules are added as small, separately tested functions in the module they belong to.

**Spec for the rules:** Title II of the Social Security Act and SSA's POMS. pyanypia has no widow's limit, no dual entitlement and no lifetime streams, so Phases 2 to 4 have **no fixture oracle**. Their tests are hand-worked cases from the statute and POMS examples, written down in the test file with the citation.

## Global constraints

- Everything that exists today must stay cent-exact: the full existing suite passes unchanged after every task.
- New money values: `round_benefit()` and `floor_dollar()` exactly as SSA applies them; never `round()`.
- Fixed summation order (explicit loops) for money, as in the rest of the package.
- Errors use `abort_field()`: field name, count of bad rows, first bad row.
- Every new exported function gets roxygen docs, an example and a NEWS entry.
- Regenerate docs with the roxygen2 version in `DESCRIPTION` (`RoxygenNote: 7.1.2`), or hand-edit `.Rd` files, so unrelated help pages are not rewritten.
- Performance target: 100,000 people, yearly rows to death, under 5 minutes on a laptop. Chunk like `benefit_stream()` does.
- Commits end with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`.

## Known gaps this plan closes

Checked against the code on 2026-10-08:

| Gap | Effect today | Phase |
|---|---|---|
| No widow(er)'s limit (RIB-LIM) | Widow of a worker who claimed early is overpaid. Example: worker born June 1964, average wage, claims at 62+1; widow claims at her NRA in June 2034. ranypia pays $3,219; the limit is max($2,266, 82.5% × $3,219.40 = $2,656). | 2 |
| Survivors don't inherit the deceased worker's delayed credits | Widow of a worker who claimed late is underpaid | 2 |
| No dual entitlement (§202(k)) | A spouse or widow(er) with their own record gets the full auxiliary benefit on top of their own | 2 |
| Child and spouse-with-child benefits never end | `auxiliary()` has no stop ages (child: 18, or 19 in school, or disabled before 22; spouse-with-child: youngest child 16) | 2 |
| Only retired workers have streams | No disabled, family or survivor streams | 1, 3 |

## Phase 1: Disabled-worker streams

`disabled_worker()` already takes `benefit = list(year, month)` with vectors (checked: 24 benefit months in one call work).

- [x] Add `disabled_stream()` in `R/stream.R`, mirroring `benefit_stream()`: rows from entitlement (default: onset + 5-month waiting period, as `disabled_worker()` computes it) to `to_age`, every `step` months; `childcare` follows the row map.
- [x] Conversion at NRA: a disabled worker converts to a retired-worker benefit at NRA with the same PIA and no reduction. Verify `disabled_worker()` gives the right amount past NRA by comparing with pyanypia at a handful of ages; add a `type` column (`"disabled"` before NRA, `"retired"` from NRA).
- [ ] **Ask:** recovery from disability (benefits stop) — out of scope, or a `recovery_age` argument?
- [x] Tests: each row equals `disabled_worker()` at that month; conversion month; chunking; panel with `people`.

*Done 2026-10-08:* `disabled_worker()` matched pyanypia to the cent on 400 random cases with benefit months to age 100, and a 300-case fixture (`disabled_lifetime.json`, from `tools/make_fixtures.py`) now locks that in. Recovery was not added; a per-worker `to_age` ends a stream early in the meantime.

## Phase 2: Missing statutory rules

Each rule is its own function and test file. Put each in `R/family.R` unless noted.

- [x] **Widow(er)'s limit.** If the deceased worker was entitled to a reduced benefit, the widow(er)'s benefit is limited to the larger of what the worker was getting and 82.5% of the PIA. If the worker had delayed credits, the widow(er)'s base includes them. Add `worker_benefit` (the deceased's own benefit in the benefit month, or `NA` if never entitled) to `auxiliary()` for `"widow"`/`"disabled_widow"`, and apply the limit in `family_benefits()` after the age reduction. Look up and cite the governing POMS sections (the widow(er)'s limit and survivors' delayed credits, in the RS 00615 chapter) in the tests; do not cite section numbers from memory.
- [x] **Dual entitlement.** A spouse or widow(er) entitled to their own retired or disabled benefit gets only the excess of the auxiliary benefit over their own PIA, with age reductions applied to each part separately. Add `own_pia` and `own_benefit` to `auxiliary()`. Look up the POMS dual-entitlement sections and use their worked examples as test cases, cited by section.
- [x] **Termination ages.** Add an end month to children (18; 19 if in school; none if disabled before 22) and spouse-with-child (youngest child turns 16). Simplest form: `auxiliary(..., end_age = )` plus a helper that computes it.
- [x] **Ask:** the earnings test, divorced spouses (paid outside the family maximum) and remarriage before 60 — include, or leave out of scope as the README says today? *Answered 2026-10-08: leave out for now; revisit after Phase 4.*
- [x] Tests: hand-worked cases for each rule, including the June-1964 widow example above ($2,656), a widow of a worker who claimed at 70, and a spouse whose own PIA exceeds half the worker's (spousal benefit $0).

*Done 2026-10-08 (`tests/testthat/test-family-rules.R`):* rules follow POMS RS 00615.320 (limit), RS 00615.706 and 20 CFR 404.313(e) (survivors' credits), RS 00615.020 (dual entitlement, methods B and C), RS 00615.768 (family maximum with a dually entitled member, benefits from 10/1999) and 20 CFR 404.352 (child stop ages). Design choices for Phase 3: `end_age` defaults to no stop in `family_benefits()`, because pyanypia's fixtures include two children past 18; the streams should default children to 216 months. A dually entitled child or parent is paid their benefit less their own benefit, with no age reduction. Not covered: a reduced DIB after a reduced RIB in the limit (needs the earnings test's ARF), and the RIB-LIM rule that year-of-death credits wait until January.

## Phases 3 and 4: Family, survivor and panel benefits — done 2026-10-08

Built as one function, `lifetime_benefits()` in `R/lifetime.R`, rather than separate `family_stream()` and `survivor_stream()` exports: the family and survivor logic only makes sense once the panel says who is married to whom, who has died and which children are in care. It uses only the one-call functions and `family_benefits()`.

- [x] Own benefits: retired (from `claim_age`, if fully insured) or disabled (after the waiting period, if disability insured and before any retirement claim), converting at NRA.
- [x] Spouse benefits from the later of the spouse's `claim_age` (deemed filing) and the worker's entitlement, reduced for the age at spousal entitlement; a spouse caring for a child under 16 instead (unreduced, any age).
- [x] Survivors: widow(er) benefits from the later of the death month and `survivor_claim_age` (default 60); a widowed parent caring for a child under 16 before then; children while the parent is entitled or after death, to `end_age` (default 216). The deceased worker must have been entitled or fully insured at death. The survivor PIA is `deceased_worker()`'s, or, for a disabled worker, the frozen DI PIA with the regular family maximum if higher; the re-indexed guarantee applies for a death before 62.
- [x] Phase 2 rules wired in: `worker_factor` (all credits earned before death count, 20 CFR 404.313(e)(1)), dual entitlement with the person's own PIA and factor in that month.
- [x] **Ask, answered:** claiming ages (own, spousal, survivor) are given inputs. Output is nominal dollars, one row per person-month; `months = 6` gives one row a year.
- [x] Tests (`tests/testthat/test-lifetime.R`): single retiree; DI to retirement; spouse with no earnings; widow with the limit and dual entitlement; young survivor family with the guarantee; a living retiree's spouse and child in care; every row checked against direct calls.
- [x] Timing: 10,000 people (couples, 8% disabled), one row a year: 9 seconds, so about 90 seconds for 100,000.

Not covered (also in the function's documentation): the earnings test, divorce, remarriage, marriage-length requirements, disabled widow(er)s, disability recovery, a child on more than one record, currently insured status, and insured status gained after the claim month.

## Backlog from the Social Security Act audit

Decided 2026-10-08 in `docs/audits/2026-10-08-ssact-audit.md` (item numbers refer to it).

Fix now (branch `audit-fixes`):

- [x] #1 Regular family maximum for a disabled worker's family from the month the benefit converts to retirement at FRA (§203(a)(6); POMS RS 00615.742).
- [x] #5 Child's and mother's/father's benefits when the deceased worker was currently insured (§202(d), (g); §214(b)).
- [x] #14 Replace the pre-1978 quarters-of-coverage approximation with actual historical data; update the README and help pages. *Done as a `qc_history` input of actual QC counts from earnings records (Anthony's choice, 2026-10-08); the annual rule remains the fallback.*

Incorporate later, each its own phase:

- [ ] The earnings test, §203(b) and (f), with the reduction-factor adjustment at FRA, §202(q)(7).
- [ ] Divorced and surviving divorced spouses (outside the family maximum, §203(a)(3)(C)), remarriage, and marriage-length requirements (§216(b)–(g)).
- [ ] Pre-1979 computation methods.
- [ ] The lump-sum death payment, §202(i).
- [ ] Payable benefits after trust fund depletion, alongside scheduled benefits.

Noted, no change planned: #2 COLA stabilizer, #7 disabled child in care, #9 combined family maximum, #10–#13 SSA practice. Ignored: #3, #4, #6 and totalization.

## Out of scope unless the user says otherwise

Totalization, the GPO/WEP (repealed after December 2023), and benefit taxation (§86). Pre-1979 methods and the lump-sum death payment moved to the backlog above.
