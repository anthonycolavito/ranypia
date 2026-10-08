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

- [ ] **Widow(er)'s limit.** If the deceased worker was entitled to a reduced benefit, the widow(er)'s benefit is limited to the larger of what the worker was getting and 82.5% of the PIA. If the worker had delayed credits, the widow(er)'s base includes them. Add `worker_benefit` (the deceased's own benefit in the benefit month, or `NA` if never entitled) to `auxiliary()` for `"widow"`/`"disabled_widow"`, and apply the limit in `family_benefits()` after the age reduction. Look up and cite the governing POMS sections (the widow(er)'s limit and survivors' delayed credits, in the RS 00615 chapter) in the tests; do not cite section numbers from memory.
- [ ] **Dual entitlement.** A spouse or widow(er) entitled to their own retired or disabled benefit gets only the excess of the auxiliary benefit over their own PIA, with age reductions applied to each part separately. Add `own_pia` and `own_benefit` to `auxiliary()`. Look up the POMS dual-entitlement sections and use their worked examples as test cases, cited by section.
- [ ] **Termination ages.** Add an end month to children (18; 19 if in school; none if disabled before 22) and spouse-with-child (youngest child turns 16). Simplest form: `auxiliary(..., end_age = )` plus a helper that computes it.
- [ ] **Ask:** the earnings test, divorced spouses (paid outside the family maximum) and remarriage before 60 — include, or leave out of scope as the README says today?
- [ ] Tests: hand-worked cases for each rule, including the June-1964 widow example above ($2,656), a widow of a worker who claimed at 70, and a spouse whose own PIA exceeds half the worker's (spousal benefit $0).

## Phase 3: Family and survivor streams

Built on Phase 2. `family_benefits()` is already vectorized over families; a stream is one row per family per benefit month.

- [ ] `family_stream()`: for a living worker's family. Inputs per family: the worker (as for `benefit_stream()` or `disabled_stream()`), and a `members` data frame (`family`, `kind`, birth date, `claim_age`, `own_pia`/`own_benefit`, `end_age`). For each benefit month, compute the worker's PIA and family maximum with the worker stream, then `family_benefits()`. A spouse is paid only from the later of their claim month and the worker's entitlement month.
- [ ] `survivor_stream()`: same, after the worker's death, via `deceased_worker()` (worker never entitled or died before 62) or the worker's own stream values at death (entitled worker), `widow_guarantee_pia()` where it applies, and the Phase 2 limit.
- [ ] **Ask:** when a widow(er) is entitled to both a survivor benefit and their own, the claiming order (survivor first and switch to own at 70, or the reverse) changes lifetime benefits. Take the claiming ages as inputs, or add an option that picks the larger each month?
- [ ] Tests: a two-earner couple over a full life cycle checked by hand at four or five months (both alive, one dies, conversions); a family with two children hitting the family maximum, then a child aging out.

## Phase 4: Panel lifetime runner

The goal function. One call from a panel to person-month benefits until death.

- [ ] `lifetime_benefits(earnings, people, step = 12, policy = current_law())`, where `people` has one row per person: `id`, birth date, `claim_age`, optional `onset_year`/`onset_month` (disability), `death_age`, and `spouse_id` (or `NA`), plus optional children rows.
- [ ] For each person-month, decide the person's entitlements from those events (own retired or disabled; spouse; widow(er); child) and route to the Phase 1 to 3 streams. Return one row per person-month with `own_benefit`, `aux_benefit`, `total`, `type`, and the record it is paid on.
- [ ] Mortality stays outside the package: `death_age` comes from the user's microsimulation, so the same function serves stochastic and expected-value runs.
- [ ] **Ask:** output shape for 100,000+ people (yearly rows by default; monthly only on request; optionally summed to calendar years), and whether to return nominal dollars only (discounting and price deflation left to the user) — recommended.
- [ ] Tests: small synthetic panels covering each path (single retiree; DI to retirement; couple with one early death; widow with own benefit; young survivor family), each row checked against the Phase 1 to 3 functions called directly; a 100,000-person timing test kept out of CRAN checks.

## Out of scope unless the user says otherwise

Pre-1979 computation methods, totalization, the GPO/WEP (repealed after December 2023), lump-sum death payment, and benefit taxation (§86).
