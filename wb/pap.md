# West Bengal replication and exploratory public-goods extension

Written before running the outcome regressions in this folder. This is a
retrospective replication and an exploratory extension, not a registered or
blind confirmatory study. Published coefficients, source-level distributions,
and the source auditor's treatment-group means for the 1998 replication were
already inspected. No 2003 outcome-on-treatment estimate informed this plan.

## Units, contrasts and interpretation

1. Replicate selected Tables 1 and 7 of Chattopadhyay and Duflo, NBER w8615,
   using the released archive. Unit: GP for officeholder/GP facility outcomes;
   two randomly sampled villages per GP for village outcomes. Population:
   161 non-pretest Birbhum GPs. Exposure: 1998 women's Pradhan reservation.
   Contrast: unweighted reserved-minus-unreserved means, 1998 to summer 2000.
   GP assignment implies GP-clustered CR2 for repeated village observations;
   GP-level models use HC2. Legacy classical/CR0/CR1 calculations are separate
   comparisons, not claimed to reproduce an undocumented Moulton implementation.
2. Explore whether 2003 Pradhan quotas predict public goods reported since
   2003 by current officeholders in the follow-up survey. Population: named
   Birbhum research GPs with explicit 2003 year codes, an observed current
   Pradhan survey, consistent survey GP identifiers and a 2006 visit year.
   One observation per GP, equal weight. Outcomes: tubewells/handpumps built
   plus repaired, metal-road kilometers built plus repaired, new informal
   education centers (SSKs). Each is estimated separately with its available
   cases; no missing observation becomes an unreserved office or zero outcome.
3. Primary exploratory extension is an unadjusted quota03 association. The
   adjusted model adds prior women/SC/ST reservations, current SC/ST quotas,
   and block fixed effects, requiring explicit 1998 history without a detected
   source conflict. A separate model tests the prior women's quota conditional
   on current quotas in that same adjusted specification.

The original reduced form concerns reservation, not only leader gender:
experience, candidate selection and competition may also change. For 2003,
rotation creates dependence across years, SC/ST strata may change, and a survey
of surviving current officeholders may differ from original election winners.
Block fixed effects do not establish random assignment. These extensions are
reported as associations with model-based intervals unless contemporary
assignment rosters and selection assumptions can be established.

## Outcomes and fixed recodes

- Table 1: female Pradhan = prsex 2 versus 1; treatment = womres 1 versus 2.
- Table 7 village water: sum wwprrt, wwprbt, wwpurt, wwpubt, twprrt, twprbt,
  twpurt, twpubt, twgprt, twgpbt. All components must be observed. This literal
  reconstruction does not exactly match printed means; do not optimize it.
- Village roads: report good-or-moderate (vroad 1 or 2) and good-only (1)
  separately. The former matches rounded printed means but the label differs.
- Formal school buildings: pspurt, pspubt, pskurt, pskubt, secsrt, secsbt;
  the eight-field variant adding psnbrt/psnbbt is an explicitly labeled
  sensitivity, because schools without a building conflict with the row label.
- GP water: gtubb 1/2. GP metal road: gmetb or gmetr equals 1; both 2 means 0.
- GP SSK presence: gssk 1/2, a different construct from formal-school buildings.
- 2003 water: F3_4a_q + F3_4b_q, gated by F3_4a/F3_4b.
- 2003 roads: F3_11a_q + F3_11b_q, gated by F3_11a/F3_11b.
- 2003 new SSK: F2_8b, gated by F2_8. Current absence of an SSK alone
  does not prove zero creation since2003; unanswered histories remain missing.
- A No gate and a missing/zero amount is a documented structural zero. A No
  gate plus a positive amount is a contradiction, not a zero. A Yes gate
  with missing/999/-999 amount remains missing. Never recode 99 globally.

## Samples and checks fixed before estimation

Use all 161 eligible GPs for the paper replication; separately show the
158-GP source-conflict sensitivity. Village selection is prvill=="NO", not
villnum<=2. Verify A:B and C:D one-to-one joins and 498:166 village-to-GP joins.
For 2003 exclude Gonpur's conflicting numeric identifiers in the main sample;
retain the raw row and show a sensitivity allowing its interview-index link.
Keep Ayas and Rudranagar year exceptions outside the explicit-2003 sample.
Report exact-name matches as an identity sensitivity, never infer identity from
reservation patterns. Use complete cases separately and report sample counts
and missingness by quota group and block. Show an untrimmed count outcome as
primary; no outcome-driven winsorization or post hoc subgroup selection.

## Inference, multiplicity and expectations

Use estimatr::lm_robust with explicit HC2 at GP level and CR2 clustered on GP
at village level. Report 95% intervals, treated-unit counts and an SE ladder
(classical, HC1/HC2/HC3 or CR0/CR1/CR2 as appropriate). Three public-goods
outcomes form one family within each extension specification; report Holm
adjustment, a conservative familywise procedure valid under dependence.
No unrestricted permutation is presented as exact randomization inference:
the original strata/roster mechanism is not recovered.

The prior paper motivates positive water/road contrasts and potentially
negative education contrasts; no sign restriction is imposed. For exploratory
extensions, small-to-moderate standardized effects (0, 0.1, 0.3 SD) are design
scenarios, not claimed prior facts. Report an approximate MDE and design-based
simulation only if the assignment mechanism is recovered; otherwise label
normal-approximation power/coverage exercises as model-based diagnostics.
No causal placebo claim is made from a later quota predicting 2000 outcomes,
because mechanical rotation can connect the two. Long-run MNREGA/SHRUG effects
require a verified historical GP-to-official-code crosswalk and are not inferred
by matching raw names to present-day geography.
