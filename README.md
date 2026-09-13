## New Evidence on the Effects of Randomly Assigned Reservations for Women Leaders in Indian Local Government

### Replication Materials

* [Data](data/)
	* SHRUG 2.0 can be downloaded from: https://www.devdatalab.org/shrug. 
	* MNREGA data can be downloaded from: https://doi.org/10.7910/DVN/ZHF9WC
	* UP Local Election data repository: https://github.com/in-rolls/local_elections_up
* [Scripts](scripts/)
	* Please get started with the [India Reservations.Rproj](India Reservations.Rproj) file. You can run the scripts in order.
* [Outputs](tabs/)

### Social Audit

https://mnregaweb4.nic.in/netnrega/SocialAuditFindings/SA-GPReport.aspx?page=S&lflag=eng

### Authors

Don Green, Manu Singh, and Gaurav Sood

## 🔗 Adjacent Repositories

- [in-rolls/mnrega_social](https://github.com/in-rolls/mnrega_social) — MNREGA Social Audit Data
- [in-rolls/up-2023-electoral-rolls](https://github.com/in-rolls/up-2023-electoral-rolls)
- [in-rolls/local_elections_kerala](https://github.com/in-rolls/local_elections_kerala) — Kerala Local Government Seat Reservation Data and Winner Attributes
- [in-rolls/local_elections_up](https://github.com/in-rolls/local_elections_up) — UP Local Election Data --- GP and ULB. Seat reservation, winner, and candidates for some elections
- [in-rolls/local_elections_bihar](https://github.com/in-rolls/local_elections_bihar) — Candidate Info. + Valid Votes Won by Cands. in the 2016 Bihar Panchayat Elections

Rajasthan election sources and historical manual election linkages are consumed from a pinned commit of `local_elections_rajasthan`, with SHA-256 checks in `data/manifest.yaml`. Files resolve from the versioned cache, a sibling checkout, or the pinned GitHub source. The migration preserves source values and historical links; study-specific exclusions and election-to-outcome joins remain here.


UP cross-election panels and Weaver preparations are produced in
`local_elections_up`. This repository and `quota_raj` pin the same commit and
SHA-256 values in their manifests. Adjacent-year links are built independently,
then composed through identical intermediate source-election IDs. Name
normalization preserves Hindi vowel marks and rejects conflicting numeric
sequences, including GP 45 versus GP 44. Shared raw inputs and historical fuzzy
panels are not duplicated here. `02b` adds MNREGA rollout flags and study variables;
`03b` and `04` join the resulting election histories to outcomes. Unknown
reservation is excluded after linkage. Weaver vintages remain separate upstream;
`99_narasimhan_weaver.R` retains its explicitly pinned earlier source vintage.
