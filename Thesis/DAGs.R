library(dagitty)

rq12.dag = dagitty("dag {
  Domain -> AUCECEXAM
  Domain -> Faults
  Project -> Commits
  Project -> Language
  Project -> Domain
  Project -> Faults
  Project -> LOC
  Project -> AUCECEXAM
  Language -> AUCECEXAM
  Language -> Faults
  LOC -> AUCECEXAM
  LOC -> Faults
  Origin -> LOC
  Origin -> Faults
  Origin -> AUCECEXAM
  Time -> AUCECEXAM
  Weight -> AUCECEXAM
  Faults -> AUCECEXAM
}")

rq4.dag = dagitty("dag {
  Domain -> AUCECEXAM
  Domain -> Faults
  Project -> Commits
  Project -> Language
  Project -> Domain
  Project -> Faults
  Project -> LOC
  Project -> AUCECEXAM
  Language -> AUCECEXAM
  Language -> Faults
  LOC -> AUCECEXAM
  LOC -> Faults
  Origin -> LOC
  Origin -> Faults
  Origin -> AUCECEXAM
  Time -> AUCECEXAM
  Weight -> AUCECEXAM
  Faults -> AUCECEXAM
  Algorithm -> AUCECEXAM
}")

impliedConditionalIndependencies(rq12.dag)


impliedConditionalIndependencies(rq4.dag)
