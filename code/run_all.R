# Run from the repository root: Rscript code/run_all.R
for (script in c('01_registrants_by_role.R', '02_role_college_heatmap.R', '03_project_topics.R')) {
  source(file.path('code', script), local = new.env())
}
capture.output(sessionInfo(), file = 'code/sessionInfo.txt')
