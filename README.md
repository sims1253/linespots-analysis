# Linespots Analysis Repository

This repository holds the empirical analysis for the Linespots fault-prediction algorithm.
The Linespots reference implementation and evaluation code can be found [here](https://gitlab.com/sims1253/linespots-lib/).

## An empirical study of Linespots: A novel past-fault algorithm
The analysis reports for the recent *An empirical study of Linespots: A novel past-fault algorithm* paper can be found here:
- [Predictive Perfomance](Paper/Report/RQ1_analysis_report.html)([Rmd](Paper/Report/RQ1_analysis_report.Rmd))
- [Runtime](Paper/Report/RQ2_analysis_report.html)([Rmd](Paper/Report/RQ2_analysis_report.Rmd))

The csv file with the evaluation output, in case you want to run your own analysis from the ground up can be found [here](Paper/data/full_evaluation.csv) or can be replicated using the [docker image](https://github.com/sims1253/linespots-docker).

## Master Thesis

My master thesis about Linespots: [https://hdl.handle.net/20.500.12380/300418](https://hdl.handle.net/20.500.12380/300418).
There is no analysis report and no reproducability package per se but the evaluation data can be found [here](Thesis/data.csv).

While not perfect, this should mostly reproduce the results:
- Clone [https://gitlab.com/sims1253/linespots-lib](https://gitlab.com/sims1253/linespots-lib) and checkout the `sims/evaluation` branch.
- Run `pipenv install` in the repository
- Run `pipenv run project_getter.py` from the `linespots/utils` directory
- Run `pipenv run json_builder.py` from the `linespots/utils` directory
- Change the name of the json config in `full-evaluation.py`(line 44) to `final-config.json`
- Run `pipenv run full-evaluation.py` from the top-level directory of the repository
- This should create a `full_evaluation.csv` file in the `evaluation_projects` directory

I am aware that this process is error prone (and won't reproduce the exact results due to how I calculated commit ranges) but it I do not intend to fix this. For an up to date analysis see the paper above.

## Bachelor Thesis

The beginning of the Linespots Journey.
Published [here](https://www.tuhh.de/sts/research/publications/student-theses.html#c105916)([pdf](https://www.sts.tuhh.de/pw-and-m-theses/2016/scholz16.pdf)).
I never did a proper statistical analysis for this but the code is available [here](https://gitlab.com/sims1253/Linespots) and the repository contains some instructions on how to reproduce what I did.