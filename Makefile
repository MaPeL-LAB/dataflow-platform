.PHONY: setup setup-optional list example test clean

setup:
	Rscript scripts/bootstrap.R

setup-optional:
	Rscript scripts/bootstrap.R --with-optional

list:
	Rscript scripts/list_pipelines.R

example:
	Rscript scripts/run_pipeline.R --pipeline data_dictionary --input examples/data --output outputs/example --overwrite true

test:
	Rscript scripts/run_tests.R

clean:
	rm -rf outputs/*
	touch outputs/.gitkeep
