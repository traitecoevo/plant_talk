figures: fig/fitness-1.pdf

fig/fitness-1.pdf: plantmodel.R plantmodel-fun.R output/tree.output.rds
	Rscript plantmodel.R

output/tree.output.rds: simulation.R
	Rscript $<

clean:
	rm -f fig/*.pdf

.PHONY: figures clean
