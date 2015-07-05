IMAGE = bayesian_mcmc

.PHONY : \
	build \
	run \
	repl \
	rmi \
	shell \
	test

##
# Build the Docker container with R and Two library dependencies.
build :
	docker build -t $(IMAGE) .

##
# Remove the Docker container.
rmi :
	docker rmi $(IMAGE)

##
# Execute the script
run :
	docker run -t --rm -v $$(pwd):/usr/src/app -w /usr/src/app \
		$(IMAGE) \
		Rscript bayesian_mcmc.R

##
# Tests the library.
test :
	docker run -t --rm -v $$(pwd):/usr/src/app -w /usr/src/app \
		$(IMAGE) \
		Rscript tests/testthat.R

##
# Inspect the Docker container.
shell :
	docker run -it --rm -v $$(pwd):/usr/src/app -w /usr/src/app \
		$(IMAGE) \
		bash

##
# R playground.
repl :
	mkdir -p output
	docker run -it --rm -v $$(pwd):/usr/src/app -w /usr/src/app \
		$(IMAGE)
