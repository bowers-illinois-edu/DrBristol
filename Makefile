# load.R fixes a bug with devtool's `help` to enable `help` on
# functions in this package, as well as loading the package
LOAD=R_PROFILE=load.R
RCMD=R -q

.PHONY:interactive
interactive:
	@$(LOAD) $(RCMD) --no-save

.PHONY:.devtools
.devtools:
	@$(RCMD) -e "devtools:::$(FUNC)($(DEVTOOLSARG))"

DEVTOOLSARG=
.PHONY:dependencies
dependencies: FUNC=install_deps
dependencies: DEVTOOLSARG=dependencies=TRUE

.PHONY: test
test: FUNC=test

.PHONY: check
check: FUNC=check

.PHONY: document
document: FUNC=document
# vignette: FUNC=build_vignettes # To be renabled if we add vignettes
# clean-vignette: FUNC=clean_vignettes
#
.PHONY: build
build: FUNC=build
dependencies test check document build: .devtools
#vignette clean-vignette: .devtools

.PHONY:clean
clean: #clean-vignette
	git clean -Xfd

.PHONY:spell-check-DESCRIPTION
spell-check-DESCRIPTION:
	aspell -c DESCRIPTION --personal=NULL
