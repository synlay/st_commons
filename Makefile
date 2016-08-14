REBAR=rebar3

.PHONY: all compile test dialyze distclean rebuild rel tar

all: compile test dialyze

compile:
	@$(REBAR) compile

ifeq ($(CI),true)
test:
	@$(REBAR) as ci do eunit, cover
else
test:
	@$(REBAR) as dev_console do eunit, cover --verbose
endif

ci: test dialyze

dialyze:
	@echo starting static code analysis with dialyzer...
	@$(REBAR) as test dialyzer

distclean:
	@rm -rf _build

rebuild: distclean all

rel:
	@$(REBAR) as prod release

tar:
	@$(REBAR) as prod tar
