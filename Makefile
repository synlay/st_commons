REBAR ?= $(shell which rebar3)

ifeq ($(REBAR),)
REBAR = $(CURDIR)/rebar3
endif

REBAR_URL=https://s3.amazonaws.com/rebar3/rebar3

all: $(REBAR) compile test dialyze

$(REBAR):
	curl -Lo rebar3 $(REBAR_URL) || wget $(REBAR_URL)
	chmod a+x rebar3
	./rebar3 update

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

.PHONY: all compile test dialyze distclean rebuild rel tar
