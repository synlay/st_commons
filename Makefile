PROJECT = st_commons
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

travis_ci: ci coveralls

coveralls:
	@$(REBAR) as ci coveralls send

ifeq ($(TRAVIS),true)
dialyze:
	@echo starting static code analysis with dialyzer...
	@make dialyzer_concrete
else
dialyze:
	@echo starting static code analysis with dialyzer...
	@$(REBAR) as test dialyzer
endif

distclean:
	@rm -rf _build

rebuild: distclean all

rel:
	@$(REBAR) as prod release

tar:
	@$(REBAR) as prod tar

########################################################################################################################
#                   Travis CI dialyzer support based on chef/concrete and prebuild base plts                           #
########################################################################################################################
ifeq ($(TRAVIS),true)

ERL = $(shell which erl)

ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

# For use on Travis CI, skip dialyzer for R14 and R15. Newer versions
# have a faster dialyzer that is less likely to cause a build timeout.
SKIP_DIALYZER ?= false
DIALYZER = dialyzer
R14 = $(findstring R14,$(TRAVIS_OTP_RELEASE))
R15 = $(findstring R15,$(TRAVIS_OTP_RELEASE))
ifneq ($(R14),)
DIALYZER = echo "SKIPPING dialyzer"
endif
ifneq ($(R15),)
DIALYZER = echo "SKIPPING dialyzer"
endif
ifneq ($(SKIP_DIALYZER),false)
DIALYZER = echo "SKIPPING dialyzer"
endif

DIALYZER_OPTS ?= -Wunderspecs -Wrace_conditions -Wunmatched_returns -Werror_handling --statistics

# Find all the deps the project has by searching the deps dir
ALL_DEPS = $(notdir $(wildcard _build/default/lib/*))
# Create a list of deps that should be used by dialyzer by doing a
# complement on the sets
DEPS_LIST = $(filter-out $(DIALYZER_SKIP_DEPS), $(ALL_DEPS))
# Create the path structure from the dep names
# so dialyzer can find the .beam files in the ebin dir
# This list is then used by dialyzer in creating the local PLT
DIALYZER_DEPS = $(foreach dep,$(DEPS_LIST),_build/default/lib/$(dep)/ebin)

DEPS_PLT = deps.plt

ERLANG_DIALYZER_APPS ?= asn1 \
                        compiler \
                        crypto \
                        edoc \
                        erts \
                        inets \
                        kernel \
                        mnesia \
                        public_key \
                        ssl \
                        stdlib \
                        syntax_tools \
                        tools \
                        xmerl

PROJ ?= $(notdir $(CURDIR))

# Let's compute $(BASE_PLT_ID) that identifies the base PLT to use for this project
# and depends on your `$(ERLANG_DIALYZER_APPS)' list and your erlang version

# As of OTP release 17, the OTP release number corresponds to the major part of the OTP version and the actual current
# OTP version can be read from the text file <OTP installation root>/releases/<OTP release number>/OTP_VERSION or
# <OTP source root>/OTP_VERSION.
ERLANG_VERSION := $(shell ERL_FLAGS="" $(ERL) -eval 'io:format("~p", [filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])]), halt().' -noshell | xargs cat 2>/dev/null)

ifeq ($(ERLANG_VERSION),)
# Fallback if OTP is not an installed development system or some error occurred
ERLANG_VERSION := $(shell ERL_FLAGS="" $(ERL) -eval 'io:format("~p", [filename:join([code:root_dir(), "OTP_VERSION"])]), halt().' -noshell | xargs cat 2>/dev/null)
endif

ifeq ($(ERLANG_VERSION),)
# Fallback for Erlang/OTP versions < 17 or some error occurred
ERLANG_VERSION := $(shell ERL_FLAGS="" $(ERL) -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().' -noshell)
endif

MD5_BIN := $(shell which md5 || which md5sum)
ifeq ($(MD5_BIN),)
# neither md5 nor md5sum, we just take the project name
BASE_PLT_ID := $(PROJ)
else
BASE_PLT_ID := $(word 1, $(shell echo $(ERLANG_DIALYZER_APPS) | $(MD5_BIN)))
endif

ifeq ($(TRAVIS),true)
## If we're running on travis, pull the plt from S3
## We got them from https://github.com/esl/erlang-plts
## To add to the collection make sure they're public and match the erlang version
## reported by make otp_version
## s3cmd put --acl-public --guess-mime-type <FILENAME> s3://concrete-plts

BASE_PLT := travis-erlang-$(TRAVIS_OTP_RELEASE).plt
BASE_PLT_URL := http://s3.amazonaws.com/concrete-plts/$(BASE_PLT)
else
BASE_PLT := ~/.cache/rebar3/.concrete_dialyzer_plt_$(BASE_PLT_ID)_$(ERLANG_VERSION).plt
endif

DIALYZER_SRC = -r _build/ci+test/lib/$(PROJECT)/ebin

# Only include local PLT if we have deps that we are going to analyze
ifeq ($(strip $(DIALYZER_DEPS)),)
dialyzer_concrete: $(BASE_PLT)
	@$(DIALYZER) $(DIALYZER_OPTS) --plts $(BASE_PLT) $(DIALYZER_SRC)
else
dialyzer_concrete: $(BASE_PLT) $(DEPS_PLT)
	@$(DIALYZER) $(DIALYZER_OPTS) --plts $(BASE_PLT) $(DEPS_PLT) $(DIALYZER_SRC)

$(DEPS_PLT):
	@$(DIALYZER) --build_plt $(DIALYZER_DEPS) --output_plt $(DEPS_PLT)
endif

$(BASE_PLT):
ifeq ($(TRAVIS),true)
		@echo "Attempting to download PLT: $(BASE_PLT_URL)."
		-wget $(BASE_PLT_URL)

		@if [ -f $(BASE_PLT) ] ; then \
			echo "Downloaded PLT successfully to $(BASE_PLT)" ; \
		else \
			echo "Download failed. Please wait while a new PLT is compiled." ; \
			$(DIALYZER) --build_plt --apps $(ERLANG_DIALYZER_APPS) --output_plt $(BASE_PLT) ; \
			echo "now try your build again" ; \
		fi;

else
		@echo "Missing $(BASE_PLT). Please wait while a new PLT is compiled."
		$(DIALYZER) --build_plt --apps $(ERLANG_DIALYZER_APPS) --output_plt $(BASE_PLT)
		@echo "now try your build again"
endif

endif

.PHONY: all compile test dialyze distclean rebuild rel tar dialyzer_concrete ci travis_ci coveralls
