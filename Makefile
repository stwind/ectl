REBAR := bin/rebar
DIALYZER := dialyzer
DIALYZER_APPS := kernel stdlib sasl inets crypto public_key ssl
GIT_SERVER := https://github.com/basho
DEPS := $(CURDIR)/deps
BIN := $(CURDIR)/bin
APP := ectl

BASIC_PLT := $(APP).plt

.PHONY: all deps clean test ct xref docs lock-deps

all: app

app: $(REBAR) deps
	@$(REBAR) compile escriptize

deps: $(REBAR) 
	@$(REBAR) get-deps

clean: $(REBAR)
	@$(REBAR) clean

ifndef SUITES
EUNIT_SUITES =
else
EUNIT_SUITES = suites=$(SUITES)
endif
test: $(REBAR) deps
	@$(REBAR) compile -D TEST
	@$(REBAR) eunit skip_deps=true $(EUNIT_SUITES)

ct: $(REBAR) app
	@$(REBAR) ct skip_deps=true

$(BASIC_PLT): build-plt

build-plt: 
	@$(DIALYZER) --build_plt --output_plt $(BASIC_PLT) --apps $(DIALYZER_APPS)

dialyze: $(BASIC_PLT)
	@$(DIALYZER) -r src deps/*/src --no_native --src --plt $(BASIC_PLT) -Werror_handling \
		-Wrace_conditions -Wunmatched_returns # -Wunderspecs

xref: $(REBAR) clean app
	@$(REBAR) xref skip_deps=true

docs: $(REBAR)
	@$(REBAR) doc skip_deps=true

lock-deps: $(REBAR) app
	@$(REBAR) lock-deps ignore=meck,proper,rebar

bin/%:
	@mkdir -p $(DEPS) $(BIN)
	git clone $(GIT_SERVER)/$*.git $(DEPS)/$*
	$(MAKE) -C $(DEPS)/$*
	cp $(DEPS)/$*/$* $(BIN)/$*
