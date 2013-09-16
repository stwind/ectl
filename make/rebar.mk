BASE_DIR := $(shell pwd)
REBAR_REPO := https://github.com/rebar/rebar.git
REBAR ?= $(BASE_DIR)/rebar
DEPS ?= deps
 
$(REBAR):
	@mkdir -p $(DEPS)
	@[ ! -d $(DEPS)/rebar ] && git clone $(REBAR_REPO) $(DEPS)/rebar; $(MAKE) -C $(DEPS)/rebar
	@cp $(DEPS)/rebar/rebar .
 
rebar: $(REBAR)
