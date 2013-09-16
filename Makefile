escript: app
	@$(REBAR) escriptize skip_deps=true

include make/rebar.mk
include make/common.mk
