escript: app
	@$(REBAR) escriptize skip_deps=true

include make/common.mk
include make/rebar.mk
