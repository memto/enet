REBAR ?= rebar3

all: dirs compile

dirs:
	-@mkdir -p priv/tmp

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

deps:
	@$(REBAR) get-deps

.PHONY: dialyzer typer clean distclean

dialyzer:
	@$(REBAR) dialyzer

typer: $(DEPSOLVER_PLT)
	@typer -I include --plt $(DEPSOLVER_PLT) -r ./src

distclean: clean
	@rm $(DEPSOLVER_PLT)

print-%: ; @echo $*=$($*)