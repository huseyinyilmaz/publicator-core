DIALYZER = dialyzer
REBAR = ./rebar
MNESIA_DIR = /tmp/mnesia
NODE_NAME = publicator@127.0.0.1

#REBAR_URL = https://raw.github.com/wiki/rebar/rebar/rebar
REBAR_URL = https://github.com/rebar/rebar/wiki/rebar

APPS =   kernel stdlib crypto webtool mnesia eunit tools os_mon runtime_tools xmerl inets

.PHONY : all get-deps get-rebar configure compile clean test eunit ct build-erlang-plt build-plt dialyze docs start blackbox

# compile
all: compile

get-rebar:
	rm -f rebar
	wget $(REBAR_URL)
	chmod a+x rebar

# get dependencies
get-deps:
	@$(REBAR) get-deps


configure: get-rebar get-deps

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	if [ -d "test" ]; then rm -f test/*.beam; fi
	rm -f erl_crash.dump

test: clean compile ct

eunit:
	@$(REBAR) eunit skip_deps=true

ct:
	@$(REBAR) ct skip_deps=true

build-erlang-plt:
	@$(DIALYZER) --build_plt --output_plt .erlang_dialyzer.plt \
		--apps erts kernel stdlib ssl crypto

build-plt: compile
	@$(DIALYZER) --build_plt  --add_to_plt --plt .erlang_dialyzer.plt\
		--output_plt .deps_dialyzer.plt \
		--apps deps/* $(APPS)

dialyze:
	@$(DIALYZER) --src lib/*/src --plt .deps_dialyzer.plt\
		-Werror_handling \
		-Wrace_conditions -Wunmatched_returns \
		| grep -v -f ./.dialyzer-ignore-warnings
# -Wunderspecs
docs:
	@$(REBAR) doc skip_deps=true

# start for development
start: compile
	erl -pa ebin deps/*/ebin \
	    -i  include deps/*/include \
	    -config default_sys.config \
	    -lager handlers '[{lager_console_backend, debug}]' \
	    -mnesia dir '"$(MNESIA_DIR)"' \
	    -name $(NODE_NAME) \
	    -s lager \
	    -s publicator_core



blackbox:
	rm -f publicator_test
	cd devutils/publicator_test/; ../../rebar compile escriptize;mv publicator_test ../../
	./publicator_test
