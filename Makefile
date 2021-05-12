BUILD=$(PWD)/.rpmbuild

NAME=BS
RELEASE=$(shell git rev-parse --abbrev-ref HEAD)
VERSION=$(shell git rev-list HEAD --count)
RPMBUILD=rpmbuild --define "_topdir $(BUILD)" --define "_version $(VERSION)" --define "_release $(RELEASE)"

rebar='rebar3'

default: compile
all: clean compile
build: release
bs: book_store_app

bs_app:rpmbuild-tree
	@echo -- building book_store_app
	@(make; make tar; cp _build/prod/rel/bs/bs-1.0.tar.gz $(BUILD)/SOURCES/)
	$(RPMBUILD) -bb bs.spec
compile:
	@$(rebar) compile
clean:
	@echo "cleaning book store..."
	@$(rebar) clean
	@$(rebar) as prod clean
cleanall:
	@$(rebar) clean -a
	@$(rebar) as prod clean -a
test:
	@$(rebar) do ct
release:
	@$(rebar) release
shell:
	@$(rebar) shell
tar:
	@$(rebar) as prod tar


rpmbuild-tree:
	@mkdir -p $(BUILD)/SOURCES $(BUILD)/SPECS $(BUILD)/RPMS $(BUILD)/SRPMS $(BUILD)/BUILD
