.PHONY: build clean deploy site
all: build

#############
# Variables #
#############

STACK_LOCAL_INSTALL_PATH = $(shell stack path --local-install-root)

SITE_PROG_PATH = $(STACK_LOCAL_INSTALL_PATH)/site

################################
## Targets for specific files ##
################################

$(SITE_PROG_PATH): site.hs
	@echo "Building..."
	@stack build
	@echo "Built."

#####################
## General targets ##
#####################

build: $(SITE_PROG_PATH)

clean:
	@echo "Cleaning..."
	-@stack exec -- site clean 2>/dev/null || true
	@rm -rf _cache/ _site/
	@stack clean
	@echo "Clean."

deploy:

site: $(SITE_PROG_PATH) 
	# We don't actually need to use rebuild here, we could just use build.
	stack exec -- site rebuild

watch: $(SITE_PROG_PATH)
	stack exec -- site watch --host 0.0.0.0
