TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS = 

ALL_DIRS = \
	Control/Monad

PACKAGE = mtl
VERSION = 1.0
PACKAGE_DEPS = base

SRC_HC_OPTS += -fglasgow-exts

SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries ($(PACKAGE) package)"

include $(TOP)/mk/target.mk
