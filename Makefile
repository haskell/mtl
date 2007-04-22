TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS =

ALL_DIRS =                  \
	Control/Monad           \
	Control/Monad/Cont      \
	Control/Monad/Error     \
	Control/Monad/RWS       \
	Control/Monad/Reader    \
	Control/Monad/State     \
	Control/Monad/Writer

PACKAGE = mtl
VERSION = 1.0.1
PACKAGE_DEPS = base

EXCLUDED_SRCS += Setup.hs

SRC_HC_OPTS += -Wall -fglasgow-exts

SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries ($(PACKAGE) package)"

include $(TOP)/mk/target.mk
