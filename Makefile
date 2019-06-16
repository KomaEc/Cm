MAKE_DIR = $(PWD)
LUA_DIR := $(MAKE_DIR)/backend/lua-5.1.4

PLAT = none

all:
	dune build ./bin/cmc.exe;
	make $(PLAT) -C $(LUA_DIR)
clean:
	dune clean
	make clean -C $(LUA_DIR)