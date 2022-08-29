ALL: boot
	./boot test.ml

boot: boot.c
	$(CC) -Wall -Werror -O2 -g -oboot boot.c
