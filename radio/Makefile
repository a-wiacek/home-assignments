TARGET: sikradio-sender sikradio-receiver

CC	= gcc
CFLAGS	= -O2 -Wall
LIBS	= -lpthread

sikradio-sender: sender.c sender.h const.h err.c
	$(CC) $(CFLAGS) $(LIBS) $^ -o $@

sikradio-receiver: receiver.c receiver.h const.h err.c
	$(CC) $(CFLAGS) $(LIBS) $^ -o $@

.PHONY: clean TARGET
clean:
	rm -f sikradio-sender sikradio-receiver *.o *~ *.bak
