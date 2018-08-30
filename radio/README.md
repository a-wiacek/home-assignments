Internet radio: sender and receiver.

Sender reads data from stdin and broadcasts it.

Receiver takes data sent by sender and puts in to stdout.
Receiver also has simple, text UI (connect with Telnet). Here one can change radio station receiver is listening to.

Sender options (copied from header file):
* obligatory parameter -a: send address (dots notation)
* -P: data port (default: 26349)
* -C: control port (default: 36349)
* -p: pack size [B] (default: 512)
* -f: FIFO queue size [B] (default: 131072)
* -R: retransmission time [ms] (default: 250)
* -n: name (default: Nienazwany Nadajnik)

Receiver options (copied from header file):
* -d: discover address (default: 255.255.255.255)
* -P: data port (default: 26349)
* -C: control port (default: 36349)
* -U: user interface port (default: 16349)
* -b: buffer size [B] (default: 65536)
* -R: retransmission time [ms] (default: 250)
* -n: preferred radio station name (no default value)
