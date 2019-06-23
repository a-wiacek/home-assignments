"""
Secure processor is used to cipher data using AES.
It has 32 bytes of storage. First 16 bytes store key and are write-only.
Next 16 bytes store data. On writing the last byte those 16 bytes are ciphered
and overwrite data bytes. The goal was to decipher flag ciphered using AES CBC.
The easy version of this problem allowed any amount of commands to processor
(one command is "write one byte" or "read one data byte"), the hard version
was limited to 1000 requests. Here is the solution to easy version.
"""

from pwn import *
import Crypto.Cipher.AES

server = remote('h4x.0x04.net', 31337)

def hexpad(i):
    return "%02x" % i

def write_keybyte(i, data):
    assert len(data) == 2
    server.sendline('w ' + hex(i)[2:] + ' ' + data)

def write_databyte(i, data):
    assert len(data) == 2
    server.sendline('w ' + hex(i + 16)[2:] + ' ' + data)

def write_data(data):
    assert len(data) == 32
    for i in range(16):
        write_databyte(i, data[2 * i:2 * i + 2])

def read_databyte(i):
    server.sendline('r ' + hex(i + 16)[2:])
    return server.recvline().strip('\n')

def read_data():
    string = ""
    for i in range(16):
        string += read_databyte(i)
    return string

# Compare data of SecureProc with string using as few requests as possible
def compare_with_data(string):
    assert len(string) == 32
    for i in range(16):
        d = read_databyte(i)
        if string[2 * i:2 * i + 2] != d:
            return False
    return True


server.sendline('easy')
for i in range(7):
    server.recvline()
msg = server.recvline().split(' ')[-1][:-2]
for i in range(2):
    server.recvline()
iv = msg[:32]
ciphertext = msg[32:160]

guesskey_plaintext = msg[128:160] # It is in data section already
key = ""
write_databyte(15, guesskey_plaintext[30:32])
guesskey_ciphertext = read_data()
for i in range(16):
    for j in range(256):
        j_hex = hexpad(j)
        write_keybyte(i, j_hex)
        write_data(guesskey_plaintext)
        if compare_with_data(guesskey_ciphertext):
            key += j_hex
            break
c = Crypto.Cipher.AES.new(key.decode("hex"), Crypto.Cipher.AES.MODE_CBC, iv.decode("hex"))
print(c.decrypt(ciphertext.decode("hex")).split('\n')[0])
