# Lamp
### Introduction
- Lamp is a very simple programming language based on lambda calculus.
- I said "based on", but it's just lambda calculus with little bit of low-level features.
- This language will be used to make an operating system.
### Features
#### ` (backticks)
- This allows us to use real integers, not church integers.
- For example, \`d25 is 25 in decimal, \`h3f is 3f in hexadecimal.
- This also allows us to use primitive functions such as readMem, writeMem.
### Examples
- Handle structure:
```
-- get struct addr, member off, and size then return value
    readMember = \s .\m .\n .`readMem (`+ s m) n;

-- get struct addr, member off, value, and size then write value
    writeMember = \s .\m .\r .\n .`writeMem (`+ s m) r n;

-- get struct addr then return value of member 'size'
    GDTR.size = \s .readMember s `h0 `h2;
-- get struct addr then return value of member 'offs'
    GDTR.offs = \s .readMember s `h2 `h8;
-- read
    GDTR.size= = \r .\s .writeMember s `h0 r `h2;
-- read
    GDTR.offs= = \r .\s .writeMember s `h2 r `h8;

-- write 23 to GDTR.size
-- `alloc n makes space of n bytes in .data then return its addr
    GDTR.size= `d23 (`alloc `d10);
```