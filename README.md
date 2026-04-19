# Lamp
### Introduction
- Lamp is a very simple programming language based on lambda calculus.
- I said "based on", but it's just lambda calculus with little bit of low-level features.
- This language will be used to make an operating system.
- The cabal project in this repository is the source of Lamp-to-C converter. (I have no knowledge to make a compiler...)
- Check test.lmp and prelude.lmp for usage. I test converter using that file.
### etc.
- The builded executable, header.c, and files being included by #incld should placed in the same directory. (Not in cabal project. This will be changed soon)