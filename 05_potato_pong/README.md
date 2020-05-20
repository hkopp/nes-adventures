A clone of pong, putting all of my skills from my previous experiments
together.

If you want to try it, load potato_pong.nes into your favorite
emulator.

If you want to compile it by yourself, you have to type the following.

```
ca65 potato_pong.s -o potato_pong.o
ld65 potato_pong.o -o potato_pong.nes -C nesfile.ini
```
