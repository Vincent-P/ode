:: Incantation get all arguments except first
for /f "tokens=1,* delims= " %%a in ("%*") do set ALL_BUT_FIRST=%%b

:: -nostdlib disables libc/libc++
:: -ffreestanding disables startup code
:: -fuse-ld=lld forces clang to use lld instead of linker.exe
:: -fno-stack-check, -fno-stack-protector, -mno-stack-arg-probe disables stack checks that relies on CRT
clang.exe src/main.c -o build/%1.exe ^
	  -O0 -g -std=c11 ^
	  -isystem "C:\Program Files (x86)\Windows Kits\10\Include\10.0.22621.0\um" ^
	  -isystem "C:\Program Files (x86)\Windows Kits\10\Include\10.0.22621.0\shared" ^
	  -isystem "src/stub" ^
	  -nostdlib ^
	  -mno-stack-arg-probe ^
	  -Wall -Wextra -Wpedantic -Wconversion -Wmissing-prototypes -Wimplicit-fallthrough -Wno-c2x-extensions -Wno-unused-parameter ^
	  -DUNITY_BUILD ^
	  %ALL_BUT_FIRST% ^
	  -fuse-ld=lld -Xlinker /SUBSYSTEM:windows -lkernel32 -Xlinker /STACK:0x100000,0x100000 
