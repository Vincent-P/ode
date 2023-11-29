:: Incantation get all arguments except first
for /f "tokens=1,* delims= " %%a in ("%*") do set ALL_BUT_FIRST=%%b

:: -nostdlib disables libc/libc++
:: -ffreestanding disables startup code
:: -fuse-ld=lld forces clang to use lld instead of linker.exe
:: -fno-stack-check, -fno-stack-protector, -mno-stack-arg-probe disables stack checks that relies on CRT
clang.exe src/main.c -o build/%1.exe -g -DUNITY_BUILD -nostdlib -ffreestanding -fuse-ld=lld -mno-stack-arg-probe -std=c11 -Wall -Wextra -Wpedantic -Wconversion -Wno-c2x-extensions %ALL_BUT_FIRST% -lkernel32 -Xlinker /STACK:0x100000,0x100000 
