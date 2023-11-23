@echo off
:: Incantation get all arguments except first
for /f "tokens=1,* delims= " %%a in ("%*") do set ALL_BUT_FIRST=%%b

:: /Zi Enables debug information
:: /GS- Disable Buffer Security Check (that relies on the CRT).
:: /Gs1000000000 Set the threshold for the control stack check calls stupidly high. Checks rely on the CRT for __chkstk.
:: /link will pass all remaining arguments to the linker.
:: /nodefaultlib disables all libs by default (including the CRT)
cl.exe ../src/main.cpp /Fe:%1.exe /std:c++20 /permissive- /DUNITY_BUILD /Zi /GS- /Gs1000000000 /W4 %ALL_BUT_FIRST% /link /nodefaultlib /subsystem:console kernel32.lib /STACK:0x100000,0x100000
