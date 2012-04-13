@echo off
bin\yasm -f win32 %1.asm && echo compiled
IF ERRORLEVEL 1 goto fail
rem link.exe /subsystem:console /entry:main %1.obj /libpath:"C:\Program Files (x86)\Microsoft SDKs\Windows\v7.0A\Lib" /defaultlib:Kernel32.lib /defaultlib:user32.lib  && echo linked
gcc %1.obj -m32 -o %1.exe
rem %CD%\%1.exe

IF ERRORLEVEL 1 goto fail
goto end
:fail

echo "FAIL"
echo •
pause

:end

rem /libpath:"C:\Program Files\Microsoft SDKs\Windows\v6.0A\Lib" /defaultlib:kernel32.lib /defaultlib:user32.lib