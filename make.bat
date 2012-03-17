@echo off
bin\yasm -f win32 %1.asm && echo compiled
IF ERRORLEVEL 1 goto fail
link.exe /subsystem:windows /entry:main %1.obj /libpath:"C:\Program Files\Microsoft SDKs\Windows\v6.0A\Lib" /defaultlib:kernel32.lib /defaultlib:user32.lib  && echo linked
rem %CD%\%1.exe

IF ERRORLEVEL 1 goto fail
goto end
:fail

echo "FAIL"
pause

:end

rem /libpath:"C:\Program Files\Microsoft SDKs\Windows\v6.0A\Lib" /defaultlib:kernel32.lib /defaultlib:user32.lib