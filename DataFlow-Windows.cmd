@echo off
setlocal

where powershell.exe >nul 2>&1
if errorlevel 1 (
  echo [ERROR] Windows PowerShell was not found.
  echo Install PowerShell or run the R pipeline directly with Rscript.
  pause
  exit /b 1
)

powershell.exe -NoLogo -NoProfile -ExecutionPolicy Bypass -File "%~dp0DataFlow.ps1" -Interactive
set "DATAFLOW_STATUS=%ERRORLEVEL%"

echo.
pause
exit /b %DATAFLOW_STATUS%
