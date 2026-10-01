@echo off

:: If first argument is :main, skip wrapper
if "%~1"==":main" goto main

set LOGFILE=%~dp0run_%date:~-4,4%%date:~-10,2%%date:~-7,2%_%time:~0,2%%time:~3,2%.log

powershell -NoProfile -Command ^
  "& { cmd /c '"%~f0" :main' | Tee-Object -FilePath '%LOGFILE%' }"

exit /b


:main
shift
setlocal enabledelayedexpansion

rem =========================================================
rem Interactive control flag
rem =========================================================
set RUNALL=0

echo.
echo ======================================
echo Enter Week Identifier (YYYYMMDD)
echo ======================================
set /p NEWEST=Week ID: 

if not exist "DataExternal\%NEWEST%" (
    echo.
    echo ERROR: Folder DataExternal\%NEWEST% does not exist!
    echo Please check the week identifier and try again.
    pause
    exit /b 1
)

echo Using folder: %NEWEST%

rem =========================================================
rem 1. InputUpdates
rem =========================================================
call :AskRun "Section 1 - InputUpdates"
if errorlevel 1 goto skip1

call :logEcho ======================================
call :logEcho Running InputUpdates.py
call :logEcho ======================================

python InputUpdates.py "%NEWEST%"
if errorlevel 1 (
echo ERROR: InputUpdates.py failed!
pause
exit /b 1
)

set INPUTFILE=DataExternal\%NEWEST%\InputUpdates_%NEWEST%.txt

if not exist "%INPUTFILE%" (
call :logEcho ERROR: InputUpdates file not found!
exit /b 1
)

call :logEcho Reading dates from InputUpdates file
set "FORE="
set "END="

for /f "usebackq tokens=1,* delims==" %%A in ("%INPUTFILE%") do (
    set "KEY=%%A"
    set "VALUE=%%B"

    set "KEY=!KEY: =!"
    set "VALUE=!VALUE: =!"

    if /I "!KEY!"=="FORE_START_DATE" set "FORE=!VALUE!"
    if /I "!KEY!"=="END_DATE" set "END=!VALUE!"
)

call :ConvertDate %FORE% FORE_ISO
call :ConvertDate %END% END_ISO

:skip1
call :logEcho Skipping or completed Section 1

rem =========================================================
rem 2. Preprocessor
rem =========================================================
set "RUN_D=0"

call :AskRun "Section 2 - Preprocessor"
if errorlevel 1 goto skip2

call :logEcho Running Preprocessor workflow

cd Preprocessor
set "BASE=..\DataExternal\%NEWEST%"

call :RunOMRI "-5000" A
call :RunOMRI "-3500" B
call :RunOMRI "-2000" C

set "D_FILE="

for %%F in ("%BASE%\*_OMRI *.csv") do (
    if exist "%%F" (
        echo %%~nxF | findstr /E /C:"_OMRI -5000.csv" /C:"_OMRI -3500.csv" /C:"_OMRI -2000.csv" >nul
        if errorlevel 1 set "D_FILE=%%F"
    )
)

if defined D_FILE (
    set "RUN_D=1"
    call :RunOMRIFile "!D_FILE!" D
) else (
    call :logEcho No Scenario D OMRI file found
)

python ForecastDuplicate_STN.py --forecast "Y" -f ..\Input\timeseries\forecast.dss

cd ..

:skip2
call :logEcho Skipping or completed Section 2

rem =========================================================
rem 3. Hydro binaries
rem =========================================================
call :AskRun "Section 3 - Hydro binaries"
if errorlevel 1 goto skip3

call :logEcho Running hydro binaries

.\bin\hydro .\Input\hydroA.inp || exit /b 1
.\bin\hydro .\Input\hydroB.inp
.\bin\hydro .\Input\hydroC.inp
if "!RUN_D!"=="1" (
.\bin\hydro .\Input\hydroD.inp
) else (
    call :logEcho Skipping Scenario D hydro
)

:skip3
call :logEcho Skipping or completed Section 3

rem =========================================================
rem 4. PTM NP
rem =========================================================
call :AskRun "Section 4 - PTM NP"
if errorlevel 1 goto skip4

set "PTM=.\bin\ptm.bat"
call :RunPTMNP || goto :fail_NP

:skip4
call :logEcho Skipping or completed Section 4

rem =========================================================
rem 5. PTM PP
rem =========================================================
call :AskRun "Section 5 - PTM PP"
if errorlevel 1 goto skip5

set "PTM=.\bin\ptm.bat"
call :RunPTMPP || goto :fail_PP

:skip5
call :logEcho Skipping or completed Section 5

rem =========================================================
rem 6. PTM SP
rem =========================================================
call :AskRun "Section 6 - PTM SP"
if errorlevel 1 goto skip6

set "PTM=.\bin\ptm.bat"
call :RunPTMSP || goto :fail_SP

:skip6
call :logEcho Skipping or completed Section 6

rem =========================================================
rem 7. NoPumping
rem =========================================================
call :AskRun "Section 7 - NoPumping"
if errorlevel 1 goto skip7

python "..\Model_NOPUMP\NoPumping.py" || exit /b 1

:skip7
call :logEcho Skipping or completed Section 7

rem =========================================================
rem 8. Hydro NP
rem =========================================================
call :AskRun "Section 8 - Hydro NP"
if errorlevel 1 goto skip8

pushd "..\Model_NOPUMP" || (
    echo ERROR: Could not change to Model_NOPUMP folder
    exit /b 1
)

.\bin\hydro ".\Input\hydroA.inp" || (
    popd
    exit /b 1
)

.\bin\hydro ".\Input\hydroB.inp"
.\bin\hydro ".\Input\hydroC.inp"
if "!RUN_D!"=="1" (
.\bin\hydro ".\Input\hydroD.inp"
) else (
    call :logEcho Skipping Scenario D NoPumping hydro
)

popd

:skip8
call :logEcho Skipping or completed Section 8

rem =========================================================
rem 9. Combine CSV
rem =========================================================
call :AskRun "Section 9 - Combine CSV"
if errorlevel 1 goto skip9
pushd "..\Model" || (
    echo ERROR: Could not change to Model folder
    exit /b 1
)

python combine_sp_csv.py -fs "%NEWEST%" || exit /b 1

:skip9
call :logEcho Skipping or completed Section 9

rem =========================================================
rem 10. ZOI preprocessing
rem =========================================================
call :AskRun "Section 10 - ZOI preprocessing"
if errorlevel 1 goto skip10
pushd "..\ZOI" || (
    echo ERROR: Could not change to ZOI folder
    exit /b 1
)

python DSM2_DEZOI_realtime_3weeks_parallel_combined.py -fs "%NEWEST%" || exit /b 1

:skip10
call :logEcho Skipping or completed Section 10

rem =========================================================
rem 11. ZOI spatial plots
rem =========================================================
call :AskRun "Section 11- ZOI spatial plots"
if errorlevel 1 goto skip11
pushd "..\ZOI" || (
    echo ERROR: Could not change to ZOI folder
    exit /b 1
)
python PyRscript.py %NEWEST% || exit /b 1

:skip11
call :logEcho Skipping or completed Section 11

rem =========================================================
rem 12. ZOI channel length charts
rem =========================================================
call :AskRun "Section 12- ZOI channel length charts"
if errorlevel 1 goto skip12
pushd "..\ZOI" || (
    echo ERROR: Could not change to ZOI folder
    exit /b 1
)

python PyRscript_channellength.py %NEWEST% || exit /b 1

:skip12
call :logEcho Skipping or completed Section 12

rem =========================================================
rem 13. PTM Plots
rem =========================================================
call :AskRun "Section 13 - PTM Plots"

call :logEcho ======================================
call :logEcho Running PTM plotting script
call :logEcho ======================================

if errorlevel 1 goto skip13
pushd "..\Model" || (
    echo ERROR: Could not change to Model folder
    exit /b 1
)

python plot_ptm_particles.py %NEWEST% || exit /b 1

:skip13
call :logEcho Skipping or completed Section 13

rem =========================================================
rem 14. LFS Plots
rem =========================================================
call :AskRun "Section 14 - LFS Plots"

call :logEcho ======================================
call :logEcho Running LFS plotting script
call :logEcho ======================================

if errorlevel 1 goto skip14
pushd "..\Model" || (
    echo ERROR: Could not change to Model folder
    exit /b 1
)

python plot_ptm_lfs.py %NEWEST% || exit /b 1

:skip14
call :logEcho Skipping or completed Section 14

echo DONE!
pause
goto :eof

rem =========================================================
rem Functions
rem =========================================================

:AskRun
if "%RUNALL%"=="1" exit /b 0

echo.
echo ======================================
echo Run %1 ?
echo   Y = Yes
echo   N = Skip
echo   A = Run all remaining
echo ======================================

choice /c YNA /n

if errorlevel 3 (
    set RUNALL=1
    exit /b 0
)
if errorlevel 2 exit /b 1
exit /b 0


:ConvertDate
:: %1 = DDMonYYYY
:: %2 = output variable
set DATE=%1
set DAY=%DATE:~0,2%
set MON=%DATE:~2,3%
set YEAR=%DATE:~5,4%
if "%MON%"=="Jan" set MM=01
if "%MON%"=="Feb" set MM=02
if "%MON%"=="Mar" set MM=03
if "%MON%"=="Apr" set MM=04
if "%MON%"=="May" set MM=05
if "%MON%"=="Jun" set MM=06
if "%MON%"=="Jul" set MM=07
if "%MON%"=="Aug" set MM=08
if "%MON%"=="Sep" set MM=09
if "%MON%"=="Oct" set MM=10
if "%MON%"=="Nov" set MM=11
if "%MON%"=="Dec" set MM=12
set %2=%YEAR%-%MM%-%DAY%
exit /b

:RunOMRI
set SUFFIX=%~1
set SD=%2
for %%F in ("%BASE%\*_OMRI %SUFFIX%.csv") do (
    if exist "%%F" (
        call :logEcho Running %%~nxF  (Scenario %SD%)
        python CSVToDSM2_pyhecdss.py ^
            -c "%%F" ^
            -f "..\Input\timeseries\forecast.dss" ^
            -d "%BASE%\dicu.dss" ^
            -fs %FORE_ISO% ^
            -fe %END_ISO% ^
            -sd %SD%
        exit /b
    )
)
call :logEcho WARNING: No OMRI file found for suffix %SUFFIX%
exit /b

:RunOMRIFile
set "OMRI_FILE=%~1"
set "SD=%~2"

if not exist "%OMRI_FILE%" (
    call :logEcho ERROR: OMRI file not found: %OMRI_FILE%
    exit /b 1
)

call :logEcho Running %~nx1  (Scenario %SD%)

python CSVToDSM2_pyhecdss.py ^
    -c "%OMRI_FILE%" ^
    -f "..\Input\timeseries\forecast.dss" ^
    -d "%BASE%\dicu.dss" ^
    -fs %FORE_ISO% ^
    -fe %END_ISO% ^
    -sd %SD%

exit /b

:RunPTMNP
rem -----------------------
rem NP PTM runs
rem -----------------------

call "%PTM%" ".\Input\PTM\np\ptmA_350.inp"
call "%PTM%" ".\Input\PTM\np\ptmA_469.inp"
call "%PTM%" ".\Input\PTM\np\ptmA_465.inp"
call "%PTM%" ".\Input\PTM\np\ptmA_99.inp"

call "%PTM%" ".\Input\PTM\np\ptmB_350.inp"
call "%PTM%" ".\Input\PTM\np\ptmB_469.inp"
call "%PTM%" ".\Input\PTM\np\ptmB_465.inp"
call "%PTM%" ".\Input\PTM\np\ptmB_99.inp"

call "%PTM%" ".\Input\PTM\np\ptmC_350.inp"
call "%PTM%" ".\Input\PTM\np\ptmC_469.inp"
call "%PTM%" ".\Input\PTM\np\ptmC_465.inp"
call "%PTM%" ".\Input\PTM\np\ptmC_99.inp"

if "!RUN_D!"=="1" (
call "%PTM%" ".\Input\PTM\np\ptmD_350.inp"
call "%PTM%" ".\Input\PTM\np\ptmD_469.inp"
call "%PTM%" ".\Input\PTM\np\ptmD_465.inp"
call "%PTM%" ".\Input\PTM\np\ptmD_99.inp"
) else (
    call :logEcho Skipping Scenario D NP PTM runs
)
exit /b

:RunPTMPP
rem -----------------------
rem PP PTM runs
rem -----------------------
call "%PTM%" ".\Input\PTM\pp\ptmA_314.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_352.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_361.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_367.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_362.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_356.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_463.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_459.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_354.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_404.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_322.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_462.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_42.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_39.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_323.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_350.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_249.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_261.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_34.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_41.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_46.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_469.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_465.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_353.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_29.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_86.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_99.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_225.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_329.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_420.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_359.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_75.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_145.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_227.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_293.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_304.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_351.inp"
call "%PTM%" ".\Input\PTM\pp\ptmA_365.inp"

call "%PTM%" ".\Input\PTM\pp\ptmB_352.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_361.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_367.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_362.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_356.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_463.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_459.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_354.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_404.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_322.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_462.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_42.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_39.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_314.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_323.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_350.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_249.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_261.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_34.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_41.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_46.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_469.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_465.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_353.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_29.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_86.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_99.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_225.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_329.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_420.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_359.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_75.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_145.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_227.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_293.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_304.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_351.inp"
call "%PTM%" ".\Input\PTM\pp\ptmB_365.inp"

call "%PTM%" ".\Input\PTM\pp\ptmC_352.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_361.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_367.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_362.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_356.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_463.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_459.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_354.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_404.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_322.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_462.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_42.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_39.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_314.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_323.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_350.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_249.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_261.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_34.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_41.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_46.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_469.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_465.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_353.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_29.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_86.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_99.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_225.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_329.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_420.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_359.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_75.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_145.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_227.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_293.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_304.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_351.inp"
call "%PTM%" ".\Input\PTM\pp\ptmC_365.inp"
if "!RUN_D!"=="1" (
call "%PTM%" ".\Input\PTM\pp\ptmD_352.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_361.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_367.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_362.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_356.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_463.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_459.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_354.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_404.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_322.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_462.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_42.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_39.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_314.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_323.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_350.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_249.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_261.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_34.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_41.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_46.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_469.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_465.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_353.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_29.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_86.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_99.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_225.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_329.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_420.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_359.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_75.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_145.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_227.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_293.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_304.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_351.inp"
call "%PTM%" ".\Input\PTM\pp\ptmD_365.inp"
) else (
    call :logEcho Skipping Scenario D PP PTM runs
)
exit /b

:RunPTMSP
rem -----------------------
rem SP PTM runs
rem -----------------------
call "%PTM%" ".\Input\PTM\sp\ptmA_350.inp"
if errorlevel 1 (
    echo ERROR: SPptmA_350.inp failed
    pause
    exit /b 1
)
call "%PTM%" ".\Input\PTM\sp\ptmB_350.inp"
call "%PTM%" ".\Input\PTM\sp\ptmC_350.inp"
if "!RUN_D!"=="1" (
call "%PTM%" ".\Input\PTM\sp\ptmD_350.inp"
) else (
    call :logEcho Skipping Scenario D SP PTM runs
)

rem (Repeat similar for B, C, D as in NP and PP)
exit /b

:fail_NP
call :logEcho ERROR: PTM NP failed
pause
exit /b 1

:fail_PP
call :logEcho ERROR: PTM PP failed
pause
exit /b 1

:fail_SP
call :logEcho ERROR: PTM SP failed
pause
exit /b 1

:logEcho
echo %*
exit /b
