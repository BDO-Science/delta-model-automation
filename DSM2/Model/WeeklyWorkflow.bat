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
set "MODEL_DIR=%CD%"

rem =========================================================
rem Interactive control flag
rem =========================================================
set RUNALL=0
rem =========================================================
rem PTM station lists (edit here; same stations for every scenario)
rem =========================================================
set "NP_STATIONS=350 469 465 99"
set "PP_STATIONS=314 352 361 367 362 356 463 459 354 404 322 462 42 39 323 350 249 261 34 41 46 469 465 353 29 86 99 225 329 420 359 75 145 227 293 304 351 365"
set "SP_STATIONS=350"

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
rem 0. Scenario discovery (always runs, not skippable)
rem =========================================================
call :logEcho Discovering OMR scenarios in DataExternal\%NEWEST%

python scenario_discovery.py "%NEWEST%"
if errorlevel 1 (
    call :logEcho ERROR: Scenario discovery failed!
    pause
    exit /b 1
)

set "SCEN_FILE=DataExternal\%NEWEST%\scenarios_%NEWEST%.cmd"
if not exist "%SCEN_FILE%" (
    call :logEcho ERROR: %SCEN_FILE% not found!
    pause
    exit /b 1
)
call "%SCEN_FILE%"

call :logEcho Scenarios found: %SCEN_COUNT%
for %%S in (%SCEN_LIST%) do call :logEcho   Scenario %%S = OMRI !SCEN_%%S_OMRI!
set "RUN_D=0"
if defined SCEN_D_OMRI set "RUN_D=1"

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
    call :logEcho ERROR: InputUpdates.py failed!
    pause
    exit /b 1
)

:skip1
rem ---- Always read dates (Section 2 needs them even if Section 1 was skipped)
set "INPUTFILE=DataExternal\%NEWEST%\InputUpdates_%NEWEST%.txt"
set "FORE="
set "END="
set "FORE_ISO="
set "END_ISO="

if exist "%INPUTFILE%" (
    call :logEcho Reading dates from InputUpdates file
    for /f "usebackq tokens=1,* delims==" %%A in ("%INPUTFILE%") do (
        set "KEY=%%A"
        set "VALUE=%%B"
        set "KEY=!KEY: =!"
        set "VALUE=!VALUE: =!"
        if /I "!KEY!"=="FORE_START_DATE" set "FORE=!VALUE!"
        if /I "!KEY!"=="END_DATE" set "END=!VALUE!"
    )
    if defined FORE call :ConvertDate !FORE! FORE_ISO
    if defined END  call :ConvertDate !END! END_ISO
) else (
    call :logEcho WARNING: %INPUTFILE% not found - run Section 1 before Section 2
)

call :logEcho Skipping or completed Section 1


rem =========================================================
rem 2. Preprocessor
rem =========================================================
call :AskRun "Section 2 - Preprocessor"
if errorlevel 1 goto skip2

if not defined FORE_ISO (
    call :logEcho ERROR: Forecast dates not available. Run Section 1 first.
    pause
    exit /b 1
)
if not defined END_ISO (
    call :logEcho ERROR: End date not available. Run Section 1 first.
    pause
    exit /b 1
)

call :logEcho Running Preprocessor workflow for scenarios: %SCEN_LIST%

cd Preprocessor
set "BASE=..\DataExternal\%NEWEST%"

for %%S in (%SCEN_LIST%) do (
    call :RunOMRIFile "%BASE%\!SCEN_%%S_FILE!" %%S
    if errorlevel 1 (
        call :logEcho ERROR: Preprocessing failed for scenario %%S
        pause
        exit /b 1
    )
)

python ForecastDuplicate_STN.py -f ..\Input\timeseries\forecast.dss --scenarios %SCEN_LIST% --yes
if errorlevel 1 (
    call :logEcho ERROR: ForecastDuplicate_STN.py failed
    pause
    exit /b 1
)

cd ..

:skip2
call :logEcho Skipping or completed Section 2
rem =========================================================
rem 3. Hydro binaries
rem =========================================================
call :AskRun "Section 3 - Hydro binaries"
if errorlevel 1 goto skip3

call :logEcho Running hydro binaries for scenarios: %SCEN_LIST%

for %%S in (%SCEN_LIST%) do (
    if not exist ".\Input\hydro%%S.inp" (
        call :logEcho ERROR: .\Input\hydro%%S.inp not found
        pause
        exit /b 1
    )
    call :logEcho Running hydro for scenario %%S
    .\bin\hydro .\Input\hydro%%S.inp
    if errorlevel 1 (
        call :logEcho ERROR: hydro failed for scenario %%S
        pause
        exit /b 1
    )
)

:skip3
call :logEcho Skipping or completed Section 3

rem =========================================================
rem 4. PTM NP
rem =========================================================
call :AskRun "Section 4 - PTM NP"
if errorlevel 1 goto skip4

set "PTM=.\bin\ptm.bat"
call :RunPTMType np "%NP_STATIONS%" || goto :fail_NP

:skip4
call :logEcho Skipping or completed Section 4

rem =========================================================
rem 5. PTM PP
rem =========================================================
call :AskRun "Section 5 - PTM PP"
if errorlevel 1 goto skip5

set "PTM=.\bin\ptm.bat"
call :RunPTMType pp "%PP_STATIONS%" || goto :fail_PP

:skip5
call :logEcho Skipping or completed Section 5

rem =========================================================
rem 6. PTM SP
rem =========================================================
call :AskRun "Section 6 - PTM SP"
if errorlevel 1 goto skip6

set "PTM=.\bin\ptm.bat"
call :RunPTMType sp "%SP_STATIONS%" || goto :fail_SP

:skip6
call :logEcho Skipping or completed Section 6

rem =========================================================
rem 7. NoPumping
rem =========================================================
call :AskRun "Section 7 - NoPumping"
if errorlevel 1 goto skip7

call :logEcho Running NoPumping for scenarios: %SCEN_LIST%
python "..\Model_NOPUMP\NoPumping.py" --scenarios %SCEN_LIST%
if errorlevel 1 (
    call :logEcho ERROR: NoPumping.py failed
    pause
    exit /b 1
)

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

call :logEcho Running NoPumping hydro for scenarios: %SCEN_LIST%

for %%S in (%SCEN_LIST%) do (
    if not exist ".\Input\hydro%%S.inp" (
        call :logEcho ERROR: .\Input\hydro%%S.inp not found in Model_NOPUMP
        popd
        pause
        exit /b 1
    )
    call :logEcho Running NoPumping hydro for scenario %%S
    .\bin\hydro ".\Input\hydro%%S.inp"
    if errorlevel 1 (
        call :logEcho ERROR: NoPumping hydro failed for scenario %%S
        popd
        pause
        exit /b 1
    )
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
if errorlevel 1 goto skip13

call :logEcho ======================================
call :logEcho Running PTM plotting script
call :logEcho ======================================

pushd "..\Model" || (
    echo ERROR: Could not change to Model folder
    exit /b 1
)

python plot_ptm_particles.py %NEWEST% || exit /b 1

:skip13
call :logEcho Skipping or completed Section 13
rem =========================================================
rem STOP BEFORE LFS PRODUCTS
rem =========================================================
echo.
echo ======================================
echo Workflow paused before LFS products.
echo.
echo Verify that SLS survey data are available.
echo.
echo Press:
echo   Y = Continue to LFS and Entrainment
echo   N = Stop workflow here
echo ======================================

choice /c YN /n

if errorlevel 2 (
    echo Workflow stopped before Section 14.
    pause
    exit /b 0
)
call :PrepareSLS
if errorlevel 1 (
    echo.
    echo WARNING: DSM2\SLS\%NEWEST% could not be prepared.
    echo Sections 14 and 15 will fail until it exists with a survey CSV in it.
    pause
)

rem =========================================================
rem 14. LFS Plots
rem =========================================================
call :AskRun "Section 14 - LFS Plots"
if errorlevel 1 goto skip14

call :logEcho ======================================
call :logEcho Running LFS plotting script
call :logEcho ======================================


pushd "..\Model" || (
    echo ERROR: Could not change to Model folder
    exit /b 1
)

python plot_ptm_lfs.py %NEWEST% || exit /b 1

:skip14
call :logEcho Skipping or completed Section 14

rem =========================================================
rem 15. Entrainment Plots
rem =========================================================
call :AskRun "Section 15 - Entrainment Plots"
if errorlevel 1 goto skip15

call :logEcho ======================================
call :logEcho Running Entrainment plotting script
call :logEcho ======================================

pushd "..\Model" || (
    echo ERROR: Could not change to Model folder
    exit /b 1
)

python entrainment_map.py %NEWEST% || exit /b 1

:skip15
call :logEcho Skipping or completed Section 15

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
:: %1 = DDMonYYYY   %2 = output variable
set "CD_IN=%~1"
set "CD_DAY=%CD_IN:~0,2%"
set "CD_MON=%CD_IN:~2,3%"
set "CD_YEAR=%CD_IN:~5,4%"
set "CD_MM="
if /I "%CD_MON%"=="Jan" set "CD_MM=01"
if /I "%CD_MON%"=="Feb" set "CD_MM=02"
if /I "%CD_MON%"=="Mar" set "CD_MM=03"
if /I "%CD_MON%"=="Apr" set "CD_MM=04"
if /I "%CD_MON%"=="May" set "CD_MM=05"
if /I "%CD_MON%"=="Jun" set "CD_MM=06"
if /I "%CD_MON%"=="Jul" set "CD_MM=07"
if /I "%CD_MON%"=="Aug" set "CD_MM=08"
if /I "%CD_MON%"=="Sep" set "CD_MM=09"
if /I "%CD_MON%"=="Oct" set "CD_MM=10"
if /I "%CD_MON%"=="Nov" set "CD_MM=11"
if /I "%CD_MON%"=="Dec" set "CD_MM=12"
if not defined CD_MM (
    call :logEcho ERROR: Could not parse date "%~1"
    exit /b 1
)
set "%~2=%CD_YEAR%-%CD_MM%-%CD_DAY%"
exit /b 0

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
    -sd %SD% ^
    --yes
exit /b %errorlevel%

:RunPTMType
rem %1 = run type (np|pp|sp)   %2 = space-separated station list
set "PT_TYPE=%~1"
set "PT_STATIONS=%~2"
set "PT_FAIL=0"

rem ---- Pass 1: check every input file exists BEFORE starting long runs
for %%S in (%SCEN_LIST%) do (
    for %%N in (%PT_STATIONS%) do (
        if not exist ".\Input\PTM\%PT_TYPE%\ptm%%S_%%N.inp" (
            call :logEcho ERROR: Missing .\Input\PTM\%PT_TYPE%\ptm%%S_%%N.inp
            set "PT_FAIL=1"
        )
    )
)
if "!PT_FAIL!"=="1" exit /b 1

rem ---- Pass 2: run them (scenario outer loop, station inner loop)
for %%S in (%SCEN_LIST%) do (
    for %%N in (%PT_STATIONS%) do (
        call :logEcho Running PTM %PT_TYPE% - scenario %%S - station %%N
        call "%PTM%" ".\Input\PTM\%PT_TYPE%\ptm%%S_%%N.inp"
        if errorlevel 1 (
            call :logEcho ERROR: PTM failed for ptm%%S_%%N.inp ^(%PT_TYPE%^)
            set "PT_FAIL=1"
        )
    )
)
if "!PT_FAIL!"=="1" exit /b 1
exit /b 0

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
:PrepareSLS
rem Create DSM2\SLS\<NEWEST> by copying the most recent EARLIER weekly SLS folder
set "SLS_ROOT="
for %%I in ("%MODEL_DIR%\..\SLS") do set "SLS_ROOT=%%~fI"
set "SLS_DEST=%SLS_ROOT%\%NEWEST%"

if not exist "%SLS_ROOT%\" (
    call :logEcho ERROR: SLS folder not found: !SLS_ROOT!
    exit /b 1
)

if exist "%SLS_DEST%\" (
    call :logEcho SLS folder %NEWEST% already exists - using it as is
    exit /b 0
)

set "SLS_SRC="
for /f "delims=" %%D in ('dir /b /ad /o-n "%SLS_ROOT%" 2^>nul') do (
    if not defined SLS_SRC (
        echo %%D| findstr /r /x "[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]" >nul
        if not errorlevel 1 (
            if %%D lss %NEWEST% set "SLS_SRC=%%D"
        )
    )
)

if not defined SLS_SRC (
    call :logEcho ERROR: No weekly SLS folder earlier than %NEWEST% was found to copy from
    exit /b 1
)

call :logEcho Copying SLS folder !SLS_SRC! to %NEWEST%
xcopy "%SLS_ROOT%\!SLS_SRC!" "%SLS_DEST%\" /E /I /Y /Q >nul
if errorlevel 1 (
    call :logEcho ERROR: Copy of the SLS folder failed
    exit /b 1
)

dir /b "%SLS_DEST%\*.csv" >nul 2>&1
if errorlevel 1 call :logEcho WARNING: No CSV files found in !SLS_DEST!

call :logEcho Created !SLS_DEST! from !SLS_SRC!
call :logEcho NOTE: This reuses the survey from !SLS_SRC!. If a newer SLS survey is out, replace the CSV in !SLS_DEST!.
exit /b 0