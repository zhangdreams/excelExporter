@echo off

chcp 65001 >nul

:loop

echo 拖动文件到这里并按下 Enter 导出:

set /p "input="

if "%input%"=="" (
	goto loop
)

REM set "input_file"=%input%
REM set "input_file"="%~1"

REM 前后端输出目录
set "out_dir1=./out/"
set "out_dir2=./out/"

REM start "" /wait excel.exe %input% %out_dir1% %out_dir2%
for /F "delims=" %%i in ('excel.exe %input% %out_dir1% %out_dir2%') do (
	echo %%i
	)

echo %input% 导出成功...


goto loop

:eof