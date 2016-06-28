build-debug:
	xbuild /p:Configuration=Debug Exercises.fsproj

build-release:
	xbuild /p:Configuration=Release Exercises.fsproj

debugArgs:
	clear; mono --debug --debugger-agent=transport=dt_socket,server=y,address=127.0.0.1:5858 bin/Debug/Exercises.exe $(filter-out $@,$(MAKECMDGOALS))

runArgs:
	clear; mono bin/Debug/Exercises.exe $(filter-out $@,$(MAKECMDGOALS))
