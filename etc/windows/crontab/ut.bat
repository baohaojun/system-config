set path=c:\cygwin64\bin;%path%
echo %path%
C:\cygwin64\bin\bash.exe -c "set -x; (set -x ;net stop w32time; sleep 1; net start w32time; sleep 1; w32tm /config /update ; w32tm /resync /rediscover) 2>&1 |iconv -f gbk -t utf-8 |tee ~/time.txt"
