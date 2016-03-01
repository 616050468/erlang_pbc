"protoc.exe" -opriv\msg.pb priv\msg.proto
call cmd /C rebar.cmd compile
pause