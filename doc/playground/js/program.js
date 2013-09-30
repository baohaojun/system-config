document.writeln(window.btoa("hello"));
document.writeln(window.atob("aGVsbG8="));
ret = window.confirm("hello world");
ret = window.find("hellox");
var n = 1;
setInterval(function () {document.writeln("hello " + n); n += 1}, 1000);
document.writeln("ret is " + ret);
document.writeln(escape("hello\\hell<>"));
y = window.navigator;
for (x in y) {
//    document.writeln(x + ": " + y[x]);
}


document.writeln("<h1> hello world</h1>");

