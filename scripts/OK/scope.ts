y = 2;

var y : number;

function f() {
    z = "oucou";
    { let x = 1;
      var z : string;
      x = 2;
    }
}

y = g(2);

function g(a : number) : number {
    return a + 1;
}
