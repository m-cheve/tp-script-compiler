function f(x : number | "coucou" | string[]) {
}

f(42);
f("coucou");
f(["", "1"]);


type square = { color : string;
		side : number };

type circle = { radius : number, color : string, };

function color_of(h : square | circle) : string {
    return h.color;
}

var r : string;

r = color_of({ color : "blue", side : 2 });
r = color_of({ radius : 1.2, color : "red"});
