type numerr = number | "error";

let x : number = 1, y : string, z = 12;
const e = 1, f : string = "d";

function fonction(u : string, v : number = 42, w) : number {
    return v;
}

function nope() {
    return;
}

x = fonction(y, 12, 1);
