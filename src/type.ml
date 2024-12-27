type comment =
    |Line of string (* //... *)
    |Group of string (* /* ... */ *)

type cte =
    |Decimal of string
    |Binary of string
    |Octal of string
    |Hexa of string
    |FloatDecimal of string
    |Boolean of bool
    |String of string

type id = string

type unaire=
    |Typeof |Plus |Minus

type bop =
    |Pow |Mult |Div
    |Add |Sub

type cond =
    |Blt |Ble |Gt |Ge (* <, <=, >, >= *)
    |Eq |Diff (* ==, != *)
    |StrictEq |StrictDiff (* ===, !== *)

type expression =
    |Par of expression (* (e) *)
    |Cte of cte
    |Left of left_Member
    |Object of (id * expression) list (* {i1: e1, ..., in: en} *)
    |Array of expression list (* [e1, ..., en] *)
    |Function of expression * expression list (* e(e1, ..., en) *)
    |Unaire of unaire * expression
    |Bop of expression * bop * expression
    |Cond of expression * cond * expression
    |And of expression * expression
    |Or of expression * expression
    |Affectation of left_Member * expression (* l = e *)
and left_Member =
    |IdLM of id
    |Table of expression * expression (* e[e] *)
    |SubElement of expression * id (* e.i *)

type primType=
    |Number
    |Boolean
    |String

type type_ =
    |IdT of id
    |Cte of cte (* par exemple 42 est un type dont le seul élément est 42 *)
    |PrimitiveType of primType (* number, boolean ou string *)
    |Array of type_ (* t[] (t = type)*)
    |Any
    |Object of objectContent list (* {i1: t1, ..., in: tn} (t en option) (séparation : ',' ou ';')*)
    |Union of type_ list (* t1 | ... | tn  ( toute valeur qui possède un de ces types est aussi typable par l’union)*)
and objectContent =
    id * type_ option (* i: t ou i *)

type instruction =
    |Empty (* ; *)
    |Expression of expression (* e; *)
    |Block of body list (* {b1 ... bn} (bi = instruction ou déclaration)*)
    |VarDeclaration of binding list (* var bs (bs = i1 t01 e01, i2 t02 e02, ..., in t0n e0n)*)
    |If of expression * instruction * instruction option (* if (e) i1 else i2 (la partie else i2 est optionnelle ; si elle est présente, elle se rapporte au if le plus proche)*)
    |While of expression * instruction (* while (e) i *)
    |Return of expression option (* return; ou return e; *)
and binding =
    |Binding of id * type_ option * expression option (* i t0 e0 (t0 = ': type_' ou 'vide') (e0 = '= expression' ou 'vide')*)
and declaration =
    |Alias of id * type_ (* type i = t *)
    |LocalLet of binding list (* let bs; *)
    |LocalConst of binding list (* const bs; *)
    |Function of id * binding list * type_ option * body list (* i ( as ) t0 { dis } (si t0 n'est pas vide = ': t')*)
and body =
    |Declaration of declaration
    |Instr of instruction

type file = body list