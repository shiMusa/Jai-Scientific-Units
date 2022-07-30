#import "Basic";
Math :: #import "Math";


// https://www.nist.gov/pml/owm/metric-si/si-units
SI_UNITS          :: "m s mol A K cd kg";
SI_UNITS_EXTENDED :: "m s mol A K cd kg Bq sr";

#module_parameters(UNITS := SI_UNITS, SI_DERIVED_UNITS := true);

#if (UNITS == SI_UNITS || UNITS == SI_UNITS_EXTENDED) && SI_DERIVED_UNITS {
    _N   :: #run  _kg * _m / _s / _s;
    _J   :: #run _N * _m;
    _C   :: #run _A * _s;
    _kat :: #run _mol / _s;
    _W   :: #run _J / _s;
    _Pa  :: #run _N / _m / _m;
    _V   :: #run _W/_A;
    _Wb  :: #run _V * _s;
    _T   :: #run _Wb / _m / _m;
    _Gy  :: #run _J / _kg;
    _Ohm :: #run _V / _A;
    _H   :: #run _Wb / _A;
    _F   :: #run _C / _V;
    _Sv  :: #run _J / _kg;
    _S   :: #run inv(_Ohm);
    _Hz  :: #run inv(_s);
    _rad :: #run (2 * Math.PI64) / _s;
}


// There are `Unit`s and `Measure`s. 

// `Unit`s just hold the - well - units of whatever you are working with, e.g.
// `m`, `s`, `Ohm` - whatever is given as the module parameters.

// A `Measure` holds a value and it's `Unit`s. So, e.g. 2 m, 10 Ohm, -42 V, 3 J.

// Operators are overloaded for both in combination with each other and number
// values (.INTEGER, .FLOAT)


// Construct the `Units` struct from the given string of units
#insert #run ()->string {
    #import "String";
    us := split(UNITS, " ");

    b : String_Builder;

    append(*b, "Unit :: struct {\n");
    print_to_builder(*b, "    exp: [%]s8;\n", us.count);
    print_to_builder(*b, "    names :: string.%;\n", us);
    append(*b, "}\n");

    for us {
        print_to_builder(*b, "_% :: Unit.{s8.[", it);
        for i: 0..us.count-1 {
            if i == it_index then
                append(*b, "1");
            else
                append(*b, "0");
            if i != us.count-1 then
                append(*b, ",");
        }
        append(*b, "]};\n");
    }

    return builder_to_string(*b);
}();

inv :: (u: Unit) -> Unit {
    res := u;
    for *res.exp {
        <<it = -<<it;
    }
    return res;
}

operator * :: (a: Unit, b: Unit) -> Unit {
    res : Unit = ---;
    for a.exp {
        res.exp[it_index] = it + b.exp[it_index];
    }
    return res;
}
operator / :: (a: Unit, b: Unit) -> Unit {
    res : Unit = ---;
    for a.exp {
        res.exp[it_index] = it - b.exp[it_index];
    }
    return res;
}

Measure :: struct(T: Type) {
    value: T;
    units : Unit;
}
str :: (val: Measure) -> string {
    b : String_Builder;

    print_to_builder(*b, "% ", val.value);

    for val.units.exp {
        if it == 0 then continue;
        if it != 1 then
            print_to_builder(*b, "%^% ", Unit.names[it_index], it);
        else
            print_to_builder(*b, "% ", Unit.names[it_index]);
    }

    return builder_to_string(*b);
}


operator * :: (a: $T, b: Unit) -> Measure(T) #modify {
    ti := cast(*Type_Info)T;
    if ti.type != .INTEGER && ti.type != .FLOAT
        return false;
    return true;
} #symmetric {
    res : Measure(T);
    res.value = a;
    res.units = b;
    return res;
}
operator / :: (a: $T, b: Unit) -> Measure(T) #modify {
    ti := cast(*Type_Info)T;
    if ti.type != .INTEGER && ti.type != .FLOAT
        return false;
    return true;
} { 
    res : Measure(T);
    res.value = a;
    res.units /= b;
    return res;
}
operator / :: (a: Unit, b: $T) -> Measure(T) #modify {
    ti := cast(*Type_Info)T;
    if ti.type != .INTEGER && ti.type != .FLOAT
        return false;
    return true;
} { 
    res : Measure(T);
    res.value = cast(T)(1)/b;
    res.units = a;
    return res;
}


operator * :: (a: Measure($T), b: Unit) -> Measure(T) #symmetric {
    res := a;
    res.units *= b;
    return res;
}
operator / :: (a: Measure($T), b: Unit) -> Measure(T) {
    res := a;
    res.units /= b;
    return res;
}
operator / :: (a: Unit, b: Measure($T)) -> Measure(T) {
    res := b;
    res.units = a/b.units;
    return res;
}


operator * :: (a: Measure($A), b: Measure(A)) -> Measure(A) {
    res : Measure(A);
    res.value = a.value * b.value;
    res.units = a.units * b.units;
    return res;
}
operator / :: (a: Measure($A), b: Measure(A)) -> Measure(A) {
    res : Measure(A);
    res.value = a.value / b.value;
    res.units = a.units / b.units;
    return res;
}


operator * :: (a: Measure($A), b: A) -> Measure(A) #symmetric {
    res := a;
    res.value *= b;
    return res;
}
operator / :: (a: Measure($A), b: A) -> Measure(A) {
    res := a;
    res.value /= b;
    return res;
}
operator / :: (a: $A, b: Measure(A)) -> Measure(A) {
    res : Measure(A);
    res.value = a/b.value;
    res.units /= b.units;
    return res;
}


operator + :: (a: Measure($A), b: Measure(A)) -> Measure(A) {
    for a.units.exp {
        assert(it == b.units.exp[it_index], "\n\nCannot add measures with different units:\n\t% + %", str(a), str(b));
    }
    res := a;
    res.value += b.value;
    return res;
}
operator - :: (a: Measure($A), b: Measure(A)) -> Measure(A) {
    for a.units.exp {
        assert(it == b.units.exp[it_index], "\n\nCannot subtract measures with different units:\n\t% - %", str(a), str(b));
    }
    res := a;
    res.value -= b.value;
    return res;
}