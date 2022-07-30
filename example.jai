#import "Basic";
#import,file "units.jai";

main :: () {
    x := 1.0 * _m * _m;
    y := 2.0 * _s;
    print("x = %, y = %\n", x, y);
    print("x = %, y = %\n", str(x), str(y));

    z := x * y;
    print("z = %\n", str(z));

    z = z * 2.0;
    print("z = %\n", str(z));
    z = 0.1 * z;
    print("z = %\n", str(z));
    z = 10.0/z;
    print("z = %\n", str(z));

    z = z * 1.0 * _s;
    print("z = %\n", str(z));

    #if false {
        z = z + x;
    }
}