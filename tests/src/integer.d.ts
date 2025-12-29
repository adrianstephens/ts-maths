export type integer = number & {
    __brand: 'Integer';
};
export declare const integer: {
    from(n: number): integer;
    is(n: number): n is integer;
    almost(x: number, epsilon?: number): boolean;
    shift(x: integer, n: integer): integer;
    add(a: integer, b: integer): integer;
    sub(a: integer, b: integer): integer;
    mul(a: integer, b: integer): integer;
    div(a: integer, b: integer): integer;
    neg(n: integer): integer;
    abs(n: integer): integer;
    sign(n: integer): number;
    ipow(x: integer, n: integer): integer;
    rpow(x: integer, n: integer, d: integer): integer;
    sqrt(n: integer): integer;
    root(n: integer, r: integer): integer;
    modPow(ibase: integer, iexp: integer, mod: integer): integer;
    gcd(...values: integer[]): integer;
    extendedGcd(a: integer, b: integer): {
        g: integer;
        x: integer;
        y: integer;
    };
    lcm(...x: integer[]): integer;
};
export default integer;
