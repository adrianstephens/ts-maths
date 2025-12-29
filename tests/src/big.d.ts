import { Operators, scalarExt } from "./core";
export declare function isBigInt(x: any): x is bigint;
declare class _Big implements scalarExt<_Big, number> {
    value: bigint;
    constructor(value: bigint);
    dup(): big;
    neg(): big;
    scale(b: number): big;
    add(b: big): big;
    sub(b: big): big;
    mul(b: big): big;
    div(b: big): big;
    mag(): number;
    from(n: number | bigint): _Big;
    ipow(n: number): big;
    rpow(n: number, d: number): big;
    npow(n: number): big;
    sqrt(): big;
    abs(): big;
    sign(): number;
    recip(): big;
    divmod(b: big): number;
    lt(b: big): boolean;
    eq(b: big): boolean;
    valueOf(): number;
    toString(): string;
}
declare class extentB {
    min: bigint;
    max: bigint;
    static fromCentreExtent(centre: bigint, size: bigint): extentB;
    static from<U extends Iterable<bigint>>(items: U): extentB | undefined;
    constructor(min: bigint, max: bigint);
    extent(): bigint;
    centre(): bigint;
    add(p: bigint): void;
    combine(b: extentB): void;
    encompasses(b: extentB): boolean;
    overlaps(b: extentB): boolean;
    contains(p: bigint): boolean;
    clamp(p: bigint): bigint;
}
export declare const big: ((value: bigint) => _Big) & Operators<bigint> & {
    is(x: any): x is bigint;
    shift(x: bigint, n: bigint): bigint;
    sign(a: bigint): 0 | 1 | -1;
    abs(a: bigint): bigint;
    max(...values: bigint[]): bigint;
    min(...values: bigint[]): bigint;
    gcd(...values: bigint[]): bigint;
    extendedGcd(a: bigint, b: bigint): {
        g: bigint;
        x: bigint;
        y: bigint;
    };
    lcm(...x: bigint[]): bigint;
    divToReal(a: bigint, b: bigint): number;
    modPow(base: bigint, exp: bigint, mod: bigint): bigint;
    npow(x: bigint, n: number): bigint;
    random(bits: number): bigint;
    root(x: bigint, b: number): bigint;
    log2(n: bigint, bits?: bigint): bigint;
    extent: typeof extentB;
};
export type big = _Big;
export default big;
export declare namespace big {
    type extent = extentB;
}
