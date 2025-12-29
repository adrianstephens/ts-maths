import { Operators, scalar, scalarExt, ops1 } from "./core";
import integer from "./integer";
declare class _real implements scalarExt<_real> {
    value: number;
    constructor(value: number);
    dup(): real;
    neg(): real;
    scale(b: number): real;
    mul(b: real): real;
    div(b: real): real;
    add(b: real): real;
    sub(b: real): real;
    mag(): number;
    from(n: number | bigint): _real;
    sign(): number;
    abs(): real;
    recip(): real;
    divmod(b: real): number;
    lt(b: real): boolean;
    eq(b: real): boolean;
    sqrt(): real;
    ipow(n: number): real;
    npow(n: number): real;
    rpow(n: number, d: number): _real;
    valueOf(): number;
    toString(): string;
}
declare class _extent {
    min: number;
    max: number;
    static fromCentreExtent(centre: number, size: number): _extent;
    static from<U extends Iterable<number>>(items: U): _extent;
    constructor(min?: number, max?: number);
    extent(): number;
    centre(): number;
    add(p: number): void;
    combine(b: _extent): void;
    encompasses(b: _extent): boolean;
    overlaps(b: _extent): boolean;
    contains(p: number): boolean;
    clamp(p: number): number;
}
export declare const real: ((value: number) => _real) & Operators<number> & {
    is(x: any): x is number;
    approx(x: number, y: number, epsilon?: number): boolean;
    copySign(a: number, b: number): number;
    gcd(...values: number[]): number;
    extendedGcd(a: number, b: number): {
        g: number;
        x: number;
        y: number;
    };
    lcm(...x: number[]): number;
    denominator(x: number, maxDen: number, eps?: number): integer;
    commonDenominator(numbers: number[], maxDen?: number, eps?: number): integer;
    rationalApprox(x: number, maxDen: number, eps?: number): number[];
    continuedFraction(x: number, maxTerms?: number, eps?: number): integer[];
    extent: typeof _extent;
};
export type real = _real;
export default real;
export declare namespace real {
    type extent = _extent;
}
export declare function asScalar(x: number | bigint | ops1<any>): scalar<any>;
export declare function asScalarExt(x: number | bigint | ops1<any>): scalarExt<any>;
