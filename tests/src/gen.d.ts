import { Operators, ops1, scalar, scalarExt, has, has0, hasop, divmodto, Immutable } from "./core";
declare class extentT<T extends has<'lt'>> {
    min: T;
    max: T;
    static fromCentreExtent<T extends scalar<T>>(centre: T, size: T): extentT<T>;
    static from<T extends scalar<T>, U extends Iterable<T>>(items: U): extentT<T> | undefined;
    constructor(min: T, max: T);
    extent(): any;
    centre(): any;
    add(p: T): void;
    combine(b: extentT<T>): void;
    encompasses(b: extentT<T>): boolean;
    overlaps(b: extentT<T>): boolean;
    contains(p: T): boolean;
    clamp(p: T): T;
}
export type canDenominator = Pick<scalarExt<any>, 'from' | 'recip' | 'lt'> & divmodto<any>;
export type GenTypes = number | bigint | ops1<any, any>;
export declare const gen: {
    dup<T extends GenTypes>(a: T): any;
    neg<T extends GenTypes>(a: T): any;
    add<T extends GenTypes>(a: T, b: T): any;
    sub<T extends GenTypes>(a: T, b: T): any;
    mul<T extends GenTypes>(a: T, b: T): any;
    div<T extends GenTypes>(a: T, b: T): any;
    ipow<T extends has0<"mul">>(base: T, exp: number, one?: T): T;
    eq<T extends has0<"eq">>(a: T, b: T): boolean;
    lt<T extends has0<"lt">>(a: T, b: T): boolean;
    copySign<T extends scalar<T>>(a: T, b: T): T;
    max<T extends has0<"lt">>(...values: T[]): T;
    min<T extends has0<"lt">>(...values: T[]): T;
    compare<T extends has0<"lt">>(a: T, b: T): number;
    gcd<T extends Pick<scalarExt<any>, "sign" | "abs" | "dup"> & divmodto<any>>(...values: T[]): T;
    extendedGcd<T extends has<"scale" | "sign" | "abs" | "dup"> & divmodto<any>>(a: T, b: T, one: T): {
        g: T;
        x: T;
        y: any;
    };
    lcm<T extends Pick<scalarExt<any>, "divmod" | "sign" | "dup" | "abs" | "scale">>(...values: T[]): T;
    denominator<T extends canDenominator>(x: T, maxDen: bigint, eps?: T): bigint;
    commonDenominator<T extends canDenominator & has0<"scale">>(numbers: T[], maxDen?: bigint, eps?: T): bigint | 0;
    rationalApprox<T extends canDenominator & has0<"dup">>(x: Immutable<T>, maxDen: bigint, eps?: T): [bigint, bigint];
    continuedFractionT<T extends canDenominator & has0<"dup" | "sign" | "abs" | "add">>(x: T, maxTerms?: number, eps?: T): (bigint | number)[];
    modPow<T extends hasop<"mul"> & divmodto<any>>(base: T, exp: number, mod: T): T;
    OperatorsBase<T extends ops1<T>>(_con: new (...args: any[]) => T): Pick<Operators<T>, (keyof Operators<T> & keyof T) | "func" | "ipow">;
    extent: typeof extentT;
};
export declare abstract class Mod<T> {
    v: T;
    constructor(v: T);
    abstract dup(): this;
    abstract neg(): this;
    abstract scale(n: number): this;
    abstract add(b: Mod<T>): this;
    abstract sub(b: Mod<T>): this;
    abstract mul(b: Mod<T>): this;
    abstract div(b: Mod<T>): this;
    abstract recip(): this;
    abstract from(n: number): this;
    abstract ipow(n: number): this;
    abstract sign(): number;
    abstract eq(b: Mod<T>): boolean;
    abstract toString(): string;
}
export declare function ModFactory<T extends has<'sign' | 'abs' | 'dup' | 'from' | 'eq'> & divmodto<any>>(r: T): {
    new (v: T): {
        _create(p: T): /*elided*/ any;
        wrap(p: T): /*elided*/ any;
        dup(): /*elided*/ any;
        neg(): /*elided*/ any;
        scale(n: number): /*elided*/ any;
        add(b: /*elided*/ any): /*elided*/ any;
        sub(b: /*elided*/ any): /*elided*/ any;
        mul(b: /*elided*/ any): /*elided*/ any;
        div(b: /*elided*/ any): /*elided*/ any;
        recip(): /*elided*/ any;
        from(n: number): /*elided*/ any;
        ipow(n: number): /*elided*/ any;
        sign(): number;
        eq(b: /*elided*/ any): boolean;
        toString(): string;
        v: T;
    };
    wrap(p: T): {
        _create(p: T): /*elided*/ any;
        wrap(p: T): /*elided*/ any;
        dup(): /*elided*/ any;
        neg(): /*elided*/ any;
        scale(n: number): /*elided*/ any;
        add(b: /*elided*/ any): /*elided*/ any;
        sub(b: /*elided*/ any): /*elided*/ any;
        mul(b: /*elided*/ any): /*elided*/ any;
        div(b: /*elided*/ any): /*elided*/ any;
        recip(): /*elided*/ any;
        from(n: number): /*elided*/ any;
        ipow(n: number): /*elided*/ any;
        sign(): number;
        eq(b: /*elided*/ any): boolean;
        toString(): string;
        v: T;
    };
    _create(p: T): {
        _create(p: T): /*elided*/ any;
        wrap(p: T): /*elided*/ any;
        dup(): /*elided*/ any;
        neg(): /*elided*/ any;
        scale(n: number): /*elided*/ any;
        add(b: /*elided*/ any): /*elided*/ any;
        sub(b: /*elided*/ any): /*elided*/ any;
        mul(b: /*elided*/ any): /*elided*/ any;
        div(b: /*elided*/ any): /*elided*/ any;
        recip(): /*elided*/ any;
        from(n: number): /*elided*/ any;
        ipow(n: number): /*elided*/ any;
        sign(): number;
        eq(b: /*elided*/ any): boolean;
        toString(): string;
        v: T;
    };
};
export default gen;
export declare namespace gen {
    type extent<T extends has<'lt'>> = extentT<T>;
}
