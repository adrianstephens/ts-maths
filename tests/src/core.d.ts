export type hasProperty<K extends string, S> = {
    [P in K]: S;
};
export declare function isInstance<T>(x: any, i: (new (...args: any[]) => T) | ((...args: any[]) => T)): x is T;
export declare function hasStatic(x: any, f: string): ((...args: any[]) => any) | undefined;
export declare function arrayOf<U>(arr: any[], g: (x: any) => x is U): arr is U[];
export type AsFreeFunction<T, K extends keyof T> = T[K] extends (this: infer This, ...args: infer Args) => infer R ? (self: This, ...args: Args) => R : never;
export interface Operators<T> {
    from(n: number): T;
    func(name: string, args: T[]): T | undefined;
    variable(name: string): T | undefined;
    dup(a: T): T;
    neg(a: T): T;
    scale(a: T, b: number): T;
    add(a: T, b: T): T;
    sub(a: T, b: T): T;
    mul(a: T, b: T): T;
    div(a: T, b: T): T;
    ipow(a: T, n: number): T;
    rpow(a: T, n: number, d: number): T;
    pow(a: T, b: T): T;
    eq(a: T, b: T): any;
    lt(a: T, b: T): any;
}
export declare function Type<T>(operators: Operators<T>): {
    new (value: T): {
        value: T;
        from(n: number): /*elided*/ any;
        dup(): /*elided*/ any;
        neg(): /*elided*/ any;
        scale(b: number): /*elided*/ any;
        add(b: /*elided*/ any): /*elided*/ any;
        sub(b: /*elided*/ any): /*elided*/ any;
        mul(b: /*elided*/ any): /*elided*/ any;
        div(b: /*elided*/ any): /*elided*/ any;
        mag(): number;
        abs(): /*elided*/ any;
        sign(): number;
        eq(b: /*elided*/ any): boolean;
        lt(b: /*elided*/ any): boolean;
        sqrt(): /*elided*/ any;
        recip(): /*elided*/ any;
        ipow(n: number): /*elided*/ any;
        rpow(n: number, d: number): /*elided*/ any;
        npow(n: number): /*elided*/ any;
        toString(): string;
        valueOf(): number;
    };
};
export interface allOps<C, S = number> {
    dup(): C;
    neg(): C;
    scale(b: S): C;
    add(b: C): C;
    sub(b: C): C;
    mul(b: C): C;
    div(b: C): C;
    mag(): number | scalarExt<any>;
    from(n: number | bigint): C;
    ipow(n: number): C;
    abs(): C;
    sign(): number;
    eq(b: C): boolean;
    lt(b: C): boolean;
    valueOf(): number | bigint;
    sqrt(): C;
    recip(): C;
    rpow(n: number, d: number): C;
    npow(n: number): C;
    divmod(this: C, b: C): number | bigint;
    min(b: C): C;
    max(b: C): C;
}
export type hasop<K extends keyof allOps<any>, T = any, S = number> = Pick<allOps<T, S>, K>;
export declare function hasop<K extends keyof allOps<any>>(f: K): (x: any) => x is hasop<K>;
export declare function hasopT<T, K extends keyof allOps<any>>(f: K): (x: T) => x is (T & hasop<K>);
export type divmodto<T, R = any> = hasProperty<'divmod', (b: T) => R>;
export interface ops0<C, S = number> extends hasop<'dup' | 'neg' | 'scale' | 'add' | 'sub', C, S> {
}
export interface ops1<C, S = number> extends ops0<C, S>, hasop<'mul' | 'div', C, S> {
}
export type ops<C, S = number> = ops1<C, S>;
export interface scalar<C, S = number> extends ops<C, S>, hasop<'from' | 'ipow' | 'abs' | 'sign' | 'eq' | 'lt' | 'valueOf', C, S> {
}
export interface scalarExt<C, S = number> extends scalar<C, S>, hasop<'sqrt' | 'recip' | 'rpow' | 'npow' | 'divmod', C, S> {
}
export type Immutable<T> = Omit<T, 'divmod'>;
export type has0<K extends keyof allOps<any>> = Pick<allOps<any>, K>;
export declare function has0<K extends keyof scalarExt<any>>(f: K): (x: object) => x is has0<K>;
export type has<K extends keyof scalarExt<any>> = ops1<any> & Pick<scalarExt<any>, K>;
export declare function has<K extends keyof scalarExt<any>>(f: K): (x: ops1<any>) => x is has<K>;
export type hasFree<K extends keyof scalarExt<any>> = {
    [P in K]: AsFreeFunction<scalarExt<any>, K>;
};
export declare function hasFree<K extends keyof scalarExt<any>>(f: K): (x: object) => x is hasFree<K>;
export declare function isScalar(x: ops1<any>): x is scalar<any>;
export declare function isScalarExt(x: ops1<any>): x is scalarExt<any>;
export declare function asScalarT<T extends scalarExt<T>>(from: T, x: number | T): T;
export declare function compare<T extends number | bigint | string>(a: T, b: T): number;
export declare function sign<T extends number | bigint>(a: T): number;
export declare function lazySlice<T>(arr: T[], start?: number, end?: number): Generator<T>;
