import integer from './integer';
export declare function isProbablePrime(n: integer): boolean;
export declare class factorisation extends Array<integer> {
    constructor(n: integer | integer[]);
    dup(): factorisation;
    divisors(limit?: number): Generator<integer>;
    selfMul(other: factorisation | integer): factorisation;
    selfDiv(other: factorisation | integer): factorisation;
    mul(other: factorisation | integer): factorisation;
    div(other: factorisation | integer): factorisation;
    pow(exp: integer): factorisation;
}
export declare function isProbablePrimeB(n: bigint, witnesses?: Iterable<bigint> | number): boolean;
export declare class factorisationB extends Map<bigint, number> {
    constructor(n: bigint | Iterable<[bigint, number]>);
    dup(): factorisationB;
    divisors(limit?: bigint): Generator<bigint>;
    selfMul(other: factorisationB | bigint): factorisationB;
    selfDiv(other: factorisationB | bigint): factorisationB;
    mul(other: factorisationB | bigint): factorisationB;
    div(other: factorisationB | bigint): factorisationB;
    pow(exp: number): factorisationB;
}
