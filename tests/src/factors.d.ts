import { scalar, hasop } from './core';
import { Polynomial, PolyTypes, PolyNTypes, coeffOps } from './polynomial';
import rational, { rationalT, canMakeRational } from './rational';
export type Factor<T> = {
    factor: Polynomial<T>;
    multiplicity: number;
};
type factorOps<T> = coeffOps<T> & hasop<'recip' | 'sign' | 'from'>;
type factorOps1<T> = factorOps<T> & hasop<'lt' | 'divmod'>;
type factorType = number | factorOps<any>;
export declare function evaluateMod<T extends PolyNTypes>(p: Polynomial<Polynomial<T>>, x: Polynomial<T>, m: Polynomial<T>): Polynomial<T>;
export declare function polyGCD<T extends PolyNTypes>(A: Polynomial<T>, B: Polynomial<T>): Polynomial<T>;
export declare function squareFreeFactorization<T extends PolyTypes>(f: Polynomial<T>): Factor<T>[];
export declare function factorOverK<T extends factorType>(f: Polynomial<T>): Factor<T>[];
interface FactorGCD<T> extends Factor<T> {
    gcd: Polynomial<Polynomial<rational>>;
}
export declare function rothsteinPartial<T extends number | factorOps1<any>>(N: Polynomial<Polynomial<T>>, D: Polynomial<Polynomial<T>>): {
    R: undefined;
    factors: never[];
    gcds: FactorGCD<T>[];
} | {
    R: Polynomial<T>;
    factors: Factor<T>[];
    gcds: FactorGCD<T>[];
};
interface Residue<T> extends Factor<T> {
    residue: Polynomial<any>;
}
export declare function rothsteinResidues<T extends canMakeRational>(N: Polynomial<Polynomial<T>>, D: Polynomial<Polynomial<T>>, gcds: FactorGCD<T>[]): Residue<T>[];
export declare function interpolate<T extends PolyNTypes>(points: [T, T][], one?: T): Polynomial<T>;
export declare function sylvesterMatrix<T>(p: T[], q: T[], zero: T): T[][];
export declare function resultant<T extends PolyNTypes>(p: Polynomial<T>, q: Polynomial<T>): T | undefined;
export declare function discriminant<T extends number | scalar<any>>(p: Polynomial<T>): T | undefined;
/**
 * Generate Vieta's formulas for a polynomial of given degree
 * Returns equations relating roots to coefficients
 *
 * For degree n with roots r₁, r₂, ..., rₙ and coefficients c₁, c₂, ..., cₙ:
 * - σ₁ = r₁ + r₂ + ... + rₙ = -c₁
 * - σ₂ = r₁r₂ + r₁r₃ + ... = c₂
 * - σ₃ = r₁r₂r₃ + ... = -c₃
 * - ...
 * - σₙ = r₁r₂...rₙ = (-1)ⁿcₙ
 */
export declare function vietasFormulas<T extends scalar<T>>(poly: Polynomial<T>, roots: T[]): T[];
/**
 * Partial-fraction decomposition for one-variable rational functions with numeric coefficients when the denominator has only distinct real linear factors.
 * Returns the polynomial part and an array of terms { root, residue } such that
 *
 *   P(x)/Q(x) = polyPart(x) + sum_r residue_r / (x - root_r)
 *
 * Returns `undefined` when the denominator is not separable into distinct real linear factors or when numeric roots cannot be obtained.
 */
type Term<T extends PolyTypes> = {
    root: T;
    residue: T;
};
export declare function partialFractions(r: rationalT<Polynomial<number>>): {
    polyPart: Polynomial<number>;
    terms: Term<number>[];
} | undefined;
type TermT<T extends scalar<T>> = {
    factor: Polynomial<T>;
    order: number;
    numer: Polynomial<T>;
};
export declare function partialFractionsT<T extends scalar<T> & hasop<'recip'>>(r: rationalT<Polynomial<T>>): {
    polyPart: Polynomial<T>;
    terms: TermT<T>[];
} | undefined;
export declare function partialPower<T extends scalar<T> & hasop<'recip'>>(r: rationalT<Polynomial<T>>): number;
export declare function hermiteReduce<T extends scalar<T> & hasop<'recip'>>(r: rationalT<Polynomial<T>>, one: T): {
    polyPart: Polynomial<T>;
    derivative: rationalT<Polynomial<T>>[];
    remainder: rationalT<Polynomial<T>>;
} | undefined;
export {};
