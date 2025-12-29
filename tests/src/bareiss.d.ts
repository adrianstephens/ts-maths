import { ops1, has } from './core';
export declare function LUDecomposeBareiss(A: number[][], pivot?: boolean): {
    perm: number[];
    swaps: number;
};
export declare function LUSolveBareissMulti(A: number[][], X: number[][], perm?: number[]): number[][];
export declare function LUSolveBareissTransposeMulti(A: number[][], X: number[][], perm?: number[]): number[][];
export declare function LUDecomposeBareissT<T extends ops1<T> & has<'sign'>>(A: T[][], pivot?: boolean): {
    perm: number[];
    swaps: number;
};
export declare function LUSolveBareissMultiT<T extends ops1<T> & has<'recip'>>(A: T[][], X: T[][], perm?: number[]): T[][];
export declare function LUSolveBareissTransposeMultiT<T extends ops1<T> & has<'recip'>>(A: T[][], X: T[][], perm?: number[]): T[][];
export declare function solveRectangularBareiss(A: number[][], B: number[][]): number[][] | undefined;
export declare function solveRectangularBareissT<T extends ops1<T> & has<'recip' | 'sign'>>(A: T[][], B: T[][]): T[][] | undefined;
