export interface Equal<T> {
    eq(b: T): boolean;
    approx?(b: T, tol: number): boolean;
}
type Testable<T> = T extends (string | number | boolean | null | undefined) ? T : T & Equal<T>;
export declare function expect<T>(v: Testable<T>, description?: string): {
    toEqual(v2: T): void;
    toBeCloseTo(v2: T, tol?: number): void;
    check(test: (v: T) => boolean): void;
};
export declare function test(name: string, fn: () => void): void;
export declare function verify<T>(v1: T, v2: T, test: (v1: T, v2: T) => boolean, description?: string): void;
export declare function assert(condition: boolean, msg?: string): void;
export declare function approxArray(a: number[], b: number[], tol?: number): boolean;
export declare function makeApproxArray(tol: number): (a: number[], b: number[]) => boolean;
export declare function sequence(length: number, from?: number, step?: number): number[];
export {};
