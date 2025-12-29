import { Operators } from './core';
type InferOpsType<O> = O extends {
    from(n: number): infer T;
} ? T : never;
export type FractionOptions = {
    fracChars?: Record<number, Record<number, string>>;
    fracSuperSub?: boolean;
};
type ConstOptions = FractionOptions & {
    fractions?: boolean;
    radicals?: boolean;
    radicalChars?: Record<number, string>;
};
export declare const fractionChars: Record<number, Record<number, string>>;
export declare const radicalChars: Record<number, string>;
export declare function toSuperscript(input: string): string;
export declare function fromSuperscript(input: string): string;
export declare function toSubscript(input: string): string;
export declare function fromSubscript(input: string): string;
export declare function fractionString(num: number, den: number, opts?: FractionOptions): string;
export declare function outputNumber(n: number, opts?: ConstOptions): string;
type VerticalStyle = {
    left: string;
    right: string;
    mid: number;
};
export declare const verticalStyles: {
    bigBraces: {
        left: string;
        right: string;
        mid: number;
    };
    bigBraces1: {
        left: string;
        right: string;
        mid: number;
    };
    brackets: {
        left: string;
        right: string;
        mid: number;
    };
    medBraces: {
        left: string;
        right: string;
        mid: number;
    };
    box: {
        left: string;
        right: string;
        mid: number;
    };
};
export declare function verticalArray(array: string[], style: VerticalStyle): string;
export declare function parse<O extends Operators<any>>(ops: O, s: string): InferOpsType<O>;
export {};
