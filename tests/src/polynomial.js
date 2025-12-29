"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.evaluateMod = exports.sylvesterMatrix = exports.resultant = exports.rothsteinResidues = exports.rothsteinPartial = exports.factorOverK = exports.squareFreeFactorization = exports.hermiteReduce = exports.partialPower = exports.partialFractionsT = void 0;
exports.PolynomialN = PolynomialN;
exports.Polynomial = Polynomial;
exports.multiplicityAt = multiplicityAt;
exports.legendrePolynomial = legendrePolynomial;
exports.legendreTable = legendreTable;
/* eslint-disable custom/no-single-use-local */
/* eslint-disable no-restricted-syntax */
const core_1 = require("./core");
const real_1 = __importStar(require("./real"));
const integer_1 = __importDefault(require("./integer"));
const big_1 = __importDefault(require("./big"));
const gen_1 = __importDefault(require("./gen"));
const complex_1 = __importStar(require("./complex"));
const rational_1 = __importStar(require("./rational"));
const string_1 = require("./string");
const prime_1 = require("./prime");
var factors_1 = require("./factors");
Object.defineProperty(exports, "partialFractionsT", { enumerable: true, get: function () { return factors_1.partialFractionsT; } });
Object.defineProperty(exports, "partialPower", { enumerable: true, get: function () { return factors_1.partialPower; } });
Object.defineProperty(exports, "hermiteReduce", { enumerable: true, get: function () { return factors_1.hermiteReduce; } });
Object.defineProperty(exports, "squareFreeFactorization", { enumerable: true, get: function () { return factors_1.squareFreeFactorization; } });
Object.defineProperty(exports, "factorOverK", { enumerable: true, get: function () { return factors_1.factorOverK; } });
Object.defineProperty(exports, "rothsteinPartial", { enumerable: true, get: function () { return factors_1.rothsteinPartial; } });
Object.defineProperty(exports, "rothsteinResidues", { enumerable: true, get: function () { return factors_1.rothsteinResidues; } });
Object.defineProperty(exports, "resultant", { enumerable: true, get: function () { return factors_1.resultant; } });
Object.defineProperty(exports, "sylvesterMatrix", { enumerable: true, get: function () { return factors_1.sylvesterMatrix; } });
Object.defineProperty(exports, "evaluateMod", { enumerable: true, get: function () { return factors_1.evaluateMod; } });
const sqrt3 = Math.sqrt(3);
const defaultEpsilon = 1e-9;
function PolynomialN(c) {
    for (const i in c) {
        switch (typeof c[i]) {
            case 'number':
                return new polynomialN(c);
            default:
                return new polynomialNT(c);
        }
    }
}
function Polynomial(c) {
    for (const i in c) {
        switch (typeof c[i]) {
            case 'number':
                return new polynomial(c);
            case 'bigint':
                return new polynomialB(c);
            default:
                return new polynomialT(c);
        }
    }
}
//-----------------------------------------------------------------------------
//	helpers
//-----------------------------------------------------------------------------
function insertSorted(arr, value, less = (a, b) => a < b) {
    const i = arr.findIndex(x => less(value, x));
    if (i === -1)
        arr.push(value);
    else
        arr.splice(i, 0, value);
    return arr;
}
function debugString(t, debug = false) {
    if (!t)
        return '0';
    const s = debug && typeof t === 'object' && t && Symbol.for("debug.description") in t ? t[Symbol.for("debug.description")]() : String(t);
    return s.includes(' ') ? `(${s})` : s;
}
function coefficientString(coef, i, x, debug) {
    return (i ? `${coef === 1 ? '' : debugString(coef, debug)}${x}${i > 1 ? (0, string_1.toSuperscript)(i.toString()) : ''}` : `${debugString(coef, debug)}`);
}
function polynomialString(coefficients, x, debug) {
    return coefficients.map((coef, i) => coefficientString(coef, i, x, debug)).reverse().join(' + ');
}
function scalarPolynomialString(coefficients, x, debug) {
    return coefficients.map((coef, i) => coef.sign() === 0 ? '' : (coef.sign() < 0 ? ' - ' : ' + ') + coefficientString(coef.abs(), i, x, debug)).reverse().join('');
}
function lessThan(a, b) {
    return typeof a === 'number' ? a < b
        : typeof a === 'bigint' ? a < BigInt(b)
            : a.lt(a.from(b));
}
//-----------------------------------------------------------------------------
//	sparse or dense arrays
//-----------------------------------------------------------------------------
function arrayCompare(a, b) {
    if (a.length < b.length)
        return -(0, core_1.sign)(b[b.length - 1]);
    if (a.length > b.length)
        return (0, core_1.sign)(a[a.length - 1]);
    for (let i = a.length; i--;) {
        const c = (0, core_1.compare)(a[i], b[i]);
        if (c)
            return c;
    }
    return 0;
}
function arrayCompareT(a, b) {
    if (a.length < b.length)
        return -b[b.length - 1].sign();
    if (a.length > b.length)
        return a[b.length - 1].sign();
    for (let i = a.length; i--;) {
        const c = gen_1.default.compare(a[i], b[i]);
        if (c)
            return c;
    }
    return 0;
}
// scale & rscale
function arrayScale(a, b) {
    for (const i in a)
        a[i] *= b;
    return a;
}
function arrayScaleB(a, b) {
    for (const i in a)
        a[i] *= b;
    return a;
}
function arrayScaleT(a, b) {
    if (typeof b === 'number') {
        for (const i in a)
            a[i] = a[i].scale(b);
    }
    else {
        for (const i in a)
            a[i] = a[i].mul(b);
    }
    return a;
}
function arrayRscaleB(a, b) {
    for (const i in a)
        a[i] /= b;
    return a;
}
function arrayRscaleT(a, b) {
    if (typeof b === 'number')
        return arrayScaleT(a, 1 / b);
    for (const i in a)
        a[i] = a[i].div(b);
    return a;
}
//-----------------------------------------------------------------------------
//	sparse arrays
//-----------------------------------------------------------------------------
function sparseShift(a, n) {
    return n === 0 ? a : n > 0 ? Array(n).concat(a) : a.slice(-n);
}
// clean
/*
function sparseClean(a: number[]) {
    for (const i in a)
        if (a[i] === 0)
            delete a[i];
}
function sparseCleanB(a: bigint[]) {
    for (const i in a)
        if (a[i] === 0n)
            delete a[i];
}
function sparseCleanT<T extends coeffOps<T>>(a: T[]) {
    if (arrayOf(a, has('sign'))) {
        for (const i in a)
            if (a[i].sign() === 0)
                delete a[i];
    } else if (arrayOf(a, hasop('mag'))) {
        for (const i in a)
            if (lessThan(a[i].mag(), defaultEpsilon))
                delete a[i];
    }
}
*/
// trim
function sparseTrim(a) {
    const keys = Object.keys(a);
    let k = keys.length;
    while (k-- && a[+keys[k]] === 0)
        ;
    a.length = k < 0 ? 0 : +keys[k] + 1;
}
function sparseTrimB(a) {
    const keys = Object.keys(a);
    let k = keys.length;
    while (k-- && a[+keys[k]] === 0n)
        ;
    a.length = k < 0 ? 0 : +keys[k] + 1;
}
function sparseTrimT(a) {
    const keys = Object.keys(a);
    let k = keys.length;
    if (k) {
        if ((0, core_1.arrayOf)(a, (0, core_1.hasop)('sign'))) {
            while (k-- && a[+keys[k]].sign() === 0)
                ;
        }
        else if ((0, core_1.arrayOf)(a, (0, core_1.hasop)('mag'))) {
            while (k-- && lessThan(a[+keys[k]].mag(), defaultEpsilon))
                ;
        }
        a.length = k < 0 ? 0 : +keys[k] + 1;
    }
    else {
        a.length = 0;
    }
}
// add
function sparseAdd(a, b) {
    for (const i in b)
        a[i] = (a[i] || 0) + b[i];
    return a;
}
function sparseAddB(a, b) {
    for (const i in b)
        a[i] = (a[i] || 0n) + b[i];
    return a;
}
function sparseAddT(a, b) {
    for (const i in b)
        a[i] = a[i] ? a[i].add(b[i]) : b[i];
    return a;
}
// sub
function sparseSub(a, b) {
    for (const i in b)
        a[i] = (a[i] || 0) - b[i];
    return a;
}
function sparseSubB(a, b) {
    for (const i in b)
        a[i] = (a[i] || 0n) - b[i];
    return a;
}
function sparseSubT(a, b) {
    for (const i in b)
        a[i] = a[i] ? a[i].sub(b[i]) : b[i].neg();
    return a;
}
// evaluate
function sparseEvaluate(c, x, monomial) {
    let result = 0;
    let xpow = 1;
    let exp = 0;
    for (const i in c) {
        const exp1 = +i;
        while (exp < exp1) {
            xpow *= x;
            ++exp;
        }
        result += c[i] * xpow;
    }
    if (monomial) {
        const exp1 = c.length;
        while (exp < exp1) {
            xpow *= x;
            ++exp;
        }
        result += xpow;
    }
    return result;
}
function sparseEvaluateB(c, x) {
    let result = 0n;
    let xpow = 1n;
    let exp = 0;
    for (const i in c) {
        const exp1 = +i;
        while (exp < exp1) {
            xpow *= x;
            ++exp;
        }
        result += c[i] * xpow;
    }
    return result;
}
function sparseEvaluateT(c, x, monomial) {
    let result = c[0];
    let exp = 1;
    let xpow = x;
    for (const i in c) {
        const exp1 = +i;
        if (exp1 === 0)
            continue;
        while (exp < exp1) {
            xpow = xpow.mul(x);
            ++exp;
        }
        result = result.add(c[exp1].mul(xpow));
    }
    if (monomial) {
        const exp1 = c.length;
        while (exp < exp1) {
            xpow = xpow.mul(x);
            ++exp;
        }
        result = result.add(xpow);
    }
    return result;
}
function sparseEvaluateNT(c, x, monomial) {
    let result = x.from(0);
    let xpow = x.from(1);
    let exp = 0;
    for (const i in c) {
        const exp1 = +i;
        while (exp < exp1) {
            xpow = xpow.mul(x);
            ++exp;
        }
        result = result.add(xpow.scale(c[i]));
    }
    if (monomial) {
        const exp1 = c.length;
        while (exp < exp1) {
            xpow = xpow.mul(x);
            ++exp;
        }
        result = result.add(xpow);
    }
    return result;
}
// multiply
function sparseMultiply(a, b, monomial) {
    const r = [];
    for (const i in a) {
        const ai = a[i];
        for (const j in b) {
            const k = +i + +j;
            r[k] = (r[k] ?? 0) + ai * b[j];
        }
    }
    if (monomial) {
        for (const i in a) {
            const k = +i + b.length;
            r[k] = (r[k] ?? 0) + a[i];
        }
        for (const i in b) {
            const k = +i + a.length;
            r[k] = (r[k] ?? 0) + b[i];
        }
    }
    return r;
}
function sparseMultiplyB(a, b) {
    const r = [];
    for (const i in a) {
        const ai = a[i];
        for (const j in b) {
            const k = +i + +j;
            r[k] = (r[k] ?? 0n) + ai * b[j];
        }
    }
    return r;
}
function sparseMultiplyT(a, b, monomial) {
    const r = [];
    for (const i in a) {
        const ai = a[i];
        for (const j in b) {
            const k = +i + +j;
            r[k] = r[k] ? r[k].add(ai.mul(b[j])) : ai.mul(b[j]);
        }
    }
    if (monomial) {
        for (const i in a) {
            const k = +i + b.length;
            r[k] = r[k] ? r[k].add(a[i]) : a[i];
        }
        for (const i in b) {
            const k = +i + a.length;
            r[k] = r[k] ? r[k].add(b[i]) : b[i];
        }
    }
    return r;
}
// divmod
function sparseDivMod(a, b, monomial) {
    const blen = b.length - (monomial ? 0 : 1);
    const qlen = Math.max(a.length - b.length + 1, 0);
    const bt = b[blen];
    const q = [];
    for (let i = qlen; i--;) {
        let at = a[i + blen];
        if (at) {
            if (!monomial)
                at /= bt;
            q[i] = at;
            for (const j in b)
                a[i + +j] = (a[i + +j] || 0) - b[j] * at;
        }
    }
    a.length = blen;
    sparseTrim(a);
    return q;
}
function sparseDivModB(a, b) {
    const blen = b.length - 1;
    const qlen = Math.max(a.length - blen, 0);
    const q = [];
    for (let i = qlen; i--;) {
        const at = a[i + blen];
        if (at) {
            q[i] = at;
            for (const j in b)
                a[i + +j] = (a[i + +j] || 0n) - b[j] * at;
        }
    }
    a.length = blen;
    sparseTrimB(a);
    return q;
}
function sparseDivModT(a, b, monomial) {
    const blen = monomial ? b.length : b.length - 1;
    const qlen = Math.max(a.length - b.length + 1, 0);
    const bt = b[blen];
    const q = [];
    for (let i = qlen; i--;) {
        let at = a[i + blen];
        if (at) {
            if (!monomial)
                at = at.div(bt);
            q[i] = at;
            for (const j in b)
                a[i + +j] = a[i + +j] ? a[i + +j].sub(at.mul(b[j])) : at.mul(b[j]).neg();
        }
    }
    a.length = blen;
    sparseTrimT(a);
    return q;
}
// pseudo-division
function sparsePseudoRem(a, b) {
    const blen = b.length - 1;
    const qlen = Math.max(a.length - blen, 0);
    const bt = b[blen];
    for (let i = qlen; i--;) {
        const at = a[i + blen];
        if (at) {
            arrayScale(a, bt);
            for (const j in b)
                a[i + +j] = (a[i + +j] || 0) - b[j] * at;
        }
    }
    a.length = blen;
    sparseTrim(a);
}
function sparsePseudoRemB(a, b) {
    const blen = b.length - 1;
    const qlen = Math.max(a.length - blen, 0);
    const bt = b[blen];
    for (let i = qlen; i--;) {
        const at = a[i + blen];
        if (at) {
            arrayScaleB(a, bt);
            for (const j in b)
                a[i + +j] = (a[i + +j] || 0n) - b[j] * at;
        }
    }
    let rlen = blen;
    while (rlen > 0 && !a[rlen - 1])
        rlen--;
    a.length = rlen;
}
function sparsePseudoRemT(a, b) {
    const blen = b.length - 1;
    const qlen = Math.max(a.length - blen, 0);
    const bt = b[blen];
    for (let i = qlen; i--;) {
        const at = a[i + blen];
        if (at) {
            arrayScaleT(a, bt);
            for (const j in b)
                a[i + +j] = a[i + +j] ? a[i + +j].sub(at.mul(b[j])) : at.mul(b[j]).neg();
        }
    }
    a.length = blen;
    sparseTrimT(a);
}
//-----------------------------------------------------------------------------
//	dense arrays
//-----------------------------------------------------------------------------
/*
// trim

function denseTrim(a: (number[])|(bigint[])) {
    while (a.length && !a[a.length - 1])
        a.pop();
}
function denseTrimT<T extends ops<T>>(a: T[]) {
    if (arrayOf(a, has('sign'))) {
        while (a.length && a[a.length - 1].sign() === 0)
            a.pop();
    } else {
        while (a.length && lessThan(a[a.length - 1].mag(), defaultEpsilon))
            a.pop();
    }
}

// add

function denseAdd(a: number[], b: number[]): number[] {
    for (const i in b)
        a[i] += b[i];
    return a;
}
function denseAddB(a: bigint[], b: bigint[]): bigint[] {
    for (const i in b)
        a[i] += b[i];
    return a;
}
function denseAddT<T extends ops<T>>(a: T[], b: T[]): T[] {
    for (const i in b)
        a[i] = a[i].add(b[i]);
    return a;
}

// sub

function denseSub(a: number[], b: number[]): number[] {
    for (const i in b)
        a[i] -= b[i];
    return a;
}
function denseSubB(a: bigint[], b: bigint[]): bigint[] {
    for (const i in b)
        a[i] -= b[i];
    return a;
}
function denseSubT<T extends ops<T>>(a: T[], b: T[]): T[] {
    for (const i in b)
        a[i] = a[i].sub(b[i]);
    return a;
}

// evaluate

function denseEvaluate(c: number[], t: number, monomial: boolean): number {
    let i = c.length;
    let r = c[--i];
    if (monomial)
        r += t;
    while (i--)
        r = r * t + c[i];
    return r;
}
function denseEvaluateB(c: bigint[], x: bigint): bigint {
    let i = c.length;
    let r = c[--i];
    while (i--)
        r = r * x + c[i];
    return r;
}
function denseEvaluateT<T extends ops<T>>(c: T[], t: T, monomial: boolean): T {
    let i = c.length;
    let r = c[--i];
    if (monomial)
        r = r.add(t);
    while (i--)
        r = r.mul(t).add(c[i]);
    return r;
}

function denseEvaluateNT<T extends ops<T> & has<'from'>>(c: number[], t: T, monomial: boolean): T {
    let i = c.length;
    let r = t.from(c[--i]);
    if (monomial)
        r = r.add(t);
    while (i--)
        r = r.mul(t).add(t.from(c[i]));
    return r;
}
function denseEvaluateBR(c: bigint[], x: rationalB): rationalB {
    let i = c.length;
    let acc		= c[--i];
    let denPow = 1n;
    while (i--) {
        denPow *= x.den;
        acc = acc * x.num + c[i] * denPow;
    }
    return new rationalB(acc, denPow);
}

// multiply

function denseMultiply(a: number[], b: number[], monomial: boolean) {
    const r = new Array<number>(a.length + b.length).fill(0);
    for (let i = 0; i < a.length; i++)
        for (let j = 0; j < b.length; j++)
            r[i + j] += a[i] * b[j];

    if (monomial) {
        for (let i = 0; i < a.length; i++)
            r[i + b.length] += a[i];

        for (let i = 0; i < b.length; i++)
            r[i + a.length] += b[i];
    }
}
function denseMultiplyB(a: bigint[], b: bigint[]) {
    const r = new Array<bigint>(a.length + b.length - 1).fill(0n);
    for (let i = 0; i < a.length; i++)
        for (let j = 0; j < b.length; j++)
            r[i + j] += a[i] * b[j];
    return r;
}
function denseMultiplyT<T extends ops<T>>(a: T[], b: T[], monomial: boolean) {
    const zero = a[0].scale(0);
    const blen = monomial ? b.length : b.length - 1;
    return Array.from({ length: a.length + blen }, (_, k) => {
        let sum = zero;
        for (let i = Math.max(0, k - blen); i <= Math.min(a.length - 1, k); i++)
            sum = sum.add(a[i].mul(b[k - i]));
        return sum;
    });
}

// divmod

function denseDivMod(a: number[], b: number[], monomial: boolean) {
    const blen	= b.length - (monomial ? 0 : 1);
    const qlen	= Math.max(a.length - b.length + 1, 0);
    const bt	= b[blen];
    const q: number[] = [];

    for (let i = qlen; i--;) {
        let at = a[i + blen];
        if (at) {
            if (!monomial)
                at /= bt;
            q[i] = at;
            for (const j in b)
                a[i + +j] -= b[j] * at;
        } else {
            q[i] = 0;
        }
    }
    a.length = blen;
    denseTrim(a);
    return q;
}
function denseDivModB(a: bigint[], b: bigint[], monomial: boolean) {
    const blen	= b.length - (monomial ? 0 : 1);
    const qlen	= Math.max(a.length - b.length + 1, 0);
    const bt	= b[blen];
    const q: bigint[] = [];

    for (let i = qlen; i--;) {
        let at = a[i + blen];
        if (at) {
            if (!monomial)
                at /= bt;
            q[i] = at;
            for (const j in b)
                a[i + +j] -= b[j] * at;
        } else {
            q[i] = 0n;
        }
    }
    a.length = blen;
    denseTrim(a);
    return q;
}
function denseDivModT<T extends ops<T>>(a: T[], b: T[], monomial: boolean) {
    const blen	= b.length - (monomial ? 0 : 1);
    const qlen	= Math.max(a.length - b.length + 1, 0);
    const bt	= b[blen];
    const q: T[] = [];

    for (let i = qlen; i--;) {
        let at = a[i + blen];
        if (at) {
            if (!monomial)
                at = at.div(bt);
            q[i] = at;
            for (const j in b)
                a[i + +j] = a[i + +j].sub(b[j].mul(at));
        } else {
            q[i] = at;
        }
    }
    a.length = blen;
    denseTrimT(a);
    return q;
}


// pseudo-division

function densePseudoRem(a: number[], b: number[]) {
    const blen	= b.length - 1;
    const qlen	= Math.max(a.length - blen, 0);
    const bt	= b[blen];

    for (let i = qlen; i--;) {
        const at = a[i + blen];
        if (at) {
            arrayScale(a, bt);
            for (const j in b)
                a[i + +j] -= b[j] * at;
        }
    }
    a.length = blen;
    sparseTrim(a);
}
function densePseudoRemB(a: bigint[], b: bigint[]) {
    const blen	= b.length - 1;
    const qlen	= Math.max(a.length - blen, 0);
    const bt	= b[blen];

    for (let i = qlen; i--;) {
        const at = a[i + blen];
        if (at) {
            arrayScaleB(a, bt);
            for (const j in b)
                a[i + +j] -= b[j] * at;
        }
    }
    a.length = blen;
    sparseTrimB(a);
}
function densePseudoRemT<T extends ops<T>>(a: T[], b: T[]) {
    const blen	= b.length - 1;
    const qlen	= Math.max(a.length - blen, 0);
    const bt	= b[blen];

    for (let i = qlen; i--;) {
        const at = a[i + blen];
        if (at) {
            arrayScaleT(a, bt);
            for (const j in b)
                a[i + +j] = a[i + +j].sub(b[j].mul(at));
        }
    }
    a.length = blen;
    sparseTrimT(a);
}
*/
//-----------------------------------------------------------------------------
//	Polynomial with real coefficients
//-----------------------------------------------------------------------------
class polynomial {
    c;
    constructor(c) {
        this.c = c;
        sparseTrim(c);
    }
    is(g) { return (0, core_1.arrayOf)(this.c, g); }
    degree() { return this.c.length - 1; }
    leadCoeff() { return this.c[this.c.length - 1] ?? 0; }
    dup() { return new polynomial(this.c.slice()); }
    from(x) { return new polynomial([x]); }
    shift(n) { return new polynomial(sparseShift(this.c, n)); }
    ipow(b) { return gen_1.default.ipow(this, b); }
    evaluate(t) {
        if (typeof t === 'number')
            return sparseEvaluate(this.c, t, false);
        if (Array.isArray(t))
            return t.map(t => sparseEvaluate(this.c, t, false));
        return sparseEvaluateNT(this.c, t, false);
    }
    deriv() {
        return new polynomial(this.c.slice(1).map((v, i) => v * (i + 1)));
    }
    add(b) {
        if (typeof b === 'number')
            return new polynomial([(this.c[0] ?? 0) + b, ...this.c.slice(1)]);
        return new polynomial(sparseAdd(this.c.slice(), b.c));
    }
    sub(b) {
        if (typeof b === 'number')
            return new polynomial([(this.c[0] ?? 0) - b, ...this.c.slice(1)]);
        return new polynomial(sparseSub(this.c.slice(), b.c));
    }
    scale(b) {
        return new polynomial(this.c.map(a => a * b));
    }
    rscale(b) {
        return this.scale(1 / b);
    }
    mul(b) {
        return new polynomial(sparseMultiply(this.c, b.c, false)); //or denseMultiply(this.c, b.c, false);
    }
    div(b) {
        return this.dup().divmod(b);
    }
    selfAdd(b) {
        if (typeof b === 'number') {
            this.c[0] = (this.c[0] ?? 0) + b;
        }
        else {
            sparseAdd(this.c, b.c);
            sparseTrim(this.c);
        }
    }
    selfSub(b) {
        if (typeof b === 'number') {
            this.c[0] = (this.c[0] ?? 0) - b;
        }
        else {
            sparseSub(this.c, b.c);
            sparseTrim(this.c);
        }
    }
    selfScale(b) {
        arrayScale(this.c, b);
    }
    selfRscale(b) {
        arrayScale(this.c, 1 / b);
    }
    divmod(b) {
        return new polynomial(sparseDivMod(this.c, b.c, false)); // or denseDivmod(a, b);
    }
    pseudoRemainder(b) {
        sparsePseudoRem(this.c, b.c);
    }
    content() {
        return real_1.default.gcd(...this.c);
    }
    abs() {
        return this.leadCoeff() < 0 ? new polynomial(this.c.map(v => -v)) : this;
    }
    neg() {
        return new polynomial(this.c.map(v => -v));
    }
    sign() {
        return Math.sign(this.leadCoeff());
    }
    compare(b) {
        return arrayCompare(this.c, b.c);
    }
    eq(b) {
        return this.compare(b) === 0;
    }
    lt(b) {
        return this.compare(b) < 0;
    }
    normalise(epsilon = defaultEpsilon) {
        let i = this.c.length - 1;
        while (i && Math.abs(this.c[i]) < epsilon)
            i--;
        const f = 1 / this.c[i];
        return new polynomialN(this.c.slice(0, i).map(v => v * f));
    }
    rationalRoots() {
        return rationalRootsN(this.dup());
    }
    realRoots(epsilon = defaultEpsilon) {
        return this.normalise().realRoots(epsilon);
    }
    allRoots(epsilon = defaultEpsilon) {
        return this.normalise(epsilon).allRoots(epsilon);
    }
    refine_roots(x, count = 1) {
        const d1 = this.deriv();
        const d2 = d1.deriv();
        return x.map(x => halley(this, d1, d2, x, count));
    }
    map(func) {
        return Polynomial(this.c.map((c, i) => func(c, i)));
    }
    toString(x = 'x', debug = false) {
        if (this.c.length < 2)
            return this.c.length ? String(this.c[0]) : '0';
        return coefficientString(this.leadCoeff(), this.degree(), x, debug) + this.c.slice(0, -1).map((coef, i) => coef === 0 ? '' : (coef < 0 ? ' - ' : ' + ') + coefficientString(Math.abs(coef), i, x, debug)).reverse().join('');
    }
    [Symbol.for("debug.description")]() { return this.toString('x', true); }
}
//-----------------------------------------------------------------------------
//	Normalised Polynomial with implicit leading coefficient of 1
//-----------------------------------------------------------------------------
class polynomialN {
    c;
    constructor(c) {
        this.c = c;
    }
    is(g) { return (0, core_1.arrayOf)(this.c, g); }
    degree() { return this.c.length; }
    dup() { return new polynomialN(this.c.slice()); }
    evaluate(t) {
        if (typeof t === 'number')
            return sparseEvaluate(this.c, t, true);
        if (Array.isArray(t))
            return t.map(t => sparseEvaluate(this.c, t, true));
        return sparseEvaluateNT(this.c, t, true);
    }
    deriv() {
        return new polynomial([...this.c.slice(1).map((v, i) => v * (i + 1)), this.c.length]);
    }
    mul(b) {
        return new polynomialN(sparseMultiply(this.c, b.c, true));
    }
    divmod(b) {
        return new polynomial(sparseDivMod(this.c, b.c, true));
    }
    rationalRoots() {
        return rationalRootsN(new polynomial([...this.c, 1]));
    }
    realRoots(epsilon = defaultEpsilon) {
        return normPolyRealRoots(this.c, epsilon);
    }
    allRoots(epsilon = defaultEpsilon) {
        return normPolyComplexRoots(this.c, epsilon);
    }
    refine_roots(x, count = 1) {
        const d1 = this.deriv();
        const d2 = d1.deriv();
        return x.map(x => halley(this, d1, d2, x, count));
    }
    toString(x = 'x', debug = false) {
        return this.degree() === 0 ? '1'
            : coefficientString(1, this.degree(), x, debug) + this.c.map((coef, i) => coef === 0 ? '' : (coef < 0 ? ' - ' : ' + ') + coefficientString(Math.abs(coef), i, x, debug)).reverse().join('');
    }
    [Symbol.for("debug.description")]() { return this.toString('x', true); }
}
//-----------------------------------------------------------------------------
//	Polynomial with bigint coefficients
//-----------------------------------------------------------------------------
class polynomialB {
    c;
    constructor(c) {
        this.c = c;
        sparseTrimB(this.c);
    }
    is(g) { return (0, core_1.arrayOf)(this.c, g); }
    degree() { return this.c.length - 1; }
    leadCoeff() { return this.c[this.c.length - 1] ?? 0n; }
    dup() { return new polynomialB(this.c.slice()); }
    from(x) { return new polynomialB([BigInt(x)]); }
    shift(n) { return new polynomialB(sparseShift(this.c, n)); }
    ipow(b) { return gen_1.default.ipow(this, b); }
    evaluate(t) {
        if (Array.isArray(t))
            return t.map(t => sparseEvaluateB(this.c, t));
        if ((0, core_1.isInstance)(t, rational_1.rationalB))
            return (0, rational_1.rationalB)(sparseEvaluateB(this.c, t.num), sparseEvaluateB(this.c, t.den));
        return sparseEvaluateB(this.c, t);
    }
    deriv() {
        return new polynomialB(this.c.slice(1).map((v, i) => v * BigInt(i + 1)));
    }
    add(b) {
        if (typeof b === 'bigint')
            return new polynomialB([(this.c[0] ?? 0n) + b, ...this.c.slice(1)]);
        return new polynomialB(sparseAddB(this.c.slice(), b.c));
    }
    sub(b) {
        if (typeof b === 'bigint')
            return new polynomialB([(this.c[0] ?? 0n) - b, ...this.c.slice(1)]);
        return new polynomialB(sparseSubB(this.c.slice(), b.c));
    }
    scale(b) {
        return new polynomialB(this.c.map(a => a * b));
    }
    rscale(b) {
        return new polynomialB(this.c.map(a => a / b));
    }
    mul(b) {
        return new polynomialB(sparseMultiplyB(this.c, b.c));
    }
    div(b) {
        return this.dup().divmod(b);
    }
    selfAdd(b) {
        if (typeof b === 'bigint') {
            this.c[0] = (this.c[0] ?? 0n) + b;
        }
        else {
            sparseAddB(this.c, b.c);
            sparseTrimB(this.c);
        }
    }
    selfSub(b) {
        if (typeof b === 'bigint') {
            this.c[0] = (this.c[0] ?? 0n) - b;
        }
        else {
            sparseSubB(this.c, b.c);
            sparseTrimB(this.c);
        }
    }
    selfScale(b) {
        arrayScaleB(this.c, b);
    }
    selfRscale(b) {
        arrayRscaleB(this.c, b);
    }
    divmod(b) {
        return new polynomialB(sparseDivModB(this.c, b.c)); // or denseDivmodB(a, b.c);
    }
    pseudoRemainder(b) {
        sparsePseudoRemB(this.c, b.c);
    }
    content() {
        return big_1.default.gcd(...this.c);
    }
    abs() {
        return this.leadCoeff() < 0n ? new polynomialB(this.c.map(v => -v)) : this;
    }
    neg() {
        return new polynomialB(this.c.map(v => -v));
    }
    sign() {
        return big_1.default.sign(this.leadCoeff());
    }
    compare(b) {
        return arrayCompare(this.c, b.c);
    }
    eq(b) {
        return this.compare(b) === 0;
    }
    lt(b) {
        return this.compare(b) < 0;
    }
    normalise(epsilon = defaultEpsilon) {
        let i = this.c.length - 1;
        while (i && big_1.default.abs(this.c[i]) < epsilon)
            i--;
        const d = this.c[i];
        return new polynomialNT(this.c.slice(0, i).map(v => (0, rational_1.rationalB)(v, d)));
    }
    rationalRoots() {
        return rationalRootsB(this.dup());
    }
    realRoots() {
        const p = this.dup();
        const roots = rationalRootsB(p);
        if (p.degree() > 0)
            roots.push(...p.normalise().realRoots());
        return roots;
    }
    allRoots() {
        return undefined;
    }
    refine_roots(x, count = 1) {
        return this.normalise().refine_roots(x, count);
    }
    map(func) {
        return Polynomial(this.c.map((c, i) => func(c, i)));
    }
    toString(x = 'x', debug = false) {
        return this.c.map((coef, i) => {
            return i === this.degree() ? coefficientString(coef, i, x, debug)
                : (coef < 0n ? ' - ' : ' + ') + coefficientString(big_1.default.abs(coef), i, x, debug);
        }).reverse().join('');
    }
    [Symbol.for("debug.description")]() { return this.toString('x', true); }
}
//-----------------------------------------------------------------------------
//	Polynomial with generic coefficients supporting ops interface
//-----------------------------------------------------------------------------
class polynomialT {
    c;
    constructor(c) {
        this.c = c;
        sparseTrimT(c);
    }
    is(g) { return (0, core_1.arrayOf)(this.c, g); }
    degree() { return this.c.length - 1; }
    leadCoeff() { return this.c[this.c.length - 1]; }
    dup() { return new polynomialT(this.c.slice()); }
    from(x) {
        if (!real_1.default.is(x))
            return new polynomialT([x]);
        for (const i of this.c) {
            if ((0, core_1.has)('from')(i))
                return new polynomialT([i.from(x)]);
            throw new Error("Cannot create polynomial from type");
        }
        throw new Error("Cannot create polynomial from empty type");
    }
    shift(n) { return new polynomialT(sparseShift(this.c, n)); }
    ipow(b) { return gen_1.default.ipow(this, b); }
    evaluate(t) {
        if (Array.isArray(t))
            return t.map(t => sparseEvaluateT(this.c, t, false));
        return sparseEvaluateT(this.c, t, false);
    }
    deriv() {
        return new polynomialT(this.c.slice(1).map((v, i) => v.scale(i + 1)));
    }
    add(b) {
        if (b instanceof polynomialT)
            return new polynomialT(sparseAddT(this.c.slice(), b.c));
        return new polynomialT([this.c[0].add(b), ...this.c.slice(1)]);
    }
    sub(b) {
        if (b instanceof polynomialT)
            return new polynomialT(sparseSubT(this.c.slice(), b.c));
        return new polynomialT([this.c[0].sub(b), ...this.c.slice(1)]);
    }
    scale(b) {
        return new polynomialT(typeof b === 'number' ? this.c.map(a => a.scale(b)) : this.c.map(a => a.mul(b)));
    }
    rscale(b) {
        return typeof b === 'number' ? this.scale(1 / b) : new polynomialT(this.c.map(a => a.div(b)));
    }
    mul(b) {
        return new polynomialT(sparseMultiplyT(this.c, b.c, false));
    }
    div(b) {
        return this.dup().divmod(b);
    }
    selfAdd(b) {
        if (b instanceof polynomialT) {
            sparseAddT(this.c, b.c);
        }
        else {
            this.c[0] = this.c[0].add(b);
            sparseTrimT(this.c);
        }
    }
    selfSub(b) {
        if (b instanceof polynomialT) {
            sparseSubT(this.c, b.c);
        }
        else {
            this.c[0] = this.c[0].sub(b);
            sparseTrimT(this.c);
        }
    }
    selfScale(b) {
        arrayScaleT(this.c, b);
    }
    selfRscale(b) {
        arrayRscaleT(this.c, b);
    }
    divmod(b) {
        return new polynomialT(sparseDivModT(this.c, b.c, false));
    }
    pseudoRemainder(b) {
        sparsePseudoRemT(this.c, b.c);
    }
    content() {
        if ((0, core_1.arrayOf)(this.c, core_1.isScalarExt))
            return gen_1.default.gcd(...this.c);
        return undefined;
    }
    abs() {
        return (0, core_1.arrayOf)(this.c, (0, core_1.has)('sign')) && this.c[this.c.length - 1].sign() < 0 ? new polynomialT(this.c.map(v => v.neg())) : this;
    }
    neg() {
        return new polynomialT(this.c.map(v => v.neg()));
    }
    sign() {
        return this.c.length < 1 ? 0 : (0, core_1.arrayOf)(this.c, (0, core_1.has)('sign')) ? this.c[this.c.length - 1].sign() : NaN;
    }
    compare(b) {
        return (0, core_1.arrayOf)(this.c, (0, core_1.has)('lt')) ? arrayCompareT(this.c, b.c) : NaN;
    }
    eq(b) {
        return this.compare(b) === 0;
    }
    lt(b) {
        return this.compare(b) < 0;
    }
    normalise(epsilon = defaultEpsilon) {
        let i = this.c.length - 1;
        if ((0, core_1.arrayOf)(this.c, (0, core_1.hasop)('mag'))) {
            while (i && lessThan(this.c[i].mag(), epsilon))
                i--;
        }
        else if (this.is((0, core_1.has)('sign'))) {
            while (i && this.c[i].sign() === 0)
                i--;
        }
        const f = this.c[i];
        return new polynomialNT(this.c.slice(0, i).map(v => v.div(f)));
    }
    rationalRoots() {
        if ((0, core_1.arrayOf)(this.c, core_1.isScalar)) {
            if ((0, core_1.arrayOf)(this.c, v => (0, core_1.isInstance)(v, rational_1.rationalB))) {
                const m = big_1.default.lcm(...this.c.map((v) => v.den));
                const p2 = new polynomialB(this.c.map((v) => v.num * (m / v.den)));
                return rationalRootsB(p2);
            }
            else if ((0, core_1.arrayOf)(this.c, v => (0, core_1.isInstance)(v, rational_1.default))) {
                const m = integer_1.default.lcm(...this.c.map((v) => v.den));
                const p2 = new polynomial(this.c.map((v) => v.num * (m / v.den)));
                return rationalRootsN(p2);
            }
            else if ((0, core_1.arrayOf)(this.c, rational_1.canMakeRational)) {
                const p2 = this.c.map((c) => rational_1.rationalB.from(c, 1n << 32n));
                const m = big_1.default.lcm(...p2.map(v => v.den));
                const p4 = new polynomialB(p2.map(v => v.num * (m / v.den)));
                return rationalRootsB(p4);
            }
        }
        return undefined;
    }
    realRoots(epsilon = defaultEpsilon) {
        return this.normalise().realRoots(epsilon);
    }
    allRoots(epsilon = defaultEpsilon) {
        return this.normalise().allRoots(epsilon); // as complexRoots<T>;
    }
    map(func) {
        return Polynomial(this.c.map((c, i) => func(c, i)));
    }
    refine_roots(x, count = 1) {
        const d1 = this.deriv();
        const d2 = d1.deriv();
        return x.map(x => halleyT(this, d1, d2, x, count));
    }
    toString(x = 'x', debug = false) {
        if (this.c.length < 2)
            return this.c.length ? String(this.c[0]) : '0';
        const c = this.c.slice(0, -1);
        const s = coefficientString(this.leadCoeff(), c.length, x, debug);
        if ((0, core_1.arrayOf)(c, (0, core_1.hasop)('abs')))
            return s + scalarPolynomialString(c, x, debug);
        const s2 = polynomialString(c, x, debug);
        return s2 ? s + ' + ' + s2 : s;
    }
}
//-----------------------------------------------------------------------------
//	Normalised General Polynomial with implicit leading coefficient of 1
//-----------------------------------------------------------------------------
class polynomialNT {
    c;
    constructor(c) {
        this.c = c;
    }
    is(g) { return (0, core_1.arrayOf)(this.c, g); }
    degree() { return this.c.length; }
    dup() { return new polynomialNT(this.c.slice()); }
    evaluate(t) {
        if (Array.isArray(t))
            return t.map(t => sparseEvaluateT(this.c, t, true));
        return sparseEvaluateT(this.c, t, true);
    }
    deriv() {
        if ((0, core_1.isScalar)(this.c[0]))
            return new polynomialT([...this.c.slice(1).map((v, i) => v.scale(i + 1)), this.c[0].from(this.c.length)]);
        return undefined;
    }
    mul(b) {
        return new polynomialNT(sparseMultiplyT(this.c, b.c, true));
    }
    divmod(b) {
        return new polynomialT(sparseDivModT(this.c, b.c, true));
    }
    rationalRoots() {
        if ((0, core_1.arrayOf)(this.c, core_1.isScalar)) {
            if ((0, core_1.arrayOf)(this.c, v => (0, core_1.isInstance)(v, rational_1.rationalB))) {
                const m = big_1.default.lcm(...this.c.map((v) => v.den));
                const p2 = new polynomialB([...this.c.map((v) => v.num * (m / v.den)), m]);
                return rationalRootsB(p2);
            }
            else if ((0, core_1.arrayOf)(this.c, v => (0, core_1.isInstance)(v, rational_1.default))) {
                const m = integer_1.default.lcm(...this.c.map((v) => v.den));
                const p2 = new polynomial([...this.c.map((v) => v.num * (m / v.den)), m]);
                return rationalRootsN(p2);
            }
            else {
                const p2 = this.c.map(c => rational_1.rationalB.from(c, 1n << 32n));
                const m = big_1.default.lcm(...p2.map(v => v.den));
                const p4 = new polynomialB([...p2.map(v => v.num * (m / v.den)), m]);
                return rationalRootsB(p4);
            }
        }
        return undefined;
    }
    realRoots(epsilon = defaultEpsilon) {
        if (this.is(core_1.isScalar)) {
            const eps = this.c[0].from(epsilon);
            if (this.is((0, core_1.hasopT)('rpow')))
                return normPolyRealRootsT(this.c, eps);
            else
                return this.refine_roots(sturmIsolateIntervalsT(this, eps).map(i => bisectRootT(this, i.min, i.max, eps)));
        }
        return undefined;
    }
    allRoots(epsilon = defaultEpsilon) {
        if ((0, core_1.arrayOf)(this.c, core_1.isScalarExt))
            return normPolyComplexRootsT(this.c, this.c[0].from(epsilon));
        return undefined;
    }
    refine_roots(x, count = 1) {
        const d1 = this.deriv();
        const d2 = d1.deriv();
        return x.map(x => halleyT(this, d1, d2, x, count));
    }
    toString(x = 'x', debug = false) {
        if (this.degree() === 0)
            return '1';
        const s = coefficientString(1, this.degree(), x, debug);
        if ((0, core_1.arrayOf)(this.c, (0, core_1.hasop)('abs')))
            return s + scalarPolynomialString(this.c, x, debug);
        const s2 = polynomialString(this.c, x, debug);
        return s2 ? s + ' + ' + s2 : s;
    }
    [Symbol.for("debug.description")]() { return this.toString('x', true); }
}
//-----------------------------------------------------------------------------
//	lagrange root bounds
//-----------------------------------------------------------------------------
function sumTop2(a) {
    if (a.length < 2)
        return a.length === 1 ? a[0] : 0;
    let max1 = -Infinity, max2 = -Infinity;
    for (const v of a) {
        if (v > max1) {
            max2 = max1;
            max1 = v;
        }
        else if (v > max2) {
            max2 = v;
        }
    }
    return max1 + max2;
}
/*
function sumTop2B(a: bigint[]) {
    if (a.length < 2)
        return a.length === 1 ? a[0] : 0n;
    let max1 = a[0], max2 = a[0];
    for (const v of a) {
        if (max1 < v) {
            max2 = max1;
            max1 = v;
        } else if (max2 < v) {
            max2 = v;
        }
    }
    return max1 + max2;
}
*/
function sumTop2T(zero, a) {
    if (a.length < 2)
        return a.length === 1 ? a[0] : zero;
    let max1 = a[0], max2 = a[0];
    for (const v of a) {
        if (max1.lt(v)) {
            max2 = max1;
            max1 = v;
        }
        else if (max2.lt(v)) {
            max2 = v;
        }
    }
    return max1.add(max2);
}
function lagrangeImproved(c) {
    const N = c.length;
    if ((0, core_1.arrayOf)(c, real_1.default.is))
        return sumTop2(c.map((c, i) => Math.pow(Math.abs(c), 1 / (N - i))));
    return sumTop2(c.map((c, i) => Math.pow(c.abs(), 1 / (N - i))));
}
/*
function lagrangeImprovedB(c: bigint[]): bigint {
    const N = c.length;
    return sumTop2B(c.map((c, i) => rootB(Big.abs(c), N - i)));
}
*/
function lagrangeImprovedT(c) {
    const N = c.length;
    const zero = (0, core_1.isInstance)(c[0], complex_1.complexT) ? c[0].r.from(0) : c[0].from(0);
    return sumTop2T(zero, c.map((c, i) => c.abs().rpow(1, (N - i))));
}
function realBound(c) {
    const N = c.length;
    const terms = c.map((c, i) => Math.pow(Math.abs(c), 1 / (N - i)));
    return new real_1.default.extent(-sumTop2(terms.filter((x, i) => (i % 2 == 0) === (c[i] > 0))), sumTop2(terms.filter((x, i) => c[i] < 0)));
}
function realBoundT(k) {
    const N = k.c.length;
    const terms = k.c.map((c, i) => (0, real_1.asScalarExt)(Number(c)).rpow(1, (N - i)));
    const zero = terms[0].from(0);
    return new gen_1.default.extent(sumTop2T(zero, terms.filter((x, i) => (i % 2 == 0) === (k.c[i].sign() > 0))).neg(), sumTop2T(zero, terms.filter((x, i) => k.c[i].sign() < 0)));
}
//-----------------------------------------------------------------------------
//	root refinement
//-----------------------------------------------------------------------------
function bisectRoot(p, min, max, tol, maxIter = 10) {
    let fmin = p.evaluate(min);
    for (let i = 0; i < maxIter && max - min > tol; i++) {
        const mid = 0.5 * (min + max);
        const fmid = p.evaluate(mid);
        // choose side where sign changes
        if (fmin * fmid <= 0) {
            max = mid;
        }
        else {
            min = mid;
            fmin = fmid;
        }
    }
    return 0.5 * (min + max);
}
function bisectRootT(p, min, max, threshold, maxIter = 10) {
    let fmin = p.evaluate(min);
    for (let i = 0; i < maxIter && threshold.lt(max.sub(min)); i++) {
        const mid = min.add(max).scale(0.5);
        const fmid = p.evaluate(mid);
        // choose side where sign changes
        if (fmin.sign() != fmid.sign()) {
            max = mid;
        }
        else {
            min = mid;
            fmin = fmid;
        }
    }
    return min.add(max).scale(0.5);
}
function halley(p, d1, d2, x, maxIter = 10, epsilon = 1e-14) {
    for (let i = 0; i < maxIter; i++) {
        const f = p.evaluate(x);
        const f1 = d1.evaluate(x);
        const f2 = d2.evaluate(x);
        const denom = 2 * f1 * f1 - f * f2;
        if (denom === 0)
            break;
        const dx = (2 * f * f1) / denom;
        x -= dx;
        if (Math.abs(dx) < epsilon * Math.max(1, Math.abs(x)))
            break;
    }
    return x;
}
function halleyScalarT(p, d1, d2, x, maxIter = 10, threshold) {
    for (let i = 0; i < maxIter; i++) {
        const f = p.evaluate(x);
        const f1 = d1.evaluate(x);
        const f2 = d2.evaluate(x);
        const denom = f1.mul(f1).scale(2).sub(f.mul(f2));
        if (denom.sign() === 0)
            break;
        const dx = f.mul(f1).scale(2).div(denom);
        x = x.sub(dx);
        if (threshold && dx.abs().lt(threshold))
            break;
    }
    return x;
}
function halleyT(p, d1, d2, x, maxIter = 10, threshold) {
    if (p.degree() < 2)
        return x;
    if ((0, core_1.isScalar)(x))
        return halleyScalarT(p, d1, d2, x, maxIter, threshold);
    for (let i = 0; i < maxIter; i++) {
        const f = p.evaluate(x);
        const f1 = d1.evaluate(x);
        const f2 = d2.evaluate(x);
        const denom = f1.mul(f1).scale(2).sub(f.mul(f2));
        const dx = f.mul(f1).scale(2).div(denom);
        x = x.sub(dx);
    }
    return x;
}
//	adjust intervals to guarantee convergence of halley's method by using roots of further derivatives
function adjust_roots(dpoly, extents) {
    const rootsd = dpoly.realRoots();
    for (const ext of extents) {
        for (const r of rootsd) {
            if (ext.contains(r)) {
                if (dpoly.evaluate(r) > 0)
                    ext.max = r;
                else
                    ext.min = r;
            }
        }
    }
    return rootsd.length;
}
// determine multiplicity of root at numeric x by checking successive derivatives
function multiplicityAt(poly, x, epsilon = Math.max(defaultEpsilon, 1e-12)) {
    if (poly.degree() < 2)
        return 1;
    let multiplicity = 0;
    if (real_1.default.is(x)) {
        const threshold = epsilon * Math.max(1, Math.abs(x));
        while (poly.degree() >= 0 && Math.abs(poly.evaluate(x)) < threshold) {
            poly = poly.deriv();
            multiplicity++;
        }
    }
    else {
        const threshold = x.abs().scale(epsilon);
        while (poly.degree() >= 0 && poly.evaluate(x).abs().lt(threshold)) {
            poly = poly.deriv();
            multiplicity++;
        }
    }
    return multiplicity;
}
//-----------------------------------------------------------------------------
//	real roots
//-----------------------------------------------------------------------------
function normPolyRealRoots(k, epsilon) {
    let zeros = 0;
    for (const i in k) {
        if (k[i] !== 0) {
            zeros = +i;
            break;
        }
    }
    return zeros > 0
        ? insertSorted(checkEven(k.slice(zeros)), 0)
        : checkEven(k);
    function checkEven(k) {
        if (k[1] === 0) {
            const pows = Object.keys(k).map(i => +i).filter(i => k[i] !== 0);
            const gcd = integer_1.default.gcd(k.length, ...pows);
            if (gcd > 1) {
                const r = normal(Array.from({ length: k.length / gcd }, (_, i) => k[i * gcd]));
                return (gcd % 2) === 0
                    ? [...r.map(r => -r).reverse(), ...r]
                    : r;
            }
        }
        return normal(k);
    }
    function normal(k) {
        switch (k.length) {
            case 1:
                return [-k[0]];
            case 2: {
                const e = k[1] * 0.5;
                const d = e * e - k[0];
                if (d > 0) {
                    const r = Math.sqrt(d);
                    return [-r - e, r - e];
                }
                else if (d == 0) {
                    return [-e];
                }
                else {
                    return []; //[-e, Math.sqrt(-d)];
                }
            }
            case 3: {
                const e = k[2] / 3;
                const f = e * e - k[1] / 3;
                const g = (e * k[1] - k[0]) / 2 - e * e * e;
                const h = g * g - f * f * f;
                if (h < 0) {
                    //3 real roots
                    const angle = Math.atan2(real_1.default.copySign(Math.sqrt(-h), g), g) / 3;
                    const c = Math.cos(angle), s = Math.sin(angle);
                    const rf = Math.sqrt(f);
                    return [
                        -2 * rf * c - e,
                        rf * (c + sqrt3 * s) - e,
                        rf * (c - sqrt3 * s) - e,
                    ].sort((a, b) => a - b);
                }
                else if (h > 0) {
                    //1 real root, 2 imaginary (y + iz) & (y - iz)
                    const rh = Math.sqrt(h);
                    const x = Math.pow(g + rh, 1 / 3);
                    const y = Math.pow(g - rh, 1 / 3);
                    return [x + y - e, /*-0.5 * (x + y) - e,	sqrt3 / 2 * (x - y)*/];
                }
                else {
                    //3 real and equal
                    return [Math.pow(-k[0], 1 / 3)];
                }
            }
            case 4: {
                //  substitute x = y - A/4 to eliminate cubic term:
                //	x^4 + px^2 + qx + r = 0
                const a2 = k[3] * k[3];
                const p = k[2] - 3 * a2 / 8;
                const q = a2 * k[3] / 8 - k[3] * k[2] / 2 + k[1];
                const t = -3 * a2 * a2 / 256 + a2 * k[2] / 16 - k[3] * k[1] / 4 + k[0];
                let roots3;
                if (Math.abs(t) < epsilon) {
                    // no absolute term: y(y^3 + py + q) = 0
                    roots3 = [0, ...normPolyRealRoots([q, p, 0], epsilon)];
                }
                else {
                    // solve the resolvent cubic ...
                    const r = normPolyRealRoots([-q * q / 8, p * p / 4 - t, p], epsilon);
                    const m = Math.max(...r);
                    const v = Math.sqrt(m * 2);
                    const u = q / Math.sqrt(m * 8);
                    roots3 = [...normPolyRealRoots([m + p / 2 - u, v], epsilon), ...normPolyRealRoots([m + p / 2 + u, -v], epsilon)];
                }
                return roots3.map(r => r - k[3] / 4);
            }
            case 5: {
                const poly = new polynomialN(k);
                const dpoly = poly.deriv();
                const d2poly = dpoly.deriv();
                const r1 = dpoly.realRoots();
                const bounds = realBound(k);
                let r;
                if (r1.length === 4) {
                    r = [];
                    const r2 = [bounds.min, ...r1, bounds.max];
                    const vals = poly.evaluate(r2);
                    for (let i = 1; i < 6; i++) {
                        if (vals[i] * vals[i - 1] <= 0)
                            r.push((r2[i] + r2[i - 1]) / 2);
                    }
                }
                else {
                    const ranges = [
                        bounds,
                    ];
                    const vals = poly.evaluate(r1);
                    if (r1.length === 2) {
                        //roots1.xy	= sort(get(roots1.xy));
                        if (vals[0] < 0) {
                            ranges[0].min = r1[1];
                        }
                        else if (vals[1] > 0) {
                            ranges[0].max = r1[0];
                        }
                        else {
                            ranges[0].max = r1[0];
                            ranges.push(new real_1.default.extent(r1[1], bounds.max));
                        }
                    }
                    //further subdivide intervals to guarantee convergence of halley's method by using roots of further derivatives
                    const num_roots = adjust_roots(d2poly, ranges);
                    if (num_roots != 3)
                        adjust_roots(d2poly.deriv(), ranges);
                    r = ranges.map(i => i.centre());
                }
                //8 halley iterations
                return r.map(i => halley(poly, dpoly, d2poly, i, 8));
            }
            default: {
                const pN = new polynomialN(k);
                const d1 = pN.deriv();
                const d2 = d1.deriv();
                return sturmIsolateIntervals(pN).map(i => halley(pN, d1, d2, bisectRoot(pN, i.min, i.max, epsilon)));
            }
        }
    }
}
function normPolyRealRootsT(k, epsilon) {
    const zero = k[0].scale(0);
    let zeros = 0;
    for (const i in k) {
        if (k[i].sign() !== 0) {
            zeros = +i;
            break;
        }
    }
    return zeros > 0
        ? insertSorted(checkEven(k.slice(zeros)), zero, (a, b) => a.lt(b))
        : checkEven(k);
    function checkEven(k) {
        if (k[1]?.sign() === 0) {
            const pows = Object.keys(k).map(i => +i).filter(i => k[i].sign() !== 0);
            const gcd = integer_1.default.gcd(...pows);
            if (gcd > 1) {
                const r = normal(Array.from({ length: k.length / gcd }, (_, i) => k[i * gcd]));
                return (gcd % 2) === 0
                    ? [...r.map(r => r.neg()).reverse(), ...r]
                    : r;
            }
        }
        return normal(k);
    }
    function normal(k) {
        switch (k.length) {
            case 1:
                return [k[0].neg()];
            case 2: {
                const e = k[1].scale(0.5);
                const d = e.mul(e).sub(k[0]);
                switch (d.sign()) {
                    default:
                    case 1: {
                        const r = d.sqrt();
                        return [r.neg().sub(e), r.sub(e)];
                    }
                    case 0:
                        return [e.neg()];
                    case -1:
                        return []; //[-e, Math.sqrt(-d)];
                }
            }
            case 3: {
                const e = k[2].scale(1 / 3);
                const f = e.mul(e).sub(k[1].scale(1 / 3));
                const g = e.mul(k[1]).sub(k[0]).scale(0.5).sub(e.mul(e).mul(e));
                const h = g.mul(g).sub(f.mul(f).mul(f));
                switch (h.sign()) {
                    case -1: {
                        //3 real roots - use complex cube roots
                        const rh = h.abs().sqrt();
                        const x = (0, complex_1.complexT)(g, rh).rpow(1, 3);
                        const y = (0, complex_1.complexT)(g, rh.neg()).rpow(1, 3);
                        const half = k[0].from(1).div(k[0].from(2));
                        const sqrt3 = k[0].from(3).rpow(1, 2);
                        return [
                            x.r.add(y.r).sub(e),
                            x.r.add(y.r).neg().sub(x.i.sub(y.i).mul(sqrt3)).mul(half).sub(e),
                            x.r.add(y.r).neg().add(x.i.sub(y.i).mul(sqrt3)).mul(half).sub(e),
                        ];
                    }
                    default:
                    case 1: {
                        //1 real root, 2 imaginary (y + iz) & (y - iz)
                        const rh = h.sqrt();
                        const x = g.add(rh).rpow(1, 3);
                        const y = g.sub(rh).rpow(1, 3);
                        return [x.add(y).sub(e)];
                    }
                    case 0: {
                        //3 real and equal
                        return [k[0].neg().rpow(1, 3)];
                    }
                }
            }
            case 4: {
                //  substitute x = y - A/4 to eliminate cubic term:
                //	x^4 + px^2 + qx + r = 0
                const a2 = k[3].mul(k[3]);
                const p = k[2].sub(a2.scale(3 / 8));
                const q = a2.mul(k[3]).scale(1 / 8).sub(k[3].mul(k[2]).scale(1 / 2)).add(k[1]);
                const t = a2.mul(a2).scale(-3 / 256).add(a2.mul(k[2]).scale(1 / 16)).sub(k[3].mul(k[1]).scale(1 / 4)).add(k[0]);
                //const px8	= k[2].scale(8).sub(a2.scale(3));
                //const qx8	= a2.mul(k[3]).sub(k[3].mul(k[2]).scale(4)).add(k[1].scale(8));
                //const tx256	= a2.mul(a2).scale(-3).add(a2.mul(k[2]).scale(16)).sub(k[3].mul(k[1]).scale(64)).add(k[0].scale(256));
                let roots3;
                if ((0, core_1.has)('lt')(t) && t.abs().lt(epsilon)) {
                    // no absolute term: y(y^3 + py + q) = 0
                    roots3 = [zero, ...normPolyRealRootsT([q, p, zero], epsilon)];
                }
                else {
                    // solve the resolvent cubic ...
                    const r = normPolyRealRootsT([q.mul(q).scale(-1 / 8), p.mul(p).scale(1 / 4).sub(t), p], epsilon);
                    const m = gen_1.default.max(...r);
                    const v = m.scale(2).sqrt();
                    const u = q.div(m.scale(8).sqrt());
                    roots3 = [...normPolyRealRootsT([m.add(p.scale(0.5)).sub(u), v], epsilon), ...normPolyRealRootsT([m.add(p.scale(0.5)).add(u), v.neg()], epsilon)];
                }
                return roots3.map(r => r.sub(k[3].scale(1 / 4)));
            }
            default: {
                const pN = new polynomialNT(k);
                const d1 = pN.deriv();
                const d2 = d1.deriv();
                return sturmIsolateIntervalsT(pN, epsilon).map(i => halleyT(pN, d1, d2, bisectRootT(pN, i.min, i.max, epsilon)));
            }
        }
    }
}
function sturmIsolateIntervals(pN) {
    const n = pN.degree();
    if (n === 0)
        return [];
    const seq = [];
    let p0 = new polynomial([...pN.c, 1]);
    let p1 = pN.deriv();
    seq.push(p0.dup(), p1.dup());
    while (p1.degree() >= 0) {
        p0.divmod(p1);
        if (p0.degree() < 0)
            break;
        // push -r
        for (let i = 0; i < p0.c.length; i++)
            p0.c[i] = -p0.c[i];
        seq.push(p0.dup());
        [p0, p1] = [p1, p0];
    }
    function signChangesAt(x) {
        let prev = 0;
        let changes = 0;
        for (const p of seq) {
            let s = Math.sign(p.evaluate(x));
            if (s === 0) {
                const eps = Math.max(1e-12, Math.abs(x) * 1e-12);
                s = Math.sign(p.evaluate(x + eps));
                if (s === 0)
                    continue;
            }
            if (prev !== 0 && s !== prev)
                changes++;
            prev = s;
        }
        return changes;
    }
    function countRootsInInterval(a, b) {
        return Math.abs(signChangesAt(a) - signChangesAt(b));
    }
    const bounds = realBound(pN.c);
    let lower = bounds.min;
    let upper = bounds.max;
    let count = countRootsInInterval(lower, upper);
    const stack = [];
    const intervals = [];
    for (;;) {
        while (count > 1) {
            const mid = 0.5 * (lower + upper);
            const lowerCount = countRootsInInterval(lower, mid);
            if (lowerCount < count)
                stack.push([mid, upper, count - lowerCount]);
            upper = mid;
            count = lowerCount;
        }
        if (count === 1)
            intervals.push(new real_1.default.extent(lower, upper));
        if (stack.length == 0)
            break;
        [lower, upper, count] = stack.pop();
    }
    return intervals;
}
function sturmIsolateIntervalsT(pN, epsilon) {
    const n = pN.degree();
    if (n === 0)
        return [];
    if (n === 1) {
        const root = pN.c[0].neg();
        return [new gen_1.default.extent(root, root)];
    }
    // Build Sturm sequence
    const seq = [];
    let p0 = new polynomialT([...pN.c, pN.c[0].from(1)]);
    let p1 = pN.deriv();
    seq.push(p0.dup(), p1.dup());
    while (p1.degree() >= 0) {
        p0.divmod(p1);
        if (p0.degree() < 0)
            break;
        // push -r
        for (let i = 0; i < p0.c.length; i++)
            p0.c[i] = p0.c[i].neg();
        seq.push(p0.dup());
        [p0, p1] = [p1, p0];
    }
    function SignChanges(x) {
        let prev = 0;
        let changes = 0;
        for (const p of seq) {
            let s = p.evaluate(x).sign();
            if (s === 0) {
                // if polynomial evaluates to zero, use sign of nearby point
                const eps = gen_1.default.max(x.from(1e-12), x.abs().scale(1e-12)); //Math.max(1e-12, Math.abs(x) * 1e-12);
                s = p.evaluate(x.add(eps)).sign();
                if (s === 0)
                    continue;
            }
            if (prev !== 0 && s !== prev)
                changes++;
            prev = s;
        }
        return changes;
    }
    function countRootsInInterval(a, b) {
        const va = SignChanges(a);
        const vb = SignChanges(b);
        return Math.abs(va - vb);
    }
    const bounds = realBoundT(pN);
    let lower = (0, core_1.asScalarT)(epsilon, bounds.min).sub(epsilon);
    let upper = (0, core_1.asScalarT)(epsilon, bounds.max).add(epsilon);
    let count = countRootsInInterval(lower, upper);
    const stack = [];
    const intervals = [];
    for (;;) {
        while (count > 1) {
            // split interval
            const mid = lower.add(upper).scale(0.5);
            const lowerCount = countRootsInInterval(lower, mid);
            if (lowerCount < count)
                stack.push([mid, upper, count - lowerCount]);
            upper = mid;
            count = lowerCount;
        }
        if (count === 1)
            intervals.push(new gen_1.default.extent(lower, upper));
        if (stack.length == 0)
            break;
        [lower, upper, count] = stack.pop();
    }
    return intervals;
}
//-----------------------------------------------------------------------------
//	complex roots
//-----------------------------------------------------------------------------
function normPolyComplexRoots(k, epsilon) {
    let zeros = 0;
    for (const i in k) {
        if (k[i] !== 0) {
            zeros = +i;
            break;
        }
    }
    return zeros > 0
        ? [...checkEven(k.slice(zeros)), complex_1.default.zero()]
        : checkEven(k);
    function checkEven(k) {
        if (k[1] === 0) {
            const pows = Object.keys(k).map(i => +i).filter(i => k[i] !== 0);
            const gcd = integer_1.default.gcd(...pows);
            if (gcd > 1) {
                const r = normal(Array.from({ length: k.length / gcd }, (_, i) => k[i * gcd]));
                return (gcd % 2) === 0
                    ? [...r.map(r => r.neg()).reverse(), ...r]
                    : r;
            }
        }
        /*
        if (k.length % 2 == 0 && k[1] === 0) {
            let odds = false;
            for (let j = 3; j < k.length; j += 2) {
                odds = k[j] !== 0;
                if (odds)
                    break;
            }
            if (!odds) {
                const r = normal(k.filter((_, i) => i % 2 == 0)).map(r => r.sqrt());
                return [...r.map(r => r.neg()).reverse(), ...r];
            }
        }*/
        return normal(k);
    }
    function normal(k) {
        switch (k.length) {
            case 1:
                return [(0, complex_1.default)(-k[0])];
            case 2: {
                const e = k[1] * 0.5;
                const d = e * e - k[0];
                if (d > 0) {
                    const r = Math.sqrt(d);
                    return [(0, complex_1.default)(-r - e), (0, complex_1.default)(r - e)];
                }
                return d == 0 ? [(0, complex_1.default)(-e)] : complex_1.default.conjugatePair((0, complex_1.default)(-e, Math.sqrt(-d)));
            }
            case 3: {
                const e = k[2] / 3;
                const f = e * e - k[1] / 3;
                const g = (e * k[1] - k[0]) / 2 - e * e * e;
                const h = g * g - f * f * f;
                if (h < 0) {
                    //3 real roots
                    const angle = Math.atan2(real_1.default.copySign(Math.sqrt(-h), g), g) / 3;
                    const c = Math.cos(angle), s = Math.sin(angle);
                    const rf = Math.sqrt(f);
                    return [
                        (0, complex_1.default)(2 * c * rf - e),
                        (0, complex_1.default)((-c - sqrt3 * s) * rf - e),
                        (0, complex_1.default)((-c + sqrt3 * s) * rf - e),
                    ];
                }
                else if (h > 0) {
                    //1 real root, 2 imaginary (y + iz) & (y - iz)
                    const rh = Math.sqrt(h);
                    const x = Math.pow(g + rh, 1 / 3);
                    const y = Math.pow(g - rh, 1 / 3);
                    return [(0, complex_1.default)(x + y - e), ...complex_1.default.conjugatePair((0, complex_1.default)(-0.5 * (x + y) - e, sqrt3 / 2 * (x - y)))];
                }
                else {
                    //3 real and equal
                    return [(0, complex_1.default)(Math.pow(-k[0], 1 / 3))];
                }
            }
            case 4: {
                //  substitute x = y - A/4 to eliminate cubic term:
                //	x^4 + px^2 + qx + r = 0
                const a2 = k[3] * k[3];
                const p = k[2] - 3 * a2 / 8;
                const q = a2 * k[3] / 8 - k[3] * k[2] / 2 + k[1];
                const t = -3 * a2 * a2 / 256 + a2 * k[2] / 16 - k[3] * k[1] / 4 + k[0];
                let roots3;
                if (Math.abs(t) < epsilon) {
                    // no absolute term: y(y^3 + py + q) = 0
                    roots3 = [complex_1.default.zero(), ...normPolyComplexRoots([q, p, 0], epsilon)];
                }
                else {
                    // solve the resolvent cubic ...
                    const r = normPolyRealRoots([-q * q / 8, p * p / 4 - t, p], epsilon);
                    const m = Math.max(...r);
                    const v = Math.sqrt(m * 2);
                    const u = q / Math.sqrt(m * 8);
                    roots3 = [...normPolyComplexRoots([m + p / 2 - u, v], epsilon), ...normPolyComplexRoots([m + p / 2 + u, -v], epsilon)];
                }
                return roots3.map(r => (0, complex_1.default)(r.r - k[3] / 4, r.i));
            }
            default:
                return aberth(new polynomialN(k));
        }
    }
}
function normPolyComplexRootsT(k, epsilon) {
    const zero = k[0].scale(0);
    let zeros = 0;
    for (const i in k) {
        if (k[i].sign() !== 0) {
            zeros = +i;
            break;
        }
    }
    return zeros > 0
        ? [...checkEven(k.slice(zeros)), (0, complex_1.complexT)(zero, zero)]
        : checkEven(k);
    function checkEven(k) {
        if (k[1]?.sign() === 0) {
            const pows = Object.keys(k).map(i => +i).filter(i => k[i].sign() !== 0);
            const gcd = integer_1.default.gcd(...pows);
            if (gcd > 1) {
                const r = normal(Array.from({ length: k.length / gcd }, (_, i) => k[i * gcd]));
                return (gcd % 2) === 0
                    ? [...r.map(r => r.neg()).reverse(), ...r]
                    : r;
            }
        }
        /*if (k.length % 2 == 0 && k[1].sign() === 0) {
            let odds = false;
            for (let j = 3; j < k.length; j += 2) {
                odds = k[j].sign() !== 0;
                if (odds)
                    break;
            }
            if (!odds) {
                const r = normal(k.filter((v, i) => i % 2 == 0)).map(r => r.sqrt());
                return [...r.map(r => r.neg()).reverse(), ...r];
            }
        }*/
        return normal(k);
    }
    function normal(k) {
        switch (k.length) {
            case 1:
                return [(0, complex_1.complexT)(k[0].neg(), zero)];
            case 2: {
                const e = k[1].scale(0.5);
                const d = e.mul(e).sub(k[0]);
                if (!(d.sign() <= 0)) {
                    const r = d.sqrt();
                    return [(0, complex_1.complexT)(r.neg().sub(e), zero), (0, complex_1.complexT)(r.sub(e), zero)];
                }
                else if (d.sign() === 0) {
                    return [(0, complex_1.complexT)(e.neg(), zero)];
                }
                else {
                    return complex_1.complexT.conjugatePair((0, complex_1.complexT)(e.neg(), d.sqrt()));
                }
            }
            case 3: {
                const e = k[2].scale(1 / 3);
                const f = e.mul(e).sub(k[1].scale(1 / 3));
                const g = e.mul(k[1]).sub(k[0]).scale(0.5).sub(e.mul(e).mul(e));
                const h = g.mul(g).sub(f.mul(f).mul(f));
                switch (h.sign()) {
                    case -1: {
                        //3 real roots
                        const s0 = gen_1.default.copySign(h.neg().sqrt(), g);
                        const angle = (0, core_1.hasStatic)(s0, 'atan2')?.(s0, g).div(3) ?? Math.atan2(Number(s0), Number(g)) / 3;
                        const c = (0, core_1.hasStatic)(s0, 'cos')?.(angle) ?? k[0].from(Math.cos(angle));
                        const s = (0, core_1.hasStatic)(s0, 'sin')?.(angle) ?? k[0].from(Math.sin(angle));
                        //const	c		= k[0].from(Math.cos(angle)), s = k[0].from(Math.sin(angle));
                        const rf = f.sqrt();
                        return [
                            (0, complex_1.complexT)(c.scale(2).mul(rf).sub(e), zero),
                            (0, complex_1.complexT)((c.neg().sub(s.scale(sqrt3))).mul(rf).sub(e), zero),
                            (0, complex_1.complexT)((c.neg().add(s.scale(sqrt3))).mul(rf).sub(e), zero),
                        ];
                    }
                    default:
                    case 1: {
                        //1 real root, 2 imaginary (y + iz) & (y - iz)
                        const rh = h.sqrt();
                        const x = g.add(rh).rpow(1, 3);
                        const y = g.sub(rh).rpow(1, 3);
                        return [(0, complex_1.complexT)(x.add(y).sub(e), zero), ...complex_1.complexT.conjugatePair((0, complex_1.complexT)(x.add(y).scale(-0.5).sub(e), y.sub(x).scale(sqrt3 / 2)))];
                    }
                    case 0: {
                        //3 real and equal
                        return [(0, complex_1.complexT)(k[0].neg().rpow(1, 3), zero)];
                    }
                }
            }
            case 4: {
                //  substitute x = y - A/4 to eliminate cubic term:
                //	x^4 + px^2 + qx + r = 0
                const a2 = k[3].mul(k[3]);
                const p = k[2].sub(a2.scale(3 / 8));
                const q = a2.mul(k[3]).scale(1 / 8).sub(k[3].mul(k[2]).scale(1 / 2)).add(k[1]);
                const t = a2.mul(a2).scale(-3 / 256).add(a2.mul(k[2]).scale(1 / 16)).sub(k[3].mul(k[1]).scale(1 / 4)).add(k[0]);
                let roots3;
                if ((0, core_1.has)('lt')(t) && t.abs().lt(epsilon)) {
                    // no absolute term: y(y^3 + py + q) = 0
                    roots3 = [(0, complex_1.complexT)(zero, zero), ...normPolyComplexRootsT([q, p, zero], epsilon)];
                }
                else {
                    // solve the resolvent cubic ...
                    const r = normPolyRealRootsT([q.neg().mul(q).scale(1 / 8), p.mul(p).scale(1 / 4).sub(t), p], epsilon);
                    const m = gen_1.default.max(...r);
                    const v = m.scale(2).sqrt();
                    const u = q.div(m.scale(8).sqrt());
                    roots3 = [...normPolyComplexRootsT([m.add(p.scale(0.5)).sub(u), v], epsilon), ...normPolyComplexRootsT([m.add(p.scale(0.5)).add(u), v.neg()], epsilon)];
                }
                return roots3.map(r => (0, complex_1.complexT)(r.r.sub(k[3].scale(1 / 4)), r.i));
            }
            default:
                return aberthT(new polynomialNT(k));
        }
    }
}
function aberth(poly, tolerance = 1e-6, maxIterations = 100) {
    const radius = lagrangeImproved(poly.c);
    const n = poly.degree();
    const roots = Array.from({ length: n }, (_, i) => complex_1.default.fromPolar(radius, 2 * Math.PI * i / n));
    const dpoly = poly.deriv();
    for (let iter = 0; iter < maxIterations; iter++) {
        let maxCorrection = 0;
        for (let i = 0; i < roots.length; i++) {
            const zi = roots[i];
            const p_zi = poly.evaluate(zi);
            const dp_zi = dpoly.evaluate(zi);
            let sum = complex_1.default.zero();
            for (let j = 0; j < roots.length; j++) {
                if (i !== j)
                    sum = sum.add((0, complex_1.default)(1).div(zi.sub(roots[j])));
            }
            const correction = p_zi.div((dp_zi.sub(p_zi.mul(sum))));
            roots[i] = roots[i].sub(correction);
            maxCorrection = Math.max(maxCorrection, Math.abs(correction.r) + Math.abs(correction.i));
        }
        if (maxCorrection < tolerance)
            break;
    }
    return roots;
}
function isComplexPolyN(poly) {
    return poly.c[0] instanceof complex_1.complexT;
}
function aberthT(poly, tolerance, maxIterations = 100) {
    let from;
    let evaluate;
    if (isComplexPolyN(poly)) {
        from = poly.c[0].r.from;
        evaluate = (p, t) => p.evaluate(t);
    }
    else {
        from = poly.c[0].from;
        evaluate = (p, t) => {
            let i = p.c.length - 1;
            let r = t.add((0, complex_1.complexT)(p.c[i], p.c[i].scale(0)));
            while (i--)
                r = r.mul(t).add((0, complex_1.complexT)(p.c[i], p.c[i].scale(0)));
            return r;
        };
    }
    const n = poly.degree();
    const zero = from(0), one = from(1);
    const czero = (0, complex_1.complexT)(zero, zero), cone = (0, complex_1.complexT)(one, zero);
    const dpoly = poly.deriv();
    const radius = lagrangeImprovedT(poly.c);
    const roots = Array.from({ length: n }, (_, i) => complex_1.complexT.fromPolar(radius, 2 * Math.PI * i / n));
    tolerance ??= from(1e-6);
    for (let iter = 0; iter < maxIterations; iter++) {
        let maxCorrection = zero;
        for (let i = 0; i < n; i++) {
            const zi = roots[i];
            const p_zi = evaluate(poly, zi);
            const dp_zi = evaluate(dpoly, zi);
            let sum = czero;
            for (let j = 0; j < roots.length; j++) {
                if (i !== j)
                    sum = sum.add(cone.div(zi.sub(roots[j])));
            }
            const correction = p_zi.div((dp_zi.sub(p_zi.mul(sum))));
            roots[i] = roots[i].sub(correction);
            maxCorrection = gen_1.default.max(maxCorrection, correction.abs());
        }
        if (maxCorrection.lt(tolerance))
            break;
    }
    return roots;
}
//-----------------------------------------------------------------------------
// Integer/Rational Root Theorem extractor
//-----------------------------------------------------------------------------
function divRoot(c, root) {
    let i = c.length - 1;
    if (i < 1)
        return [[], i ? c[0] : 0];
    const b = new Array(i);
    if (typeof root === 'number') {
        b[i - 1] = c[i];
        while (--i)
            b[i - 1] = c[i] + root * b[i];
        return [b, c[0] + root * b[0]];
    }
    else {
        b[i - 1] = c[i];
        while (--i)
            b[i - 1] = c[i] * root.den + root.num * b[i];
        return [b, c[0] * root.den + root.num * b[0]];
    }
}
function divRootB(c, root) {
    let i = c.length - 1;
    if (i < 1)
        return [[], i ? c[0] : 0n];
    const b = new Array(i);
    if (typeof root === 'bigint') {
        b[i - 1] = c[i];
        while (--i)
            b[i - 1] = c[i] + root * b[i];
        return [b, c[0] + root * b[0]];
    }
    else {
        b[i - 1] = c[i];
        while (--i)
            b[i - 1] = c[i] * root.den + root.num * b[i];
        return [b, c[0] * root.den + root.num * b[0]];
    }
}
function removeMultiplicity(p, root) {
    while (p.degree() > 0) {
        const [quotient, remainder] = divRoot(p.c, root);
        if (remainder !== 0)
            return false;
        p.c = quotient;
    }
    return true;
}
function removeMultiplicityB(p, root) {
    while (p.degree() > 0) {
        const [quotient, remainder] = divRootB(p.c, root);
        if (remainder !== 0n)
            return false;
        p.c = quotient;
    }
    return true;
}
function* rationalDivisors(numFactors, denFactors, limit) {
    const numEntries = Array.from(numFactors.entries());
    const denEntries = Array.from(denFactors.entries());
    const primes = new Set();
    function* backtrackNum(i, num, den) {
        if (limit && num * limit.den > den * limit.num)
            return;
        if (i === numEntries.length) {
            yield (0, rational_1.default)(num, den);
            return;
        }
        const [p, exp] = numEntries[i++];
        if (primes.has(p)) {
            yield* backtrackNum(i, num, den);
        }
        else {
            for (let e = 0; e <= exp; e++, num *= p)
                yield* backtrackNum(i, num, den);
        }
    }
    function* backtrackDen(i, den) {
        if (i === denEntries.length) {
            if (numEntries.length === 0)
                yield (0, rational_1.default)(1, den);
            else
                yield* backtrackNum(0, 1, den);
            return;
        }
        const [p, exp] = denEntries[i++];
        yield* backtrackDen(i, den);
        primes.add(p);
        for (let e = 1; e <= exp; e++)
            yield* backtrackDen(i, den *= p);
    }
    yield* backtrackDen(0, 1);
}
function* rationalDivisorsB(numFactors, denFactors, limit) {
    const numEntries = Array.from(numFactors.entries());
    const denEntries = Array.from(denFactors.entries());
    const primes = new Set();
    function* backtrackNum(i, num, den) {
        if (limit && num * limit.den > den * limit.num)
            return;
        if (i === numEntries.length) {
            yield (0, rational_1.rationalB)(num, den);
            return;
        }
        const [p, exp] = numEntries[i++];
        if (primes.has(p)) {
            yield* backtrackNum(i, num, den);
        }
        else {
            for (let e = 0; e <= exp; e++, num *= p)
                yield* backtrackNum(i, num, den);
        }
    }
    function* backtrackDen(i, den) {
        if (i === denEntries.length) {
            if (numEntries.length === 0)
                yield (0, rational_1.rationalB)(1n, den);
            else
                yield* backtrackNum(0, 1n, den);
            return;
        }
        const [p, exp] = denEntries[i++];
        yield* backtrackDen(i, den);
        primes.add(p);
        for (let e = 1; e <= exp; e++)
            yield* backtrackDen(i, den *= p);
    }
    yield* backtrackDen(0, 1n);
}
// modifies p to remove found roots
function rationalRootsN(p) {
    const roots0 = [];
    if (p.c[0] === 0)
        roots0.push(0);
    while (p.c[0] === 0)
        p.c = divRoot(p.c, 0)[0];
    const r = p.c.map(c => rational_1.default.from(c));
    const m = integer_1.default.lcm(...r.map(v => v.den));
    const p2 = new polynomial(r.map(v => v.num * (m / v.den)));
    p = p2;
    if (p.leadCoeff() === 1) {
        const a0 = p.c[0];
        const factors = new prime_1.factorisation(a0);
        const bound = lagrangeImproved(p.c.slice(0, -1));
        for (const x of factors.divisors(bound)) {
            if (p.evaluate(x) === 0) {
                roots0.push(x);
                if (removeMultiplicity(p, x))
                    break;
            }
            if (p.evaluate(-x) === 0) {
                roots0.push(-x);
                if (removeMultiplicity(p, -x))
                    break;
            }
        }
        return roots0.map(r => rational_1.default.from(r));
    }
    const roots = roots0.map(r => (0, rational_1.default)(r));
    const a0 = Math.abs(p.c[0]);
    const an = Math.abs(p.leadCoeff());
    const pFactors = new prime_1.factorisation(a0);
    const qFactors = new prime_1.factorisation(an);
    const bound = rational_1.default.from(lagrangeImproved(p.normalise().c));
    for (const r of rationalDivisors(pFactors, qFactors, bound)) {
        let i = p.degree();
        let acc1 = p.c[i];
        let acc2 = p.c[i];
        for (let denPow = r.den; i--; denPow *= r.den) {
            const c = p.c[i] * denPow;
            acc1 = acc1 * r.num + c;
            acc2 = acc2 * -r.num + c;
        }
        if (acc1 === 0) {
            roots.push(r);
            if (removeMultiplicity(p, r))
                break;
        }
        if (acc2 === 0) {
            const r2 = r.neg();
            roots.push(r2);
            if (removeMultiplicity(p, r2))
                break;
        }
    }
    return roots.sort((a, b) => a.compare(b));
}
// modifies p to remove found roots
/*
function rationalRootsNB(p: polynomialB): bigint[] {
    const roots: bigint[] = [];
    if (p.c[0] === 0n)
        roots.push(0n);

    while (p.c[0] === 0n)
        p.c = divRootB(p.c, 0n)[0];

    const a0		= p.c[0];
    const factors	= new factorisationB(a0);
    const bound		= lagrangeImprovedB(p.c.slice(0, -1));

    for (const x of factors.divisors(bound)) {
        if (p.evaluate(x) === 0n) {
            roots.push(x);
            if (removeMultiplicityB(p, x))
                break;
        }
        if (p.evaluate(-x) === 0n) {
            roots.push(-x);
            if (removeMultiplicityB(p, -x))
                break;
        }
    }
    return roots.sort(compare);
}
*/
// modifies p to remove found roots
function rationalRootsB(p) {
    const roots = [];
    if (p.c[0] === 0n)
        roots.push(rational_1.rationalB.from(0n));
    while (p.c[0] === 0n)
        p.c = divRootB(p.c, 0n)[0];
    if (p.degree() === 0)
        return roots;
    const a0 = big_1.default.abs(p.c[0]);
    const an = big_1.default.abs(p.leadCoeff());
    const pFactors = new prime_1.factorisationB(a0);
    const qFactors = new prime_1.factorisationB(an);
    const bound = realBoundT(p.normalise());
    const bound2 = rational_1.rationalB.from(+gen_1.default.max(bound.min.neg(), bound.max));
    for (const r of rationalDivisorsB(pFactors, qFactors, bound2)) {
        let i = p.degree();
        let acc1 = p.c[i];
        let acc2 = p.c[i];
        for (let denPow = r.den; i--; denPow *= r.den) {
            const c = p.c[i] * denPow;
            acc1 = acc1 * r.num + c;
            acc2 = acc2 * -r.num + c;
        }
        if (acc1 === 0n) {
            roots.push(r);
            if (removeMultiplicityB(p, r))
                break;
        }
        if (acc2 === 0n) {
            const r2 = r.neg();
            roots.push(r2);
            if (removeMultiplicityB(p, r2))
                break;
        }
    }
    return roots.sort((a, b) => a.compare(b));
}
//-----------------------------------------------------------------------------
//	Legendre polynomial and roots
//-----------------------------------------------------------------------------
function legendrePolynomial(n) {
    let P0 = Polynomial([1]);
    if (n === 0)
        return P0;
    let P1 = Polynomial([0, 1]);
    for (let k = 2; k <= n; ++k) {
        P0 = P1;
        P1 = Polynomial([0, 1]).mul(P1).scale(2 * k - 1).sub(P0.scale(k - 1)).scale(1 / k);
    }
    return P1;
}
function legendreTable(n) {
    const P = legendrePolynomial(n);
    const dP = P.deriv();
    // roots returns x_i in [-1, 1]
    return P.realRoots().map(x => [x, 2 / ((1 - x * x) * (dP.evaluate(x) ** 2))]);
}
//# sourceMappingURL=polynomial.js.map