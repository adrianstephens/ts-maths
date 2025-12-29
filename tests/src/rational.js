"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.rationalT = exports.rationalB = exports.rational = void 0;
exports.canMakeRational = canMakeRational;
const core_1 = require("./core");
const real_1 = __importDefault(require("./real"));
const big_1 = __importDefault(require("./big"));
const gen_1 = __importDefault(require("./gen"));
const string_1 = require("./string");
function canMakeRational(x) {
    return 'divmod' in x;
}
//-----------------------------------------------------------------------------
// number rationals
//-----------------------------------------------------------------------------
class _rational {
    num;
    den;
    constructor(num, den = 1) {
        this.num = num;
        this.den = den;
        if (this.den < 0) {
            this.den = -this.den;
            this.num = -this.num;
        }
    }
    from(n) { return exports.rational.from(n); }
    dup() { return new _rational(this.num, this.den); }
    neg() { return new _rational(-this.num, this.den); }
    recip() { return new _rational(this.den, this.num); }
    abs() { return new _rational(Math.abs(this.num), this.den); }
    frac() { return new _rational(this.num % this.den, this.den); }
    floor() { return Math.floor(this.num / this.den); }
    sign() { return Math.sign(this.num); }
    set(b) { this.num = b.num; this.den = b.den; return this; }
    scale(b) { return (0, exports.rational)(this.num * b, this.den); }
    mul(b) { return (0, exports.rational)(this.num * b.num, this.den * b.den); }
    add(b) { return (0, exports.rational)(this.num * b.den + b.num * this.den, this.den * b.den); }
    sub(b) { return (0, exports.rational)(this.num * b.den - b.num * this.den, this.den * b.den); }
    div(b) { return this.mul(b.recip()); }
    mod(b) { return this.div(b).frac().mul(b); }
    ipow(b) {
        return b < 0
            ? (0, exports.rational)(this.den ** -b, this.num ** -b)
            : (0, exports.rational)(this.num ** b, this.den ** b);
    }
    divmod(b) { const q = this.div(b); this.set(q.frac().mul(b)); return q.floor(); }
    compare(b) { return this.num * b.den - b.num * this.den; }
    lt(b) { return this.compare(b) < 0; }
    eq(b) { return this.compare(b) === 0; }
    isInteger() { return this.den === 1; }
    is0() { return this.num === 0; }
    is1() { return this.num === this.den; }
    toString(opts) { return (0, string_1.fractionString)(this.num, this.den, opts); }
    valueOf() { return this.num / this.den; }
}
exports.rational = Object.assign(function (num, den = 1) {
    const g = real_1.default.gcd(num, den);
    return new _rational(num / g, den / g);
}, gen_1.default.OperatorsBase(_rational), {
    variable(_name) { },
    rpow(a, n, d) { if (d === 1)
        return a.ipow(n); throw new Error("invalid"); },
    pow(a, b) { return this.rpow(a, b.num, b.den); },
    zero() { return new _rational(0, 1); },
    from(n, maxDen) {
        if (typeof n === 'number') {
            if (Number.isInteger(n))
                return new _rational(n, 1);
            const [h, k] = real_1.default.rationalApprox(n, maxDen ?? 1e6, 1e-20);
            return new _rational(h, k);
        }
        const b = exports.rationalB.from(n);
        return new _rational(Number(b.num), Number(b.den));
    },
    fromContinuedFraction(terms, maxDen) {
        let p2 = 1, q2 = 0;
        let p1 = terms[0], q1 = 1;
        for (const a of (0, core_1.lazySlice)(terms, 1)) {
            [p2, q2, p1, q1] = [p1, q1, a * p1 + p2, a * q1 + q2];
            if (maxDen && q1 > maxDen) {
                // compute maximal t that keeps denom <= maxDen:
                const t = Math.min((maxDen - q2) / q1, a - 1);
                p1 = p2 + t * p1;
                q1 = q2 + t * q1;
                break;
            }
        }
        return new _rational(p1, q1);
    }
});
exports.rational.prototype = _rational.prototype;
exports.default = exports.rational;
//-----------------------------------------------------------------------------
// bigint rationals
//-----------------------------------------------------------------------------
class _rationalB {
    num;
    den;
    constructor(num, den = 1n) {
        this.num = num;
        this.den = den;
        if (this.den < 0n) {
            this.den = -this.den;
            this.num = -this.num;
        }
    }
    from(n) { return exports.rationalB.from(n); }
    dup() { return (0, exports.rationalB)(this.num, this.den); }
    neg() { return (0, exports.rationalB)(-this.num, this.den); }
    recip() { return (0, exports.rationalB)(this.den, this.num); }
    abs() { return (0, exports.rationalB)(big_1.default.abs(this.num), this.den); }
    frac() { return (0, exports.rationalB)(this.num % this.den, this.den); }
    floor() { return this.num / this.den; }
    sign() { return this.num === 0n ? 0 : this.num > 0n ? 1 : -1; }
    set(b) { this.num = b.num; this.den = b.den; return this; }
    scale(b) { return typeof b === 'bigint' ? (0, exports.rationalB)(this.num * b, this.den) : this.mul(exports.rationalB.from(b)); }
    mul(b) { return (0, exports.rationalB)(this.num * b.num, this.den * b.den); }
    add(b) { return (0, exports.rationalB)(this.num * b.den + b.num * this.den, this.den * b.den); }
    sub(b) { return (0, exports.rationalB)(this.num * b.den - b.num * this.den, this.den * b.den); }
    div(b) { return this.mul(b.recip()); }
    mod(b) { return this.div(b).frac().mul(b); }
    ipow(b) {
        return b < 0
            ? (0, exports.rationalB)(this.den ** BigInt(-b), this.num ** BigInt(-b))
            : (0, exports.rationalB)(this.num ** BigInt(b), this.den ** BigInt(b));
    }
    divmod(b) { const q = this.div(b); this.set(q.frac().mul(b)); return q.floor(); }
    compare(b) { return big_1.default.sign(this.num * b.den - b.num * this.den); }
    lt(b) { return this.compare(b) < 0; }
    eq(b) { return this.compare(b) === 0; }
    toString() { return this.den === 1n ? `${this.num}` : `${this.num}/${this.den}`; }
    valueOf() { return big_1.default.divToReal(this.num, this.den); }
}
exports.rationalB = Object.assign(function (num, den = 1n) {
    const g = big_1.default.gcd(num, den);
    return new _rationalB(num / g, den / g);
}, gen_1.default.OperatorsBase(_rationalB), {
    variable(_name) { },
    rpow(a, n, d) { if (d === 1)
        return a.ipow(n); throw new Error("invalid"); },
    pow(a, b) { return this.rpow(a, Number(b.num), Number(b.den)); },
    from(n, maxDen) {
        if (typeof n === 'bigint' || (typeof n === 'number' && Number.isInteger(n)))
            return (0, exports.rationalB)(BigInt(n), 1n);
        if (typeof n === 'number') {
            const [h, k] = real_1.default.rationalApprox(n, 1e20);
            return (0, exports.rationalB)(BigInt(h), BigInt(k));
        }
        const [h, k] = gen_1.default.rationalApprox(n, maxDen ?? 1n << 64n, n.from(1e-8));
        return (0, exports.rationalB)(BigInt(h), BigInt(k));
    },
    fromContinuedFraction(terms, maxDen) {
        let p2 = 1n, q2 = 0n;
        let p1 = BigInt(terms[0]), q1 = 1n;
        for (const i of (0, core_1.lazySlice)(terms, 1)) {
            const a = BigInt(i);
            [p2, q2, p1, q1] = [p1, q1, a * p1 + p2, a * q1 + q2];
            if (maxDen && q1 > maxDen) {
                // compute maximal t that keeps denom <= maxDen:
                const t = big_1.default.min((maxDen - q2) / q1, a - 1n);
                p1 = p2 + t * p1;
                q1 = q2 + t * q1;
                break;
            }
        }
        return (0, exports.rationalB)(p1, q1);
    }
});
exports.rationalB.prototype = _rationalB.prototype;
//-----------------------------------------------------------------------------
// generic rationals - e.g. polynomials, symbolic.
// Use rational or rationalB for 'scalar' types
//-----------------------------------------------------------------------------
class _rationalT {
    num;
    den;
    constructor(num, den) {
        this.num = num;
        this.den = den;
        if (this.den.sign() < 0) {
            this.den = this.den.neg();
            this.num = this.num.neg();
        }
    }
    from(n, maxDen) {
        if (typeof n === 'bigint' || (typeof n === 'number' && Number.isInteger(n)))
            return new _rationalT(this.num.from(n), this.num.from(1));
        if (typeof n === 'number') {
            const [h, k] = real_1.default.rationalApprox(n, 1e20);
            return new _rationalT(this.num.from(h), this.num.from(k));
        }
        if ((0, core_1.isInstance)(n, exports.rational) || (0, core_1.isInstance)(n, exports.rationalB))
            return new _rationalT(this.num.from(n.num), this.num.from(n.den));
        const [h, k] = gen_1.default.rationalApprox(n, maxDen ?? 1n << 64n, n.from(1e-8));
        return new _rationalT(n.from(Number(h)), n.from(Number(k)));
    }
    dup() { return new _rationalT(this.num, this.den); }
    neg() { return new _rationalT(this.num.neg(), this.den); }
    recip() { return new _rationalT(this.den, this.num); }
    abs() { return new _rationalT(this.num.abs(), this.den); }
    frac() { const q = this.dup(); q.num.divmod(this.den); return q; }
    floor() { return this.num.dup().divmod(this.den); }
    sign() { return this.num.sign(); }
    set(b) { this.num = b.num; this.den = b.den; return this; }
    scale(b) { return this.mul(this.from(b)); }
    mul(b) { return (0, exports.rationalT)(this.num.mul(b.num), this.den.mul(b.den)); }
    add(b) { return (0, exports.rationalT)(this.num.mul(b.den).add(b.num.mul(this.den)), this.den.mul(b.den)); }
    sub(b) { return (0, exports.rationalT)(this.num.mul(b.den).sub(b.num.mul(this.den)), this.den.mul(b.den)); }
    div(b) { return this.mul(b.recip()); }
    mod(b) { return this.div(b).frac().mul(b); }
    ipow(b) {
        return b < 0
            ? new _rationalT(this.den.ipow(-b), this.num.ipow(-b))
            : new _rationalT(this.num.ipow(b), this.den.ipow(b));
    }
    divmod(b) {
        const q = this.div(b);
        const _r = q.num.divmod(q.den);
        this.set(q.mul(b));
        return _r;
    }
    compare(b) { return gen_1.default.compare(this.num.mul(b.den), b.num.mul(this.den)); }
    lt(b) { return this.compare(b) < 0; }
    eq(b) { return this.compare(b) === 0; }
    toString() { return `${this.num} / ${this.den}`; }
    valueOf() { return (this.num.div(this.den)).valueOf(); }
}
exports.rationalT = Object.assign(function (num, den) {
    return new _rationalT(num, den);
}, { // statics
//	from<T extends scalarRational<T>>(n: T, maxDen?: bigint): rationalT<T> {
//		const [h, k] = gen.rationalApprox(n, maxDen ?? 1n << 64n, n.from(1e-8));
//		return rationalT(n.from(Number(h)), n.from(Number(k)));
//	},
});
exports.rationalT.prototype = _rationalT.prototype;
//# sourceMappingURL=rational.js.map