"use strict";
/* eslint-disable no-restricted-syntax */
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.complexT = exports.complex = void 0;
const real_1 = __importDefault(require("./real"));
const gen_1 = __importDefault(require("./gen"));
class _complex {
    r;
    i;
    constructor(r, i) {
        this.r = r;
        this.i = i;
    }
    from(b) { return (0, exports.complex)(b, 0); }
    dup() { return (0, exports.complex)(this.r, this.i); }
    neg() { return (0, exports.complex)(-this.r, -this.i); }
    conj() { return (0, exports.complex)(this.r, -this.i); }
    recip() { return this.conj().scale(1 / this.magSq()); }
    magSq() { return this.r * this.r + this.i * this.i; }
    mag() { return Math.sqrt(this.magSq()); }
    abs() { return this.mag(); }
    arg() { return Math.atan2(this.i, this.r); }
    scale(b) { return (0, exports.complex)(this.r * b, this.i * b); }
    mul(b) { return (0, exports.complex)(this.r * b.r - this.i * b.i, this.r * b.i + this.i * b.r); }
    add(b) { return (0, exports.complex)(this.r + b.r, this.i + b.i); }
    sub(b) { return (0, exports.complex)(this.r - b.r, this.i - b.i); }
    div(b) { return this.mul(b.conj()).scale(1 / b.magSq()); }
    npow(b) { return exports.complex.fromPolar(Math.pow(this.mag(), b), this.arg() * b); }
    rpow(n, d) { return this.npow(n / d); }
    pow(b) {
        if (b.i === 0)
            return this.npow(b.r);
        const r = this.mag();
        if (r === 0)
            return (0, exports.complex)(0, 0);
        const ln_r = Math.log(r);
        const theta = this.arg();
        return exports.complex.fromPolar(Math.exp(ln_r * b.r - theta * b.i), ln_r * b.i + theta * b.r);
    }
    eq(b) { return this.r === b.r && this.i === b.i; }
    approx(b, eps = 1e-8) { return real_1.default.approx(this.r, b.r, eps) && real_1.default.approx(this.i, b.i, eps); }
    sqrt() {
        const m = this.abs();
        return (0, exports.complex)(Math.sqrt(0.5 * (m + this.r)), Math.sqrt(0.5 * (m - this.r)));
    }
    toString() { return `${this.r} ${this.i >= 0 ? '+' : '-'} ${Math.abs(this.i)}i`; }
}
function fromPolar2(theta, r1, r2) {
    return (0, exports.complex)(Math.cos(theta) * r1, Math.sin(theta) * r2);
}
exports.complex = Object.assign(function (r, i = 0) {
    return new _complex(r, i);
}, gen_1.default.OperatorsBase(_complex), {
    variable(name) {
        switch (name) {
            case 'i': return (0, exports.complex)(0, 1);
            case 'pi': return (0, exports.complex)(Math.PI);
            case 'e': return (0, exports.complex)(Math.E);
            case 'infinity': return (0, exports.complex)(Infinity);
            default: return undefined;
        }
    },
    lt: (a, b) => a.r < b.r || (a.r === b.r && a.i < b.i),
    fromPolar(r, t) { return (0, exports.complex)(Math.cos(t) * r, Math.sin(t) * r); },
    zero() { return (0, exports.complex)(0); },
    re(a) { return (0, exports.complex)(a.r); },
    im(a) { return (0, exports.complex)(a.i); },
    sqrt(a) {
        return typeof a === 'number' ? (a < 0 ? (0, exports.complex)(0, Math.sqrt(-a)) : (0, exports.complex)(Math.sqrt(a))) : a.sqrt();
    },
    ln(a) { return (0, exports.complex)(Math.log(a.mag()), a.arg()); },
    exp(a) { return this.fromPolar(Math.exp(a.r), a.i); },
    sin(a) {
        const e = Math.exp(a.i), re = 1 / e;
        return fromPolar2(a.r + Math.PI / 2, -(e + re) * 0.5, (e - re) * 0.5);
    },
    cos(a) {
        const e = Math.exp(a.i), re = 1 / e;
        return fromPolar2(a.r, (e + re) * 0.5, (re - e) * 0.5);
    },
    tan(a) {
        return exports.complex.sin(a).div(exports.complex.cos(a));
    },
    sinh(a) {
        const e = Math.exp(a.r), re = 1 / e;
        return fromPolar2(a.i, (e - re) * 0.5, (e + re) * 0.5);
    },
    cosh(a) {
        const e = Math.exp(a.r), re = 1 / e;
        return fromPolar2(a.i, (e + re) * 0.5, (e - re) * 0.5);
    },
    tanh(a) {
        return exports.complex.sinh(a).div(exports.complex.cosh(a));
    },
    conjugatePair(c) { return [c, c.conj()]; },
});
exports.complex.prototype = _complex.prototype;
exports.default = exports.complex;
class _complexT {
    r;
    i;
    constructor(r, i) {
        this.r = r;
        this.i = i;
    }
    dup() { return (0, exports.complexT)(this.r.dup(), this.i.dup()); }
    neg() { return (0, exports.complexT)(this.r.neg(), this.i.neg()); }
    conj() { return (0, exports.complexT)(this.r, this.i.neg()); }
    recip() { return this.conj().rscale(this.magSq()); }
    magSq() { return this.r.mul(this.r).add(this.i.mul(this.i)); }
    mag() { return this.magSq().sqrt(); }
    abs() { return this.mag(); }
    scale(b) { return (0, exports.complexT)(this.r.scale(b), this.i.scale(b)); }
    rscale(b) { return (0, exports.complexT)(this.r.div(b), this.i.div(b)); }
    mul(b) { return (0, exports.complexT)(this.r.mul(b.r).sub(this.i.mul(b.i)), this.r.mul(b.i).add(this.i.mul(b.r))); }
    add(b) { return (0, exports.complexT)(this.r.add(b.r), this.i.add(b.i)); }
    sub(b) { return (0, exports.complexT)(this.r.sub(b.r), this.i.sub(b.i)); }
    div(b) { return this.mul(b.conj()).rscale(b.magSq()); }
    arg() { return Math.atan2(Number(this.i), Number(this.r)); }
    sqrt() {
        const m = this.mag();
        return (0, exports.complexT)(m.add(this.r).scale(0.5).sqrt(), m.sub(this.r).scale(0.5).sqrt());
    }
    npow(n) {
        return exports.complexT.fromPolar(this.mag().npow(n), this.arg() * n);
    }
    rpow(n, d) {
        if (n === 1 && d === 2)
            return this.sqrt();
        return exports.complexT.fromPolar(this.mag().rpow(n, d), this.arg() * n / d);
    }
    toString() { return `${this.r} ${this.i.sign() >= 0 ? '+' : '-'} ${this.i.abs()}i`; }
    [Symbol.for("debug.description")]() { return `${this.r} + ${this.i} i`; }
}
exports.complexT = Object.assign(function (r, i) {
    return new _complexT(r, i);
}, {
    zero() { return (0, exports.complex)(0, 0); },
    fromPolar(r, t) { return (0, exports.complexT)(r.scale(Math.cos(t)), r.scale(Math.sin(t))); },
    conjugatePair(c) { return [c, c.conj()]; },
});
exports.complexT.prototype = _complexT.prototype;
//# sourceMappingURL=complex.js.map