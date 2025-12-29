"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.real = void 0;
exports.asScalar = asScalar;
exports.asScalarExt = asScalarExt;
const core_1 = require("./core");
//-----------------------------------------------------------------------------
// number
//-----------------------------------------------------------------------------
class _real {
    value;
    constructor(value) {
        this.value = value;
    }
    dup() { return (0, exports.real)(this.value); }
    neg() { return (0, exports.real)(-this.value); }
    scale(b) { return (0, exports.real)(this.value * b); }
    mul(b) { return (0, exports.real)(this.value * b.value); }
    div(b) { return (0, exports.real)(this.value / b.value); }
    add(b) { return (0, exports.real)(this.value + b.value); }
    sub(b) { return (0, exports.real)(this.value - b.value); }
    mag() { return Math.abs(this.value); }
    from(n) { return (0, exports.real)(Number(n)); }
    sign() { return Math.sign(this.value); }
    abs() { return (0, exports.real)(Math.abs(this.value)); }
    recip() { return (0, exports.real)(1 / this.value); }
    divmod(b) { const q = Math.floor(this.value / b.value); this.value -= q * b.value; return q; }
    lt(b) { return this.value < b.value; }
    eq(b) { return this.value === b.value; }
    sqrt() { return (0, exports.real)(Math.sqrt(this.value)); }
    ipow(n) { return (0, exports.real)(this.value ** n); }
    npow(n) { return (0, exports.real)(this.value ** n); }
    rpow(n, d) { return (0, exports.real)(this.value ** (n / d)); }
    valueOf() { return this.value; }
    toString() { return this.value.toString(); }
}
class _extent {
    min;
    max;
    static fromCentreExtent(centre, size) {
        const half = size * 0.5;
        return new _extent(centre - half, centre + half);
    }
    static from(items) {
        const ext = new _extent;
        for (const i of items)
            ext.add(i);
        return ext;
    }
    constructor(min = Infinity, max = -Infinity) {
        this.min = min;
        this.max = max;
    }
    extent() {
        return this.max - this.min;
    }
    centre() {
        return (this.min + this.max) * 0.5;
    }
    add(p) {
        this.min = Math.min(this.min, p);
        this.max = Math.max(this.max, p);
    }
    combine(b) {
        this.min = Math.min(this.min, b.min);
        this.max = Math.max(this.max, b.max);
    }
    encompasses(b) {
        return this.min <= b.min && this.max >= b.max;
    }
    overlaps(b) {
        return this.min <= b.max && this.max >= b.min;
    }
    contains(p) {
        return this.min <= p && this.max >= p;
    }
    clamp(p) {
        return Math.min(Math.max(p, this.min), this.max);
    }
}
exports.real = Object.assign(function (value) {
    return new _real(value);
}, {
    func(name, args) {
        const fn = Math[name];
        return fn ? fn(...args) : undefined;
    },
    variable(name) {
        switch (name) {
            case 'pi': return Math.PI;
            case 'e': return Math.E;
            case 'i': return NaN;
            case 'infinity': return Infinity;
            default: return undefined;
        }
    },
    from(n) { return n; },
    dup(n) { return n; },
    neg(a) { return -a; },
    scale(a, b) { return a * b; },
    add(a, b) { return a + b; },
    sub(a, b) { return a - b; },
    mul(a, b) { return a * b; },
    div(a, b) { return a / b; },
    ipow(a, b) { return a ** b; },
    rpow(a, n, d) { return a ** (n / d); },
    pow(a, b) { return Math.pow(a, b); },
    eq(a, b) { return a === b; },
    lt(a, b) { return a < b; },
}, {
    is(x) {
        return typeof x === 'number';
    },
    approx(x, y, epsilon = Number.EPSILON) {
        return Math.abs(x - y) < epsilon;
    },
    copySign(a, b) {
        return b < 0 ? -Math.abs(a) : Math.abs(a);
    },
    gcd(...values) {
        let a = 0;
        for (let b of values) {
            if (a) {
                while (b)
                    [a, b] = [b, a % b];
            }
            else {
                a = b;
            }
        }
        return a;
    },
    extendedGcd(a, b) {
        let s0 = 1, s = 0, t0 = 0, t = 1;
        while (b) {
            const q = Math.floor(a / b);
            [a, b, s0, t0, s, t] = [b, a - b * q, s, t, s0 - s * q, t0 - t * q];
        }
        // a = gcd, and s0 * a + t0 * b = r0
        return { g: a, x: s0, y: t0 };
    },
    lcm(...x) {
        return x.reduce((a, b) => (a / exports.real.gcd(a, b)) * b, 1);
    },
    denominator(x, maxDen, eps = Number.EPSILON) {
        x = Math.abs(x) % 1;
        if (x <= eps)
            return 1;
        let k1 = 1, k2 = 0;
        do {
            x = 1 / x;
            const f = Math.floor(x);
            x -= f;
            [k2, k1] = [k1, f * k1 + k2];
        } while (x > eps && k1 < maxDen);
        return k1;
    },
    commonDenominator(numbers, maxDen = 1000, eps = Number.EPSILON) {
        let scale = 1;
        for (const n of numbers) {
            scale *= exports.real.denominator(n * scale, maxDen, eps);
            if (scale > maxDen)
                return 0;
        }
        return scale;
    },
    rationalApprox(x, maxDen, eps = Number.EPSILON) {
        // binary check for exact representation
        const bits = 1 << 20;
        const m = x * bits;
        if (Number.isInteger(m)) {
            const d = bits / (m & -m);
            if (d < maxDen)
                return [x * d, d];
        }
        let h1 = Math.floor(x), h2 = 1;
        let k1 = 1, k2 = 0;
        let b = x - h1;
        while (b > eps && k1 < maxDen) {
            b = 1 / b;
            const f = Math.floor(b);
            [h2, h1, k2, k1] = [h1, f * h1 + h2, k1, f * k1 + k2];
            b -= f;
        }
        return [h1, k1];
    },
    continuedFraction(x, maxTerms = 64, eps) {
        const out = [];
        for (let i = 0; i < maxTerms; i++) {
            const a = Math.floor(x);
            out.push(a);
            x -= a;
            if (x === 0 || (eps !== undefined && Math.abs(x) < eps))
                break;
            x = 1 / x;
        }
        return out;
    },
    extent: _extent
});
exports.real.prototype = _real.prototype;
exports.default = exports.real;
function asScalar(x) {
    if (typeof x === 'bigint' || typeof x === 'number')
        return (0, exports.real)(Number(x));
    if ((0, core_1.isScalar)(x))
        return x;
    if ((0, core_1.hasop)('mag')(x))
        return asScalarExt(x.mag());
    throw new Error(`Not a scalar: ${x}`);
    //	if (typeof x === 'bigint')
    //		x = Number(x);
    //	return typeof x === 'number' ? real(x) : x;
}
function asScalarExt(x) {
    if (typeof x === 'bigint' || typeof x === 'number')
        return (0, exports.real)(Number(x));
    if ((0, core_1.isScalarExt)(x))
        return x;
    if ((0, core_1.hasop)('mag')(x))
        return asScalarExt(x.mag());
    throw new Error(`Not a scalar: ${x}`);
}
//# sourceMappingURL=real.js.map