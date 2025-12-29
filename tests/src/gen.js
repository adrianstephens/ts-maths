"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Mod = exports.gen = void 0;
exports.ModFactory = ModFactory;
//-----------------------------------------------------------------------------
// Generics
//-----------------------------------------------------------------------------
class extentT {
    min;
    max;
    static fromCentreExtent(centre, size) {
        const half = size.scale(0.5);
        return new extentT(centre.sub(half), centre.add(half));
    }
    static from(items) {
        let ext; // = new extentT<T>;
        for (const i of items) {
            if (!ext)
                ext = new extentT(i, i);
            else
                ext.add(i);
        }
        return ext;
    }
    constructor(min, max) {
        this.min = min;
        this.max = max;
    }
    extent() {
        return this.max.sub(this.min);
    }
    centre() {
        return this.min.add(this.max).scale(0.5);
    }
    add(p) {
        this.min = exports.gen.min(this.min, p);
        this.max = exports.gen.max(this.max, p);
    }
    combine(b) {
        this.min = exports.gen.min(this.min, b.min);
        this.max = exports.gen.max(this.max, b.max);
    }
    encompasses(b) {
        return !b.min.lt(this.min) && !this.max.lt(b.max);
    }
    overlaps(b) {
        return !b.max.lt(this.min) && !this.max.lt(b.min);
    }
    contains(p) {
        return !p.lt(this.min) && !this.max.lt(p);
    }
    clamp(p) {
        return exports.gen.min(exports.gen.max(p, this.min), this.max);
    }
}
exports.gen = {
    dup(a) { return typeof a === 'object' ? a.dup() : a; },
    neg(a) { return typeof a === 'object' ? a.neg() : -a; },
    add(a, b) { return typeof a === 'object' ? a.add(b) : a + b; },
    sub(a, b) { return typeof a === 'object' ? a.sub(b) : a - b; },
    mul(a, b) { return typeof a === 'object' ? a.mul(b) : a * b; },
    div(a, b) { return typeof a === 'object' ? a.div(b) : a / b; },
    ipow(base, exp, one) {
        let result = exp & 1 ? base : one;
        for (exp >>= 1; exp; exp >>= 1) {
            base = base.mul(base);
            if (exp & 1)
                result = result ? result.mul(base) : base;
        }
        return result;
    },
    eq(a, b) { return a.eq(b); },
    lt(a, b) { return a.lt(b); },
    copySign(a, b) {
        return b.sign() < 0 ? a.abs().neg() : a.abs();
    },
    max(...values) {
        return values.reduce((a, b) => a.lt(b) ? b : a);
    },
    min(...values) {
        return values.reduce((a, b) => a.lt(b) ? a : b);
    },
    compare(a, b) {
        return a.lt(b) ? -1 : b.lt(a) ? 1 : 0;
    },
    gcd(...values) {
        let a;
        for (let b of values) {
            b = b.dup().abs();
            if (a && a.sign() !== 0) {
                while (b.sign() !== 0) {
                    a.divmod(b);
                    [a, b] = [b, a];
                }
            }
            else {
                a = b;
            }
        }
        return a;
    },
    extendedGcd(a, b, one) {
        let s0 = one, s = one.scale(0), t0 = s, t = one;
        while (b.sign() !== 0) {
            const q = a.divmod(b);
            [a, b, s0, t0, s, t] = [b, a, s, t, s0.sub(s.mul(q)), t0.sub(t.mul(q))];
        }
        // x * a + y * b = g
        return { g: a, x: s0, y: t0 };
    },
    lcm(...values) {
        let a;
        for (const b of values)
            a = a ? b.scale(Number(a.divmod(exports.gen.gcd(a, b)))) : b;
        return a;
    },
    denominator(x, maxDen, eps) {
        const one = x.from(1);
        x.divmod(one);
        if (eps && x.lt(eps))
            return 1n;
        let k1 = 1n, k2 = 0n;
        do {
            x = x.recip();
            [k2, k1] = [k1, BigInt(x.divmod(one)) * k1 + k2];
        } while ((!eps || eps.lt(x)) && k1 < maxDen);
        return k1;
    },
    commonDenominator(numbers, maxDen = 1000n, eps) {
        let scale = 1n;
        for (const n of numbers) {
            scale *= exports.gen.denominator(n.scale(Number(scale)), maxDen, eps);
            if (scale > maxDen)
                return 0;
        }
        return scale;
    },
    rationalApprox(x, maxDen, eps) {
        //		const den = this.denominator(x.dup(), maxDen, eps);
        //		return [BigInt(x.dup().divmod(x.from(den))), den];
        const one = x.from(1);
        let b = x.dup();
        let h1 = BigInt(b.divmod(one)), h2 = 1n;
        let k1 = 1n, k2 = 0n;
        if (b.sign() < 0) {
            --h1;
            b = b.add(one);
        }
        while (b.sign() !== 0 && k1 < maxDen && (!eps || eps.lt(b.abs()))) {
            b = b.recip();
            const f = BigInt(b.divmod(one));
            [h2, h1, k2, k1] = [h1, h1 * f + h2, k1, k1 * f + k2];
        }
        return [h1, k1];
    },
    continuedFractionT(x, maxTerms = 64, eps) {
        const out = [];
        const one = x.from(1);
        x = x.dup();
        let a = x.divmod(one);
        if (x.sign() < 0) {
            --a;
            x = x.add(one);
        }
        out.push(a);
        for (let i = 1; i < maxTerms && (x.sign() !== 0 && (eps === undefined || eps.lt(x.abs()))); i++) {
            x = x.recip();
            out.push(x.divmod(one));
        }
        return out;
    },
    modPow(base, exp, mod) {
        let result;
        while (exp) {
            base.divmod(mod);
            if (exp & 1) {
                result = result ? result.mul(base) : base;
                result.divmod(mod);
            }
            base = base.mul(base);
            exp >>= 1;
        }
        return result;
    },
    OperatorsBase(_con) {
        const con = _con;
        const proto = con.prototype;
        const r = {
            func: (name, args) => {
                const staticFn = con[name];
                if (typeof staticFn === 'function')
                    return staticFn.apply(con, args);
                const protoFn = proto[name];
                if (typeof protoFn === 'function')
                    return protoFn.apply(args[0], args.slice(1));
                return undefined;
            }
        };
        // guaranteed by the ops constraint
        r.dup = (a) => a.dup();
        //		r.neg = (a: T) => a.neg();
        r.scale = (a, b) => a.scale(b);
        r.add = (a, b) => a.add(b);
        r.sub = (a, b) => a.sub(b);
        r.mul = (a, b) => a.mul(b);
        r.div = (a, b) => a.div(b);
        // static or prototype 'from'
        if (typeof con.from === 'function')
            r.from = (n) => con.from(n);
        else if (typeof proto.from === 'function')
            r.from = (n) => proto.from(n);
        // include prototype-backed binary ops only when present
        if ('eq' in proto)
            r.eq = (a, b) => a.eq(b);
        if ('lt' in proto)
            r.lt = (a, b) => a.lt(b);
        if ('pow' in proto)
            r.pow = (a, b) => a.pow(b);
        if ('rpow' in proto)
            r.rpow = (a, n, d) => a.rpow(n, d);
        r.ipow = 'ipow' in proto
            ? (a, b) => a.ipow(b)
            : (a, b) => exports.gen.ipow(a, b);
        return r;
    },
    extent: extentT,
};
class Mod {
    v;
    constructor(v) {
        this.v = v;
    }
}
exports.Mod = Mod;
// Generic polynomial-modulo-(r) wrapper factory for base=Polynomial<number>
function ModFactory(r) {
    class M extends Mod {
        static wrap(p) {
            p = p.dup();
            p.divmod(r);
            return new this(p);
        }
        static _create(p) {
            return new this(p);
        }
        _create(p) {
            return new this.constructor(p);
        }
        wrap(p) {
            p = p.dup();
            p.divmod(r);
            return new this.constructor(p);
        }
        dup() { return this.wrap(this.v.dup()); }
        neg() { return this.wrap(this.v.neg()); }
        scale(n) { return this.wrap(this.v.scale(n)); }
        add(b) { return this.wrap(this.v.add(b.v)); }
        sub(b) { return this.wrap(this.v.sub(b.v)); }
        mul(b) { return this.wrap(this.v.mul(b.v)); }
        div(b) { return this.mul(b.recip()); }
        recip() {
            const { g, x } = exports.gen.extendedGcd(this.v.dup(), r.dup(), r.from(1)); // x * this + y * r = g
            return this.wrap(x.div(g));
            //throw new Error('Mod.recip: inverse does not exist');
        }
        from(n) { return this.wrap(this.v.from(n)); }
        ipow(n) {
            if (n >= 0)
                return this.wrap(exports.gen.modPow(this.v, n, r));
            return this.recip().ipow(-n);
        }
        // comparisons / sign
        sign() { return this.v.sign(); }
        eq(b) { return this.v.eq(b.v); }
        toString() { return this.v.toString(); }
    }
    return M;
}
exports.default = exports.gen;
//# sourceMappingURL=gen.js.map