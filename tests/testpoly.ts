import { expect, test } from './test';
import {polynomial, polynomialT, aberth, polynomialN, legendreTable, aberthComplex, aberthT} from '../src/polynomial';
import complex from '../src/complex';
import * as big from '../../big/dist/index';

const table5 = legendreTable(5);
console.log('Legendre P5 coefficients:', table5);

const bpoly = new polynomialT<big.float>([big.float.from(-1), big.float.from(0), big.float.from(1)]); // x^2 - 1
const bpoly2 = bpoly.mul(bpoly);
const broots = aberthT(n => big.float.from(n), bpoly2);
console.log('bpoly:', bpoly.toString());
console.log('bpoly2:', bpoly2.toString());
console.log('bpoly2 roots:', broots);

const cpoly = new polynomialT<complex>([complex(1, 1), complex(0, 0), complex(-1, -1)]); // x^2 - 1
console.log('cpoly:', cpoly.toString());
const cpoly2 = cpoly.mul(cpoly);
const roots = aberthComplex(cpoly2);

const c = complex(1,2);
const c2 = complex.ln(c);

//const poly2 = new polynomial([-4, 0, 1]);
const poly2 = new polynomial([-1, 1]).mul(new polynomial([-2, 1])).mul(new polynomial([-3, 1])).mul(new polynomial([-4, 1])).mul(new polynomial([-5, 1]));
const poly3 = new polynomialN([-1]).mul(new polynomialN([-2])).mul(new polynomialN([-3])).mul(new polynomialN([-4])).mul(new polynomialN([-5])).mul(new polynomialN([-6]));
const r = poly2.roots();
console.log(r);

const r1 = poly3.roots();
console.log(r1);

const r2 = aberth(poly2.normalise());
console.log(r2.join('\n'));

const r3 = aberth(poly3);
console.log(r3.join('\n'));
